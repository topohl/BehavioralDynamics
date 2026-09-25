#Requires -Version 7.2
param(
  [Parameter(Mandatory = $true)]
  [ValidateSet('Inspect', 'Prepare', 'Verify', 'Activate', 'Rollback', 'Abandon')]
  [string] $Action,
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [Parameter(Mandatory = $true)]
  [ValidateSet('03_derived_metrics', '06_behavioral_dynamics',
               '12_systems_neuroscience_summary')]
  [string] $RootName,
  [Parameter(Mandatory = $true)] [string] $Manifest,
  [Parameter(Mandatory = $true)] [string] $ManifestSha256,
  [string] $ReaderQueue = '',
  [string] $ReviewedReaderGate = '',
  [string] $ReviewedReaderGateSha256 = '',
  [ValidateSet('ScientificReplay', 'ArchivePath')]
  [string] $ReaderGateKind = 'ScientificReplay',
  # Rollback only: move an archived root back even though its inventory no
  # longer matches the manifest. The receipt stays transferring for review.
  [switch] $AcceptInventoryDrift,
  [string] $AbandonReason = ''
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Relative arguments resolve against the PowerShell location, not the
# process working directory.
function FullPath([string] $Path) {
  [System.IO.Path]::GetFullPath(
    $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path)
  ).TrimEnd('\', '/')
}
function Sha256([string] $Path) {
  (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}
function Utc { [DateTime]::UtcNow.ToString('o') }
# File.Move with overwrite is one replace-rename: on failure the previous
# receipt is kept. Move-Item -Force deletes the destination first.
function Write-Receipt([string] $Path, $Record) {
  $parent = Split-Path -Parent $Path
  New-Item -ItemType Directory -Path $parent -Force | Out-Null
  $temporary = Join-Path $parent ('.numbered-root-' + [guid]::NewGuid().ToString('N') + '.tmp')
  [System.IO.File]::WriteAllText(
    $temporary, ($Record | ConvertTo-Json -Depth 5),
    [System.Text.UTF8Encoding]::new($false))
  try {
    [System.IO.File]::Move($temporary, $Path, $true)
  } catch {
    Remove-Item -LiteralPath $temporary -Force -ErrorAction SilentlyContinue
    throw
  }
}
# Directory.Move is a same-volume rename: it moves the whole root or fails
# without copying, for example when the destination exists or, on local NTFS,
# when a file below the root is open. Move-Item can fall back to moving files
# one at a time and leave the root split across both locations.
function Move-RootDirectory([string] $From, [string] $To) {
  [System.IO.Directory]::Move($From, $To)
}
function Read-Receipt([string] $Path) {
  if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $null }
  $record = try { Get-Content -LiteralPath $Path -Raw | ConvertFrom-Json } catch { $null }
  $text = @('root', 'source_root_rel', 'archive_root_rel', 'state',
            'manifest_sha256', 'reader_gate_kind', 'reader_gate_sha256',
            'reader_queue_sha256')
  $count = @('files', 'bytes')
  if ($null -eq $record -or $record -isnot [pscustomobject] -or
      @($text + $count | Where-Object {
        -not ($record.PSObject.Properties.Name -contains $_) }).Count -gt 0 -or
      @($text | Where-Object { $record.$_ -isnot [string] }).Count -gt 0 -or
      @($count | Where-Object {
        $record.$_ -isnot [long] -and $record.$_ -isnot [int] }).Count -gt 0 -or
      $record.root -cne $RootName -or
      $record.source_root_rel -cne $RootName -or
      $record.archive_root_rel -cne "history/original_layout/$RootName" -or
      $record.state -cnotin @('prepared', 'transferring', 'activated') -or
      $record.manifest_sha256 -cnotmatch '^[0-9a-f]{64}$' -or
      $record.reader_gate_kind -cnotin @('ScientificReplay', 'ArchivePath') -or
      $record.reader_gate_sha256 -cnotmatch '^[0-9a-f]{64}$' -or
      $record.reader_queue_sha256 -cnotmatch '^[0-9a-f]{64}$' -or
      $record.files -lt 1 -or $record.bytes -lt 0) {
    throw "Invalid numbered-root archive receipt: $Path"
  }
  if ($record.manifest_sha256 -cne $ManifestSha256.ToLowerInvariant()) {
    throw ("Archive receipt pins manifest $($record.manifest_sha256), not " +
           "$($ManifestSha256.ToLowerInvariant()): $Path")
  }
  return $record
}
# Hash and parse the same bytes, so the pinned hash describes what was read.
function Read-PinnedCsv([string] $Path) {
  $bytes = [System.IO.File]::ReadAllBytes($Path)
  $hash = [Convert]::ToHexString(
    [System.Security.Cryptography.SHA256]::HashData($bytes)).ToLowerInvariant()
  $body = [System.Text.Encoding]::UTF8.GetString($bytes).TrimStart([char]0xFEFF)
  [pscustomobject]@{ sha256 = $hash; rows = @($body | ConvertFrom-Csv) }
}
function Verify-ReaderGate {
  if ([string]::IsNullOrWhiteSpace($ReviewedReaderGate) -or
      $ReviewedReaderGateSha256 -notmatch '^[0-9a-fA-F]{64}$') {
    throw 'A reviewed reader gate, its SHA-256, and the original queue are required'
  }
  # A live gate must cover the committed audit queue; only a temporary
  # fixture may supply its own queue.
  if (-not $fixtureRoot -and $queuePath -ine $canonicalQueue) {
    throw "A live archive gate must use the repository audit queue: $canonicalQueue"
  }
  $gatePath = FullPath $ReviewedReaderGate
  if (-not (Test-Path -LiteralPath $queuePath -PathType Leaf) -or
      -not (Test-Path -LiteralPath $gatePath -PathType Leaf)) {
    throw 'Reader gate is missing or changed since review'
  }
  $queueFile = Read-PinnedCsv $queuePath
  $gateFile = Read-PinnedCsv $gatePath
  if ($gateFile.sha256 -cne $ReviewedReaderGateSha256.ToLowerInvariant()) {
    throw 'Reader gate is missing or changed since review'
  }
  $queue = $queueFile.rows
  $gate = $gateFile.rows
  if ($queue.Count -eq 0 -or $gate.Count -ne $queue.Count -or
      -not ($queue[0].PSObject.Properties.Name -contains 'script') -or
      -not ($gate[0].PSObject.Properties.Name -contains 'script') -or
      -not ($gate[0].PSObject.Properties.Name -contains 'review_state') -or
      -not ($gate[0].PSObject.Properties.Name -contains 'script_sha256') -or
      @($queue | Select-Object -ExpandProperty script -Unique).Count -ne $queue.Count -or
      @($gate | Select-Object -ExpandProperty script -Unique).Count -ne $gate.Count) {
    throw 'Reader gate does not have a unique row for every queued script'
  }
  $queuedScripts = @($queue | ForEach-Object script | Sort-Object)
  $gateScripts = @($gate | ForEach-Object script | Sort-Object)
  $requiredState = if ($ReaderGateKind -ceq 'ArchivePath') {
    'archive_path_ready'
  } else {
    'ready'
  }
  if ((Compare-Object $queuedScripts $gateScripts) -or
      @($gate | Where-Object { $_.review_state -cne $requiredState }).Count -gt 0) {
    throw 'Reader gate does not resolve every queued script'
  }
  if ($ReaderGateKind -ceq 'ArchivePath') {
    if (-not ($gate[0].PSObject.Properties.Name -contains 'path_review_evidence') -or
        -not ($gate[0].PSObject.Properties.Name -contains 'writer_review_evidence') -or
        @($gate | Where-Object {
          [string]::IsNullOrWhiteSpace($_.path_review_evidence) -or
          [string]::IsNullOrWhiteSpace($_.writer_review_evidence)
        }).Count -gt 0) {
      throw 'Archive path gate requires path and writer review evidence for every script'
    }
  }
  foreach ($row in $gate) {
    if ($row.script -cnotmatch '^Testing/audits/[A-Za-z0-9_.-]+\.R$' -or
        $row.script_sha256 -cnotmatch '^[0-9a-fA-F]{64}$') {
      throw "Reader gate has an invalid script path or hash: $($row.script)"
    }
    $scriptPath = FullPath (Join-Path $repoRoot ($row.script -replace '/', '\'))
    if (-not (Test-Path -LiteralPath $scriptPath -PathType Leaf) -or
        (Sha256 $scriptPath) -cne $row.script_sha256.ToLowerInvariant()) {
      throw "Reviewed audit script changed or is missing: $($row.script)"
    }
  }
  [pscustomobject]@{ gate_sha256 = $gateFile.sha256; queue_sha256 = $queueFile.sha256 }
}

$repoRoot = FullPath (Join-Path $PSScriptRoot '..')
$canonicalQueue = FullPath (Join-Path $repoRoot 'docs\behavior_output_archive_audit_script_queue.csv')
$queuePath = if ([string]::IsNullOrWhiteSpace($ReaderQueue)) { $canonicalQueue } else { FullPath $ReaderQueue }
$ready = FullPath $AnalysisReadyRoot
$fixtureRoot = $ready.StartsWith((FullPath ([System.IO.Path]::GetTempPath())) + '\',
                                 [System.StringComparison]::OrdinalIgnoreCase)
$source = FullPath (Join-Path $ready $RootName)
$archiveParent = FullPath (Join-Path $ready 'history\original_layout')
$archive = FullPath (Join-Path $archiveParent $RootName)
$manifestPath = FullPath $Manifest
$control = FullPath (Join-Path $ready '_migration_control\numbered_root_archive')
$receipt = Join-Path $control "$RootName.json"
$manifestTool = Join-Path $PSScriptRoot 'Invoke-BehaviorNumberedRootArchiveManifest.ps1'
if (-not (Test-Path -LiteralPath $ready -PathType Container) -or
    (Split-Path -Leaf $ready) -cne 'analysis_ready' -or
    -not (Test-Path -LiteralPath $manifestPath -PathType Leaf) -or
    $ManifestSha256 -notmatch '^[0-9a-fA-F]{64}$' -or
    (Sha256 $manifestPath) -cne $ManifestSha256.ToLowerInvariant()) {
  throw 'Missing analysis_ready root or changed archive manifest'
}
if ($manifestPath.StartsWith($ready + '\', [System.StringComparison]::OrdinalIgnoreCase)) {
  throw 'The archive manifest must be stored outside analysis_ready'
}

function Verify-Location([string] $Location) {
  $checked = & $manifestTool -Action Verify -AnalysisReadyRoot $ready -RootName $RootName `
    -Manifest $manifestPath -Location $Location
  if ($checked.manifest_sha256 -cne $ManifestSha256.ToLowerInvariant()) {
    throw 'Archive manifest changed during verification'
  }
  $checked
}
function Assert-NoReparsePaths {
  foreach ($path in @($ready, (Join-Path $ready 'history'), $archiveParent,
                     $source, $archive)) {
    if (Test-Path -LiteralPath $path) {
      if (((Get-Item -LiteralPath $path -Force).Attributes -band
            [System.IO.FileAttributes]::ReparsePoint) -ne 0) {
        throw "Archive transition path is a reparse point: $path"
      }
    }
  }
}
function Assert-Inventory($Checked, $Record, [string] $Message) {
  if ($Checked.files -ne [long]$Record.files -or
      $Checked.bytes -ne [long]$Record.bytes) { throw $Message }
}

# Changing actions hold an exclusive lock for their whole run. The operating
# system deletes it when the handle closes; a lock left by a lost session must
# be reviewed and removed by hand.
$lock = $null
if ($Action -cne 'Inspect' -and $Action -cne 'Verify') {
  New-Item -ItemType Directory -Path $control -Force | Out-Null
  $lockPath = Join-Path $control "$RootName.lock"
  try {
    $lock = [System.IO.FileStream]::new(
      $lockPath, [System.IO.FileMode]::CreateNew, [System.IO.FileAccess]::ReadWrite,
      [System.IO.FileShare]::None, 1, [System.IO.FileOptions]::DeleteOnClose)
  } catch {
    throw "Another archive action holds $lockPath"
  }
}
try {
  $record = Read-Receipt $receipt
  switch ($Action) {
    'Inspect' {
      if ($null -eq $record) {
        if (Test-Path -LiteralPath $archive) { throw 'Unreceipted archive destination exists' }
        $checked = Verify-Location 'Original'
        [pscustomobject]@{ action = 'Inspect'; state = 'unprepared'; root = $RootName;
          files = $checked.files; hashes = 'PASS' }
      } elseif ($record.state -ceq 'prepared') {
        if (Test-Path -LiteralPath $archive) { throw 'Prepared archive destination exists' }
        $checked = Verify-Location 'Original'
        Assert-Inventory $checked $record 'Prepared receipt inventory differs'
        [pscustomobject]@{ action = 'Inspect'; state = 'prepared'; root = $RootName;
          files = $checked.files; hashes = 'PASS' }
      } elseif ($record.state -ceq 'activated') {
        $checked = Verify-Location 'Archived'
        Assert-Inventory $checked $record 'Activated receipt inventory differs'
        [pscustomobject]@{ action = 'Inspect'; state = 'activated'; root = $RootName;
          files = $checked.files; hashes = 'PASS' }
      } else { throw "Archive needs recovery: $($record.state)" }
    }
    'Prepare' {
      if ($null -ne $record -or (Test-Path -LiteralPath $archive)) {
        throw 'Archive receipt or destination already exists'
      }
      $gateHashes = Verify-ReaderGate
      $checked = Verify-Location 'Original'
      $record = [pscustomobject]@{
        root = $RootName; source_root_rel = $RootName
        archive_root_rel = "history/original_layout/$RootName"
        state = 'prepared'; files = $checked.files; bytes = $checked.bytes
        manifest_sha256 = $ManifestSha256.ToLowerInvariant()
        reader_gate_sha256 = $gateHashes.gate_sha256
        reader_gate_kind = $ReaderGateKind
        reader_queue_sha256 = $gateHashes.queue_sha256
        prepared_at_utc = Utc
      }
      Write-Receipt $receipt $record
      [pscustomobject]@{ action = 'Prepare'; state = 'prepared'; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    }
    'Verify' {
      if ($null -eq $record) { throw 'Missing archive receipt' }
      if ($record.state -ceq 'prepared') { $checked = Verify-Location 'Original' }
      elseif ($record.state -ceq 'activated') { $checked = Verify-Location 'Archived' }
      else { throw "Archive needs recovery: $($record.state)" }
      Assert-Inventory $checked $record 'Archive receipt inventory differs'
      [pscustomobject]@{ action = 'Verify'; state = $record.state; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    }
    'Activate' {
      if ($null -eq $record -or $record.state -cne 'prepared') {
        throw 'Activate requires a prepared archive receipt'
      }
      if ($ReaderGateKind -cne $record.reader_gate_kind) {
        throw 'Reader gate kind changed after Prepare'
      }
      $gateHashes = Verify-ReaderGate
      if ($gateHashes.gate_sha256 -cne $record.reader_gate_sha256) {
        throw 'Reader gate changed after Prepare'
      }
      if ($gateHashes.queue_sha256 -cne $record.reader_queue_sha256) {
        throw 'Reader queue changed after Prepare'
      }
      if (Test-Path -LiteralPath $archive) { throw 'Archive destination already exists' }
      $checked = Verify-Location 'Original'
      Assert-Inventory $checked $record 'Source changed after Prepare'
      Assert-NoReparsePaths
      New-Item -ItemType Directory -Path $archiveParent -Force | Out-Null
      Assert-NoReparsePaths
      $record.state = 'transferring'
      Write-Receipt $receipt $record
      Move-RootDirectory $source $archive
      $checked = Verify-Location 'Archived'
      Assert-Inventory $checked $record 'Archived inventory differs from the prepared receipt'
      $record.state = 'activated'
      $record | Add-Member -NotePropertyName activated_at_utc -NotePropertyValue (Utc) -Force
      Write-Receipt $receipt $record
      [pscustomobject]@{ action = 'Activate'; state = 'activated'; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    }
    'Rollback' {
      if ($null -eq $record -or
          $record.state -cnotin @('transferring', 'activated')) {
        throw 'Rollback requires a transferring or activated archive receipt'
      }
      Assert-NoReparsePaths
      $atSource = Test-Path -LiteralPath $source
      $atArchive = Test-Path -LiteralPath $archive
      if ($AcceptInventoryDrift) {
        # Recovery after a failed post-move verification: rename the archive
        # back without trusting its contents. Nothing is deleted or rewritten.
        if ($atSource -or -not $atArchive) {
          throw 'Drift recovery requires the archive and no original root'
        }
        $record.state = 'transferring'
        $record | Add-Member -NotePropertyName drift_detected_at_utc -NotePropertyValue (Utc) -Force
        Write-Receipt $receipt $record
        Move-RootDirectory $archive $source
        return [pscustomobject]@{ action = 'Rollback'; state = 'transferring'; root = $RootName;
          files = $null; hashes = 'NOT_VERIFIED' }
      }
      if ($atSource -and -not $atArchive) {
        $checked = Verify-Location 'Original'
      } elseif (-not $atSource -and $atArchive) {
        $checked = Verify-Location 'Archived'
        $record.state = 'transferring'
        Write-Receipt $receipt $record
        Move-RootDirectory $archive $source
        $checked = Verify-Location 'Original'
      } else {
        throw 'Rollback found both or neither numbered source locations'
      }
      Assert-Inventory $checked $record 'Rollback inventory differs'
      $record.state = 'prepared'
      Write-Receipt $receipt $record
      [pscustomobject]@{ action = 'Rollback'; state = 'prepared'; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    }
    'Abandon' {
      # Leave archive control without moving anything. The receipt is kept
      # under abandoned/ for review; readers and writers then use the original.
      if ($null -eq $record -or
          -not ($record.state -ceq 'prepared' -or
                ($record.state -ceq 'transferring' -and
                 $record.PSObject.Properties.Name -contains 'drift_detected_at_utc'))) {
        throw 'Abandon requires a prepared receipt or a drift-recovered transferring receipt'
      }
      if ([string]::IsNullOrWhiteSpace($AbandonReason)) {
        throw 'Abandon requires -AbandonReason'
      }
      Assert-NoReparsePaths
      if (-not (Test-Path -LiteralPath $source -PathType Container) -or
          (Test-Path -LiteralPath $archive)) {
        throw 'Abandon requires the original root and no archive destination'
      }
      $stamp = Utc
      $record | Add-Member -NotePropertyName abandoned_at_utc -NotePropertyValue $stamp -Force
      $record | Add-Member -NotePropertyName abandon_reason -NotePropertyValue $AbandonReason -Force
      $abandoned = Join-Path $control ('abandoned\' + $RootName + '-' +
                                       ($stamp -replace '[^0-9A-Za-z]', '') + '.json')
      if (Test-Path -LiteralPath $abandoned) { throw "Abandoned receipt exists: $abandoned" }
      Write-Receipt $abandoned $record
      Remove-Item -LiteralPath $receipt
      [pscustomobject]@{ action = 'Abandon'; state = 'unprepared'; root = $RootName;
        files = $null; hashes = 'NOT_VERIFIED' }
    }
  }
} finally {
  if ($null -ne $lock) { $lock.Dispose() }
}
