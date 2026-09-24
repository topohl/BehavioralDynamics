param(
  [Parameter(Mandatory = $true)]
  [ValidateSet('Inspect', 'Prepare', 'Verify', 'Activate', 'Rollback')]
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
  [string] $ReaderGateKind = 'ScientificReplay'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function FullPath([string] $Path) {
  [System.IO.Path]::GetFullPath($Path).TrimEnd('\', '/')
}
function Sha256([string] $Path) {
  (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}
function Write-Receipt([string] $Path, $Record) {
  $parent = Split-Path -Parent $Path
  New-Item -ItemType Directory -Path $parent -Force | Out-Null
  $temporary = Join-Path $parent ('.numbered-root-' + [guid]::NewGuid().ToString('N') + '.tmp')
  [System.IO.File]::WriteAllText(
    $temporary, ($Record | ConvertTo-Json -Depth 5),
    [System.Text.UTF8Encoding]::new($false))
  Move-Item -LiteralPath $temporary -Destination $Path -Force -ErrorAction Stop
}
function Read-Receipt([string] $Path) {
  if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $null }
  $record = Get-Content -LiteralPath $Path -Raw | ConvertFrom-Json
  if ($record.root -cne $RootName -or
      $record.source_root_rel -cne $RootName -or
      $record.archive_root_rel -cne "history/original_layout/$RootName" -or
      $record.manifest_sha256 -cne $ManifestSha256.ToLowerInvariant() -or
      $record.reader_gate_kind -cnotin @('ScientificReplay', 'ArchivePath') -or
      $record.reader_gate_sha256 -notmatch '^[0-9a-f]{64}$' -or
      $record.reader_queue_sha256 -notmatch '^[0-9a-f]{64}$' -or
      $record.files -notmatch '^[1-9][0-9]*$' -or
      $record.bytes -notmatch '^[0-9]+$') {
    throw "Invalid numbered-root archive receipt: $Path"
  }
  return $record
}
function Verify-ReaderGate {
  if ([string]::IsNullOrWhiteSpace($ReaderQueue) -or
      [string]::IsNullOrWhiteSpace($ReviewedReaderGate) -or
      $ReviewedReaderGateSha256 -notmatch '^[0-9a-fA-F]{64}$') {
    throw 'A reviewed reader gate, its SHA-256, and the original queue are required'
  }
  $queuePath = FullPath $ReaderQueue
  $gatePath = FullPath $ReviewedReaderGate
  if (-not (Test-Path -LiteralPath $queuePath -PathType Leaf) -or
      -not (Test-Path -LiteralPath $gatePath -PathType Leaf) -or
      (Sha256 $gatePath) -cne $ReviewedReaderGateSha256.ToLowerInvariant()) {
    throw 'Reader gate is missing or changed since review'
  }
  $queue = @(Import-Csv -LiteralPath $queuePath)
  $gate = @(Import-Csv -LiteralPath $gatePath)
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
  $repoRoot = FullPath (Join-Path $PSScriptRoot '..')
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
  return $ReviewedReaderGateSha256.ToLowerInvariant()
}

$ready = FullPath $AnalysisReadyRoot
$source = FullPath (Join-Path $ready $RootName)
$archiveParent = FullPath (Join-Path $ready 'history\original_layout')
$archive = FullPath (Join-Path $archiveParent $RootName)
$manifestPath = FullPath $Manifest
$receipt = FullPath (Join-Path $ready ("_migration_control\numbered_root_archive\$RootName.json"))
$manifestTool = Join-Path $PSScriptRoot 'Invoke-BehaviorNumberedRootArchiveManifest.ps1'
if (-not (Test-Path -LiteralPath $ready -PathType Container) -or
    -not (Test-Path -LiteralPath $manifestPath -PathType Leaf) -or
    $ManifestSha256 -notmatch '^[0-9a-fA-F]{64}$' -or
    (Sha256 $manifestPath) -cne $ManifestSha256.ToLowerInvariant()) {
  throw 'Missing analysis_ready root or changed archive manifest'
}
if ($archiveParent -cne (FullPath (Join-Path $ready 'history\original_layout')) -or
    $source -cne (FullPath (Join-Path $ready $RootName)) -or
    $archive -cne (FullPath (Join-Path $archiveParent $RootName))) {
  throw 'Archive paths did not resolve to the expected analysis_ready locations'
}
$record = Read-Receipt $receipt

function Verify-Location([string] $Location) {
  & $manifestTool -Action Verify -AnalysisReadyRoot $ready -RootName $RootName `
    -Manifest $manifestPath -Location $Location
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
      if ($checked.files -ne [int]$record.files -or
          $checked.bytes -ne [long]$record.bytes) { throw 'Prepared receipt inventory differs' }
      [pscustomobject]@{ action = 'Inspect'; state = 'prepared'; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    } elseif ($record.state -ceq 'activated') {
      $checked = Verify-Location 'Archived'
      if ($checked.files -ne [int]$record.files -or
          $checked.bytes -ne [long]$record.bytes) { throw 'Activated receipt inventory differs' }
      [pscustomobject]@{ action = 'Inspect'; state = 'activated'; root = $RootName;
        files = $checked.files; hashes = 'PASS' }
    } else { throw "Archive needs recovery: $($record.state)" }
  }
  'Prepare' {
    if ($null -ne $record -or (Test-Path -LiteralPath $archive)) {
      throw 'Archive receipt or destination already exists'
    }
    $gateHash = Verify-ReaderGate
    $checked = Verify-Location 'Original'
    $record = [pscustomobject]@{
      root = $RootName; source_root_rel = $RootName
      archive_root_rel = "history/original_layout/$RootName"
      state = 'prepared'; files = $checked.files; bytes = $checked.bytes
      manifest_sha256 = $ManifestSha256.ToLowerInvariant()
      reader_gate_sha256 = $gateHash
      reader_gate_kind = $ReaderGateKind
      reader_queue_sha256 = Sha256 (FullPath $ReaderQueue)
      prepared_at_utc = [DateTime]::UtcNow.ToString('o')
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
    if ($checked.files -ne [int]$record.files -or
        $checked.bytes -ne [long]$record.bytes) { throw 'Archive receipt inventory differs' }
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
    $gateHash = Verify-ReaderGate
    if ($gateHash -cne $record.reader_gate_sha256) {
      throw 'Reader gate changed after Prepare'
    }
    if ((Sha256 (FullPath $ReaderQueue)) -cne $record.reader_queue_sha256) {
      throw 'Reader queue changed after Prepare'
    }
    if (Test-Path -LiteralPath $archive) { throw 'Archive destination already exists' }
    $checked = Verify-Location 'Original'
    if ($checked.files -ne [int]$record.files -or
        $checked.bytes -ne [long]$record.bytes) { throw 'Source changed after Prepare' }
    New-Item -ItemType Directory -Path $archiveParent -Force | Out-Null
    Assert-NoReparsePaths
    $record.state = 'transferring'
    Write-Receipt $receipt $record
    Move-Item -LiteralPath $source -Destination $archive -ErrorAction Stop
    $checked = Verify-Location 'Archived'
    $record.state = 'activated'
    $record | Add-Member -NotePropertyName activated_at_utc `
      -NotePropertyValue ([DateTime]::UtcNow.ToString('o')) -Force
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
    if ((Test-Path -LiteralPath $source) -and
        -not (Test-Path -LiteralPath $archive)) {
      $checked = Verify-Location 'Original'
    } elseif (-not (Test-Path -LiteralPath $source) -and
              (Test-Path -LiteralPath $archive)) {
      $checked = Verify-Location 'Archived'
      $record.state = 'transferring'
      Write-Receipt $receipt $record
      Move-Item -LiteralPath $archive -Destination $source -ErrorAction Stop
      $checked = Verify-Location 'Original'
    } else {
      throw 'Rollback found both or neither numbered source locations'
    }
    if ($checked.files -ne [int]$record.files -or
        $checked.bytes -ne [long]$record.bytes) { throw 'Rollback inventory differs' }
    $record.state = 'prepared'
    Write-Receipt $receipt $record
    [pscustomobject]@{ action = 'Rollback'; state = 'prepared'; root = $RootName;
      files = $checked.files; hashes = 'PASS' }
  }
}
