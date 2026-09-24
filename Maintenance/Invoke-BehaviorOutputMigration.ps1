param(
  [Parameter(Mandatory = $true)]
  [ValidateSet('Inspect', 'Prepare', 'Verify', 'Activate')]
  [string] $Action,
  [Parameter(Mandatory = $true)] [string] $Group,
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [string] $Plan = '',
  [string] $RepositoryRoot = '',
  [string] $OwnershipManifest = '',
  [string] $OwnershipManifestSha256 = '',
  [switch] $HistoricalDestination
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
if ([string]::IsNullOrWhiteSpace($RepositoryRoot)) {
  $RepositoryRoot = Join-Path $PSScriptRoot '..'
}
if ([string]::IsNullOrWhiteSpace($Plan)) {
  $Plan = Join-Path $RepositoryRoot 'docs\BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv'
}

function FullPath([string] $Path) {
  [System.IO.Path]::GetFullPath($Path).TrimEnd('\', '/')
}

function ChildPath([string] $Root, [string] $Relative) {
  if ([System.IO.Path]::IsPathRooted($Relative) -or
      $Relative -match '(^|[\\/])\.\.([\\/]|$)' -or
      $Relative -match '(^|[\\/])\.([\\/]|$)' -or
      [string]::IsNullOrWhiteSpace($Relative)) {
    throw "Unsafe relative path: $Relative"
  }
  $rootFull = FullPath $Root
  $candidate = FullPath (Join-Path $rootFull $Relative)
  if (-not $candidate.StartsWith($rootFull + [System.IO.Path]::DirectorySeparatorChar,
                                  [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Path escapes root: $Relative"
  }
  return $candidate
}

function Sha256([string] $Path) {
  (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}

function Sha256Text([string] $Value) {
  $bytes = [System.Text.Encoding]::UTF8.GetBytes($Value)
  $algorithm = [System.Security.Cryptography.SHA256]::Create()
  try { [System.BitConverter]::ToString($algorithm.ComputeHash($bytes)).Replace('-', '').ToLowerInvariant() }
  finally { $algorithm.Dispose() }
}

function Write-Receipt([string] $Path, $Record) {
  $directory = Split-Path -Parent $Path
  New-Item -ItemType Directory -Path $directory -Force | Out-Null
  $temporary = Join-Path $directory ('.receipt-' + [guid]::NewGuid().ToString('N') + '.tmp')
  [System.IO.File]::WriteAllText(
    $temporary, ($Record | ConvertTo-Json -Depth 6),
    [System.Text.UTF8Encoding]::new($false))
  if (Test-Path -LiteralPath $Path) {
    Move-Item -LiteralPath $temporary -Destination $Path -Force -ErrorAction Stop
  } else {
    [System.IO.File]::Move($temporary, $Path)
  }
}

$root = FullPath $AnalysisReadyRoot
$repo = FullPath $RepositoryRoot
$planPath = FullPath $Plan
if ([string]::IsNullOrWhiteSpace($OwnershipManifest) -ne
    [string]::IsNullOrWhiteSpace($OwnershipManifestSha256)) {
  throw 'Ownership manifest and its reviewed SHA-256 must be supplied together'
}
$ownershipHash = ''
$ownershipRows = @()
if (-not [string]::IsNullOrWhiteSpace($OwnershipManifest)) {
  $ownershipPath = FullPath $OwnershipManifest
  if (-not (Test-Path -LiteralPath $ownershipPath -PathType Leaf) -or
      $OwnershipManifestSha256 -notmatch '^[0-9a-fA-F]{64}$') {
    throw 'Missing ownership manifest or invalid reviewed SHA-256'
  }
  $ownershipHash = $OwnershipManifestSha256.ToLowerInvariant()
  if ((Sha256 $ownershipPath) -cne $ownershipHash) {
    throw 'Ownership manifest changed since review'
  }
  $ownershipRows = @(Import-Csv -LiteralPath $ownershipPath)
  foreach ($column in @('source_rel', 'owner_group', 'state')) {
    if ($ownershipRows.Count -eq 0 -or
        -not ($ownershipRows[0].PSObject.Properties.Name -contains $column)) {
      throw "Ownership manifest is missing column: $column"
    }
  }
  if (@($ownershipRows | Select-Object -ExpandProperty source_rel -Unique).Count -ne
      $ownershipRows.Count) {
    throw 'Ownership manifest has duplicate source paths'
  }
  foreach ($owner in $ownershipRows) {
    [void](ChildPath $root $owner.source_rel)
    if ([string]::IsNullOrWhiteSpace($owner.owner_group)) {
      throw "Missing owner for $($owner.source_rel)"
    }
  }
}
if (-not (Test-Path -LiteralPath $root -PathType Container)) { throw "Missing analysis_ready root: $root" }
if (-not (Test-Path -LiteralPath $planPath -PathType Leaf)) { throw "Missing migration plan: $planPath" }
$allRows = @(Import-Csv -LiteralPath $planPath)
$requiredColumns = @('group', 'source_rel', 'target_root_rel', 'target_file',
                     'source_sha256', 'gate', 'contract_file', 'contract_sha256')
foreach ($column in $requiredColumns) {
  if ($allRows.Count -eq 0 -or -not ($allRows[0].PSObject.Properties.Name -contains $column)) {
    throw "Migration plan is missing column: $column"
  }
}
if (@($allRows | Select-Object -ExpandProperty source_rel -Unique).Count -ne $allRows.Count -or
    @($allRows | ForEach-Object { "$($_.target_root_rel)/$($_.target_file)" } |
      Select-Object -Unique).Count -ne $allRows.Count) {
  throw 'Migration plan has duplicate source or destination files across groups'
}
$rows = @($allRows | Where-Object { $_.group -ceq $Group })
if ($rows.Count -eq 0) { throw "Unknown migration group: $Group" }
if (@($rows | Select-Object -ExpandProperty gate -Unique).Count -ne 1 -or
    @($rows | Select-Object -ExpandProperty target_root_rel -Unique).Count -ne 1 -or
    @($rows | Select-Object -ExpandProperty contract_file -Unique).Count -ne 1 -or
    @($rows | Select-Object -ExpandProperty contract_sha256 -Unique).Count -ne 1) {
  throw "Group $Group has inconsistent gate, destination, or code contract"
}
if (@($rows | Select-Object -ExpandProperty target_file -Unique).Count -ne $rows.Count -or
    @($rows | Select-Object -ExpandProperty source_rel -Unique).Count -ne $rows.Count) {
  throw "Group $Group has duplicate source or destination files"
}
$gate = $rows[0].gate
$groupPlanText = (@($rows | Sort-Object source_rel | ForEach-Object {
  @($_.group, $_.source_rel, $_.target_root_rel, $_.target_file,
    $_.source_sha256, $_.gate, $_.contract_file, $_.contract_sha256) -join [char]31
}) -join "`n")
$groupPlanHash = Sha256Text $groupPlanText
$targetRoot = ChildPath $root $rows[0].target_root_rel
$stagingRoot = ChildPath $root ("_migration_incoming/$Group")
$receiptPath = ChildPath $root ("_migration_control/$Group.json")
$historicalRoots = @{
  history_social_networks_10sec = @('social_networks', '10sec_based', '10sec')
  history_social_networks_1min = @('social_networks', '1min_based', '1min')
  history_social_networks_10min = @('social_networks', '10min_based', '10min')
  history_social_networks_30min = @('social_networks', '30min_based', '30min')
  history_state_space_1min = @('state_space', '1min_based', '1min')
  history_state_space_10min = @('state_space', '10min_based', '10min')
  history_temporal_instability_1min = @('temporal_instability', '1min_based', '1min')
  history_temporal_instability_5min = @('temporal_instability', '5min_based', '5min')
  history_gamm_features_30min = @('gamm_features', '30min_based', '30min')
}
if ($HistoricalDestination -and -not $historicalRoots.ContainsKey($Group)) {
  throw "Historical destination is not an approved resolution group: $Group"
}
$mapping = @(foreach ($row in $rows) {
  if ($row.source_rel -match '(^|[\\/])(_quarantine|quarantine|_archive|history)([\\/]|$)' -or
      $row.target_root_rel -match '(^|[\\/])(_quarantine|quarantine|_archive)([\\/]|$)') {
    throw "Quarantine, archive, and history sources and quarantine/archive destinations are forbidden"
  }
  if ($HistoricalDestination) {
    $spec = $historicalRoots[$Group]
    $expectedSourceRoot = "06_behavioral_dynamics/$($spec[0])/$($spec[1])"
    $expectedTargetRoot = "history/$($spec[0])/$($spec[2])"
    if ($row.target_root_rel.Replace('\', '/') -cne $expectedTargetRoot -or
        $row.source_rel.Replace('\', '/') -cne
          ($expectedSourceRoot + '/' + $row.target_file.Replace('\', '/'))) {
      throw "Historical group $Group has an unexpected source or destination"
    }
  } elseif ($row.target_root_rel -match '(^|[\\/])history([\\/]|$)') {
    throw "History destination requires -HistoricalDestination and an approved group"
  }
  if ($row.source_sha256 -notmatch '^[0-9a-fA-F]{64}$') {
    throw "Invalid SHA-256 for $($row.source_rel)"
  }
  [pscustomobject]@{
    source = ChildPath $root $row.source_rel
    staged = ChildPath $stagingRoot $row.target_file
    target = ChildPath $targetRoot $row.target_file
    sha256 = $row.source_sha256.ToLowerInvariant()
  }
})
$dashboardMetadata = $Group -ceq 'systems_dashboard_5min'
$metadataFiles = @(
  'tables/output_figure_inventory.csv', 'tables/output_folder_summary.csv',
  'figures/publication_panels/README.txt', 'figures/qc/README.txt',
  'figures/supplementary/README.txt', 'figures/exploratory/README.txt',
  'figures/interactive/README.txt'
)
$expectedDashboardFigures = @($mapping | Where-Object {
  $_.target -match '[\\/]figures[\\/].*\.(svg|pdf|png|jpg|jpeg|tif|tiff|html)$'
}).Count

function Source-Root($Row) {
  $sourceRel = $Row.source_rel.Replace('\', '/')
  $targetFile = $Row.target_file.Replace('\', '/')
  $suffix = '/' + $targetFile
  if (-not $sourceRel.EndsWith($suffix, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Source path does not end with its target file: $sourceRel"
  }
  $sourceRoot = $sourceRel.Substring(0, $sourceRel.Length - $suffix.Length)
  if ([string]::IsNullOrWhiteSpace($sourceRoot)) {
    throw "Missing source group root: $sourceRel"
  }
  return $sourceRoot
}

function Assert-Sources([bool] $RequireReady = $true) {
  # Remove target_file from source_rel to recover the source group root. This
  # also covers nested files and source roots shared by tables and audit groups.
  $roots = @($rows | ForEach-Object { Source-Root $_ } | Select-Object -Unique)
  foreach ($sourceRoot in $roots) {
    if ($ownershipRows.Count -gt 0) {
      $prefix = $sourceRoot.Replace('\', '/').TrimEnd('/') + '/'
      $owned = @($ownershipRows | Where-Object {
        $_.source_rel.Replace('\', '/').StartsWith(
          $prefix, [System.StringComparison]::OrdinalIgnoreCase)
      })
      $planned = @($owned | ForEach-Object { ChildPath $root $_.source_rel })
      foreach ($row in $rows) {
        if ((Source-Root $row) -ieq $sourceRoot) {
          $matches = @($owned | Where-Object { $_.source_rel -ieq $row.source_rel })
          if ($matches.Count -ne 1 -or $matches[0].owner_group -cne $Group -or
              ($RequireReady -and $matches[0].state -cne 'ready')) {
            throw "Planned source is not owned by ${Group}: $($row.source_rel)"
          }
        }
      }
    } else {
      $planned = @($allRows | Where-Object { (Source-Root $_) -ieq $sourceRoot } |
                   ForEach-Object { ChildPath $root $_.source_rel })
    }
    $directory = ChildPath $root $sourceRoot
    $actual = @(Get-ChildItem -LiteralPath $directory -Force -File -Recurse |
                Where-Object { $_.Name -ine 'Thumbs.db' } |
                ForEach-Object { $_.FullName })
    if ($actual.Count -ne $planned.Count -or
        @($actual | Where-Object { $planned -notcontains $_ }).Count -gt 0) {
      throw "Source tree has unplanned files: $directory"
    }
  }
  foreach ($item in $mapping) {
    if (-not (Test-Path -LiteralPath $item.source -PathType Leaf)) {
      throw "Missing source: $($item.source)"
    }
    if ((Sha256 $item.source) -cne $item.sha256) {
      throw "Source changed since approval: $($item.source)"
    }
  }
}

function Assert-Contract {
  if ($gate -cne 'ready') { throw "Group $Group is blocked: gate=$gate" }
  if ($dashboardMetadata) {
    if ($expectedDashboardFigures -lt 1 -or
        -not (Get-Command Rscript -ErrorAction SilentlyContinue) -or
        -not (Test-Path -LiteralPath (Join-Path $PSScriptRoot 'Refresh-SystemsDashboardMetadata.R') -PathType Leaf)) {
      throw 'Dashboard activation requires Rscript and the metadata refresh helper'
    }
  }
  $contractRel = $rows[0].contract_file
  $contractHash = $rows[0].contract_sha256
  if ($contractHash -notmatch '^[0-9a-fA-F]{64}$') { throw "Missing reviewed code hash for $Group" }
  $contract = ChildPath $repo $contractRel
  if (-not (Test-Path -LiteralPath $contract -PathType Leaf) -or
      (Sha256 $contract) -cne $contractHash.ToLowerInvariant()) {
    throw "Reviewed repository path contract is missing or changed: $contract"
  }
  $codeRows = @(Import-Csv -LiteralPath $contract)
  if ($codeRows.Count -eq 0 -or
      -not ($codeRows[0].PSObject.Properties.Name -contains 'path') -or
      -not ($codeRows[0].PSObject.Properties.Name -contains 'sha256')) {
    throw "Reviewed code contract must list path and sha256 columns: $contract"
  }
  foreach ($codeRow in $codeRows) {
    if ($codeRow.sha256 -notmatch '^[0-9a-fA-F]{64}$') {
      throw "Invalid reviewed code hash: $($codeRow.path)"
    }
    $codePath = ChildPath $repo $codeRow.path
    if (-not (Test-Path -LiteralPath $codePath -PathType Leaf) -or
        (Sha256 $codePath) -cne $codeRow.sha256.ToLowerInvariant()) {
      throw "Reviewed producer, reader, or test changed: $codePath"
    }
  }
}

function Assert-Staged {
  if (-not (Test-Path -LiteralPath $stagingRoot -PathType Container)) {
    throw "Missing staging directory: $stagingRoot"
  }
  $expected = @($mapping | ForEach-Object { $_.staged })
  $actual = @(Get-ChildItem -LiteralPath $stagingRoot -File -Recurse | ForEach-Object { $_.FullName })
  if ($actual.Count -ne $expected.Count -or @($actual | Where-Object { $expected -notcontains $_ }).Count -gt 0) {
    throw "Staged file inventory differs from approved plan"
  }
  foreach ($item in $mapping) {
    if ((Sha256 $item.staged) -cne $item.sha256) {
      throw "Staged hash mismatch: $($item.staged)"
    }
  }
}

function Assert-DashboardMetadata {
  if (-not $dashboardMetadata) { return @() }
  $paths = @($metadataFiles | ForEach-Object { ChildPath $targetRoot $_ })
  $expected = @($mapping | ForEach-Object { $_.target }) + $paths
  $actual = @(Get-ChildItem -LiteralPath $targetRoot -File -Recurse |
              ForEach-Object { $_.FullName })
  if ($actual.Count -ne $expected.Count -or
      @($actual | Where-Object { $expected -notcontains $_ }).Count -gt 0) {
    throw 'Activated dashboard inventory differs from copied files plus seven generated metadata files'
  }
  @($metadataFiles | ForEach-Object {
    $path = ChildPath $targetRoot $_
    if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
      throw "Missing dashboard metadata: $path"
    }
    [pscustomobject]@{ path = $_; sha256 = Sha256 $path }
  })
}

function Assert-Receipt([string] $ExpectedState) {
  if (-not (Test-Path -LiteralPath $receiptPath -PathType Leaf)) {
    throw "Missing migration receipt: $receiptPath"
  }
  $receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
  $receiptOwnershipHash = if ($receipt.PSObject.Properties.Name -contains
                              'ownership_manifest_sha256') {
    [string]$receipt.ownership_manifest_sha256
  } else { '' }
  if ($receipt.group -cne $Group -or $receipt.state -cne $ExpectedState -or
      $receipt.group_plan_sha256 -cne $groupPlanHash -or
      $receipt.contract_sha256 -cne $rows[0].contract_sha256.ToLowerInvariant() -or
      $receipt.target_root_rel -cne $rows[0].target_root_rel -or
      [int]$receipt.files -ne $mapping.Count -or
      $receiptOwnershipHash -cne $ownershipHash) {
    throw "Migration receipt no longer matches the reviewed plan: $receiptPath"
  }
}

Assert-Sources ($Action -ne 'Inspect')
switch ($Action) {
  'Inspect' {
    $ownershipReady = if ($ownershipRows.Count -eq 0) { $true } else {
      @($rows | Where-Object {
        $source = $_.source_rel
        @($ownershipRows | Where-Object {
          $_.source_rel -ieq $source -and $_.owner_group -ceq $Group -and
          $_.state -ceq 'ready'
        }).Count -eq 1
      }).Count -eq $rows.Count
    }
    [pscustomobject]@{ group = $Group; gate = $gate; files = $mapping.Count;
      destination = $targetRoot; source_hashes = 'PASS'; ownership_ready = $ownershipReady;
      target_exists = (Test-Path -LiteralPath $targetRoot);
      staging_exists = (Test-Path -LiteralPath $stagingRoot) }
  }
  'Prepare' {
    Assert-Contract
    if (Test-Path -LiteralPath $targetRoot) { throw "Destination already exists: $targetRoot" }
    if (Test-Path -LiteralPath $receiptPath) { throw "Migration receipt already exists: $receiptPath" }
    foreach ($item in $mapping) {
      $parent = Split-Path -Parent $item.staged
      New-Item -ItemType Directory -Path $parent -Force | Out-Null
      if (Test-Path -LiteralPath $item.staged) {
        if ((Sha256 $item.staged) -cne $item.sha256) {
          throw "Existing staged file differs; refusing overwrite: $($item.staged)"
        }
      } else {
        Copy-Item -LiteralPath $item.source -Destination $item.staged -ErrorAction Stop
      }
    }
    Assert-Staged
    Assert-Sources
    $preparedReceipt = [pscustomobject]@{
      group = $Group; state = 'prepared'; files = $mapping.Count
      target_root_rel = $rows[0].target_root_rel
      group_plan_sha256 = $groupPlanHash
      contract_sha256 = $rows[0].contract_sha256.ToLowerInvariant()
      ownership_manifest_sha256 = $ownershipHash
      prepared_at_utc = [DateTime]::UtcNow.ToString('o')
      source_retained = $true
    }
    Write-Receipt $receiptPath $preparedReceipt
    [pscustomobject]@{ group = $Group; action = 'prepared'; files = $mapping.Count;
      staging = $stagingRoot; receipt = $receiptPath; hashes = 'PASS' }
  }
  'Verify' {
    Assert-Contract
    Assert-Receipt 'prepared'
    Assert-Staged
    [pscustomobject]@{ group = $Group; action = 'verified'; files = $mapping.Count;
      hashes = 'PASS' }
  }
  'Activate' {
    Assert-Contract
    Assert-Receipt 'prepared'
    if (Test-Path -LiteralPath $targetRoot) {
      # Recover an interruption after the directory move but before the
      # receipt was promoted. Never accept an unexpected destination.
      if (Test-Path -LiteralPath $stagingRoot) {
        throw "Both staging and destination exist; refusing activation: $targetRoot"
      }
      $actual = @(Get-ChildItem -LiteralPath $targetRoot -File -Recurse |
                  ForEach-Object { $_.FullName })
      $expected = @($mapping | ForEach-Object { $_.target })
      $allowed = if ($dashboardMetadata) {
        $expected + @($metadataFiles | ForEach-Object { ChildPath $targetRoot $_ })
      } else { $expected }
      if ($actual.Count -lt $expected.Count -or
          @($actual | Where-Object { $allowed -notcontains $_ }).Count -gt 0 -or
          @($expected | Where-Object { $actual -notcontains $_ }).Count -gt 0) {
        throw "Activated destination inventory differs from the reviewed plan"
      }
    } else {
      Assert-Staged
      $parent = Split-Path -Parent $targetRoot
      New-Item -ItemType Directory -Path $parent -Force | Out-Null
      Move-Item -LiteralPath $stagingRoot -Destination $targetRoot -ErrorAction Stop
    }
    foreach ($item in $mapping) {
      if ((Sha256 $item.target) -cne $item.sha256) {
        throw "Activated hash mismatch: $($item.target)"
      }
    }
    $generatedMetadata = @()
    if ($dashboardMetadata) {
      $rscript = (Get-Command Rscript -ErrorAction Stop).Source
      & $rscript (Join-Path $PSScriptRoot 'Refresh-SystemsDashboardMetadata.R') `
        $targetRoot $expectedDashboardFigures (FullPath (Join-Path $PSScriptRoot '..')) | Out-Null
      if ($LASTEXITCODE -ne 0) { throw 'Dashboard metadata regeneration failed' }
      $generatedMetadata = @(Assert-DashboardMetadata)
    }
    Assert-Sources
    $receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
    if ($dashboardMetadata) {
      $receipt | Add-Member -NotePropertyName generated_metadata -NotePropertyValue $generatedMetadata -Force
    }
    $receipt.state = 'activated'
    $receipt | Add-Member -NotePropertyName activated_at_utc -NotePropertyValue ([DateTime]::UtcNow.ToString('o'))
    Write-Receipt $receiptPath $receipt
    [pscustomobject]@{ group = $Group; action = 'activated'; files = $mapping.Count;
      destination = $targetRoot; receipt = $receiptPath;
      hashes = 'PASS'; originals_retained = $true }
  }
}
