param(
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [string] $RepositoryRoot = (Join-Path $PSScriptRoot '..')
)

# Read-only check of all activated Stage 14 source partitions and the files
# still present only in the numbered systems-summary tree.
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$ready = [System.IO.Path]::GetFullPath($AnalysisReadyRoot).TrimEnd('\', '/')
$repo = [System.IO.Path]::GetFullPath($RepositoryRoot).TrimEnd('\', '/')
$oldRoot = Join-Path $ready '12_systems_neuroscience_summary'
if (-not (Test-Path -LiteralPath $oldRoot -PathType Container)) {
  throw "Missing numbered Stage 14 tree: $oldRoot"
}
$planFiles = @(
  (Join-Path $repo 'docs\BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv'),
  (Join-Path $repo 'docs\behavior_output_activated_plans\stage14_dashboard_activated_plan.csv'),
  (Join-Path $repo 'docs\behavior_output_activated_plans\rfid_domain_comparison_activated_plan.csv'),
  (Join-Path $repo 'docs\behavior_output_activated_plans\rfid_leading_bin_seed_activated_plan.csv'),
  (Join-Path $repo 'docs\behavior_output_activated_plans\rfid_remaining_four_activated_plan.csv')
)
$rows = @(foreach ($plan in $planFiles) {
  Import-Csv -LiteralPath $plan | Where-Object {
    $_.source_rel.StartsWith('12_systems_neuroscience_summary/',
                            [System.StringComparison]::OrdinalIgnoreCase)
  }
})
$ownersPath = Join-Path $repo 'docs\behavior_output_activated_plans\stage14_activated_ownership.csv'
$ownersHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()

function Sha256Text([string] $value) {
  $algorithm = [System.Security.Cryptography.SHA256]::Create()
  try {
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($value)
    [System.BitConverter]::ToString($algorithm.ComputeHash($bytes)).Replace('-', '').ToLowerInvariant()
  } finally { $algorithm.Dispose() }
}
function FileHash([string] $path) {
  (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant()
}

$mapped = [System.Collections.Generic.HashSet[string]]::new(
  [System.StringComparer]::OrdinalIgnoreCase)
$groups = @($rows | Group-Object group)
foreach ($group in $groups) {
  $items = @($group.Group)
  $planText = (@($items | Sort-Object source_rel | ForEach-Object {
    @($_.group, $_.source_rel, $_.target_root_rel, $_.target_file,
      $_.source_sha256, $_.gate, $_.contract_file, $_.contract_sha256) -join [char]31
  }) -join "`n")
  $receiptPath = Join-Path $ready ("_migration_control\$($group.Name).json")
  $receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
  if ($receipt.state -cne 'activated' -or -not $receipt.source_retained -or
      [int]$receipt.files -ne $items.Count -or
      $receipt.group_plan_sha256 -cne (Sha256Text $planText) -or
      $receipt.target_root_rel -cne $items[0].target_root_rel) {
    throw "Activation receipt differs from the repository plan: $($group.Name)"
  }
  if ($group.Name -ceq 'systems_dashboard_5min' -and
      $receipt.ownership_manifest_sha256 -cne $ownersHash) {
    throw 'Stage 14 dashboard ownership hash differs from its receipt'
  }
  $targetRoot = Join-Path $ready ($items[0].target_root_rel.Replace('/', '\'))
  $allowed = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase)
  foreach ($item in $items) {
    if (-not $mapped.Add($item.source_rel)) {
      throw "Duplicate Stage 14 source: $($item.source_rel)"
    }
    $source = Join-Path $ready ($item.source_rel.Replace('/', '\'))
    $target = Join-Path $targetRoot ($item.target_file.Replace('/', '\'))
    $expected = $item.source_sha256.ToLowerInvariant()
    if ((FileHash $source) -cne $expected -or (FileHash $target) -cne $expected) {
      throw "Source or target hash mismatch: $($item.source_rel)"
    }
    [void]$allowed.Add($target)
  }
  if ($group.Name -ceq 'systems_dashboard_5min') {
    foreach ($metadata in $receipt.generated_metadata) {
      $path = Join-Path $targetRoot ($metadata.path.Replace('/', '\'))
      if ((FileHash $path) -cne $metadata.sha256) {
        throw "Generated dashboard metadata changed: $path"
      }
      [void]$allowed.Add($path)
    }
  }
  $actualTarget = @(Get-ChildItem -LiteralPath $targetRoot -File -Recurse |
                    Where-Object { $_.Name -ine 'Thumbs.db' })
  if ($actualTarget.Count -ne $allowed.Count -or
      @($actualTarget | Where-Object { -not $allowed.Contains($_.FullName) }).Count) {
    throw "Activated target inventory differs: $targetRoot"
  }
  Write-Output "$($group.Name): $($items.Count) hash-matched originals and copies"
}

$oldFiles = @(Get-ChildItem -LiteralPath $oldRoot -File -Recurse |
              Where-Object { $_.Name -ine 'Thumbs.db' })
$remaining = @(foreach ($file in $oldFiles) {
  $relative = '12_systems_neuroscience_summary/' +
    $file.FullName.Substring($oldRoot.Length + 1).Replace('\', '/')
  if (-not $mapped.Contains($relative)) { $relative }
})
$hmm = @($remaining | Where-Object {
  $_.StartsWith('12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/')
})
$qcFigures = @($remaining | Where-Object {
  $_.StartsWith('12_systems_neuroscience_summary/5min_based/figures/qc/')
})
$other = @($remaining | Where-Object { $_ -notin $hmm -and $_ -notin $qcFigures })
if ($oldFiles.Count -ne 700 -or $mapped.Count -ne 371 -or
    $remaining.Count -ne 329 -or $hmm.Count -ne 183 -or
    $qcFigures.Count -ne 133 -or $other.Count -ne 13) {
  throw 'Stage 14 residual inventory changed from the reviewed snapshot'
}
$hmmMapPath = Join-Path $repo 'docs\behavior_output_activated_plans\stage14_hmm_audit_writer_map_20260924.csv'
$hmmMap = @(Import-Csv -LiteralPath $hmmMapPath)
$hmmMapped = @($hmmMap | ForEach-Object {
  '12_systems_neuroscience_summary/5min_based/' + $_.relative_path.Replace('\', '/')
})
if ($hmmMap.Count -ne 183 -or
    @($hmmMapped | Where-Object { $_ -notin $hmm }).Count -or
    @($hmm | Where-Object { $_ -notin $hmmMapped }).Count) {
  throw 'Historical HMM audit map differs from the retained 183-file tree'
}
foreach ($item in $hmmMap) {
  $path = Join-Path $ready (
    ('12_systems_neuroscience_summary/5min_based/' + $item.relative_path).Replace('/', '\'))
  if ((FileHash $path) -cne $item.source_sha256.ToLowerInvariant() -or
      $item.migration_gate -cne 'blocked_historical_input_lineage_and_writer_collisions') {
    throw "Historical HMM audit map changed: $($item.relative_path)"
  }
}
$owners = @(Import-Csv -LiteralPath $ownersPath)
$mirrorRows = @($owners | Where-Object {
  $_.owner_group -ceq 'systems_dashboard_redundant_mirror'
})
$dashboardFigures = @($rows | Where-Object {
  $_.group -ceq 'systems_dashboard_5min' -and
  $_.source_rel -match '/figures/.+\.(svg|pdf|png)$'
})
$figureByName = @{}
foreach ($figure in $dashboardFigures) {
  $name = [System.IO.Path]::GetFileName($figure.source_rel).ToLowerInvariant()
  if (-not $figureByName.ContainsKey($name)) { $figureByName[$name] = @() }
  $figureByName[$name] += $figure
}
if ($mirrorRows.Count -ne 132) { throw 'Unexpected Stage 14 QC mirror count' }
foreach ($mirror in $mirrorRows) {
  $name = [System.IO.Path]::GetFileName($mirror.source_rel).ToLowerInvariant()
  $candidates = $figureByName[$name]
  $mirrorPath = Join-Path $ready ($mirror.source_rel.Replace('/', '\'))
  $hash = FileHash $mirrorPath
  if (@($candidates | Where-Object {
    $_.source_sha256.ToLowerInvariant() -ceq $hash
  }).Count -ne 1) {
    throw "QC figure is not an exact authored-figure mirror: $($mirror.source_rel)"
  }
}
Write-Output "PASS: 700 numbered files; 371 activated copies; 329 retained-only files"
Write-Output "PASS: 183 historical HMM audit hashes; 132 exact QC figure mirrors"
Write-Output "Retained-only: 183 HMM audit, 133 QC figures (including README), 13 other records"
