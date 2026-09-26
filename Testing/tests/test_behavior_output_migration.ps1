$ErrorActionPreference = 'Stop'
$script = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorOutputMigration.ps1'
$sandbox = Join-Path ([System.IO.Path]::GetTempPath()) ("mmm_migration_test_" + [guid]::NewGuid().ToString('N'))
$root = Join-Path $sandbox 'analysis_ready'
$repo = Join-Path $sandbox 'repo'
$plan = Join-Path $sandbox 'plan.csv'

function Expect-Failure([scriptblock] $Block, [string] $Label) {
  $failed = $false
  try { & $Block | Out-Null } catch { $failed = $true }
  if (-not $failed) { throw "Expected failure: $Label" }
}

function Write-Text([string] $Path, [string] $Value) {
  [System.IO.File]::WriteAllText($Path, $Value, [System.Text.UTF8Encoding]::new($false))
}

try {
  New-Item -ItemType Directory -Path (Join-Path $root 'old'), $repo -Force | Out-Null
  $source = Join-Path $root 'old\sample.txt'
  $codeFile = Join-Path $repo 'path_contract.txt'
  $contract = Join-Path $repo 'reviewed_code.csv'
  Write-Text $source 'scientific output fixture'
  Write-Text $codeFile 'reviewed path contract'
  [pscustomobject]@{
    path = 'path_contract.txt'
    sha256 = (Get-FileHash -LiteralPath $codeFile -Algorithm SHA256).Hash.ToLowerInvariant()
  } | Export-Csv -LiteralPath $contract -NoTypeInformation
  $sourceHash = (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash.ToLowerInvariant()
  $contractHash = (Get-FileHash -LiteralPath $contract -Algorithm SHA256).Hash.ToLowerInvariant()
  $row = [pscustomobject]@{
    group = 'fixture'; source_rel = 'old/sample.txt'; target_root_rel = 'analyses/example'
    target_file = 'sample.txt'; source_sha256 = $sourceHash; gate = 'blocked_code_contract'
    contract_file = 'reviewed_code.csv'; contract_sha256 = $contractHash
  }
  $row | Export-Csv -LiteralPath $plan -NoTypeInformation

  $inspect = & $script -Action Inspect -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo
  if ($inspect.source_hashes -ne 'PASS' -or $inspect.gate -ne 'blocked_code_contract') {
    throw 'Read-only inspection did not report the blocked group correctly'
  }
  Expect-Failure { & $script -Action Prepare -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo } 'blocked gate'
  if (Test-Path -LiteralPath (Join-Path $root '_migration_control\incoming')) {
    throw 'Blocked prepare created a staging directory'
  }

  $row.gate = 'ready'
  $row | Export-Csv -LiteralPath $plan -NoTypeInformation
  $staged = Join-Path $root '_migration_control\incoming\fixture\sample.txt'
  New-Item -ItemType Directory -Path (Split-Path -Parent $staged) -Force | Out-Null
  Copy-Item -LiteralPath $source -Destination $staged
  & $script -Action Prepare -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  $other = [pscustomobject]@{
    group = 'other'; source_rel = 'other/ignored.txt'; target_root_rel = 'analyses/other'
    target_file = 'ignored.txt'; source_sha256 = $sourceHash; gate = 'blocked_code_contract'
    contract_file = ''; contract_sha256 = ''
  }
  @($row, $other) | Export-Csv -LiteralPath $plan -NoTypeInformation
  & $script -Action Verify -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  $target = Join-Path $root 'analyses\example\sample.txt'
  # Simulate interruption after the directory move and before the receipt
  # changes state; Activate must verify and finish that exact destination.
  New-Item -ItemType Directory -Path (Split-Path -Parent (Split-Path -Parent $target)) -Force | Out-Null
  Move-Item -LiteralPath (Split-Path -Parent $staged) -Destination (Split-Path -Parent $target)
  & $script -Action Activate -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  $receipt = Get-Content -LiteralPath (Join-Path $root '_migration_control\fixture.json') -Raw | ConvertFrom-Json
  if ($receipt.state -ne 'activated' -or $receipt.target_root_rel -ne 'analyses/example') {
    throw 'Activation receipt did not register the semantic destination'
  }
  if (-not (Test-Path -LiteralPath $source) -or -not (Test-Path -LiteralPath $target) -or
      (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash.ToLowerInvariant() -cne $sourceHash) {
    throw 'Activation failed to preserve the source and exact content'
  }
  Expect-Failure { & $script -Action Activate -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo } 'destination already exists'

  Write-Text $codeFile 'unreviewed code change'
  Expect-Failure { & $script -Action Prepare -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo } 'changed code contract'
  Write-Text $codeFile 'reviewed path contract'
  $extra = Join-Path $root 'old\unplanned.txt'
  Write-Text $extra 'unplanned output'
  Expect-Failure { & $script -Action Inspect -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo } 'unplanned source'
  Remove-Item -LiteralPath $extra
  $windowsCache = Join-Path $root 'old\Thumbs.db'
  Write-Text $windowsCache 'Windows thumbnail cache fixture'
  & $script -Action Inspect -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  Remove-Item -LiteralPath $windowsCache
  Write-Text $source 'unexpected source change'
  Expect-Failure { & $script -Action Inspect -Group fixture -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo } 'changed source'

  $nestedSource = Join-Path $root 'nested_old\figures\sample.txt'
  New-Item -ItemType Directory -Path (Split-Path -Parent $nestedSource) -Force | Out-Null
  Write-Text $nestedSource 'nested output fixture'
  $nestedRow = [pscustomobject]@{
    group = 'nested'; source_rel = 'nested_old/figures/sample.txt'
    target_root_rel = 'analyses/nested'; target_file = 'figures/sample.txt'
    source_sha256 = (Get-FileHash -LiteralPath $nestedSource -Algorithm SHA256).Hash.ToLowerInvariant()
    gate = 'ready'; contract_file = 'reviewed_code.csv'; contract_sha256 = $contractHash
  }
  $nestedRow | Export-Csv -LiteralPath $plan -NoTypeInformation
  & $script -Action Prepare -Group nested -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  & $script -Action Verify -Group nested -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  & $script -Action Activate -Group nested -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo | Out-Null
  $nestedTarget = Join-Path $root 'analyses\nested\figures\sample.txt'
  if (-not (Test-Path -LiteralPath $nestedSource) -or
      (Get-FileHash -LiteralPath $nestedTarget -Algorithm SHA256).Hash.ToLowerInvariant() -cne
      $nestedRow.source_sha256) {
    throw 'Nested output activation did not retain a matching source and target'
  }

  # A shared source root can be copied one owned group at a time only when a
  # reviewed manifest accounts for every file, including blocked neighbours.
  $sharedA = Join-Path $root 'shared\a.txt'
  $sharedB = Join-Path $root 'shared\b.txt'
  New-Item -ItemType Directory -Path (Split-Path -Parent $sharedA) -Force | Out-Null
  Write-Text $sharedA 'owned by A'
  Write-Text $sharedB 'owned by B'
  $sharedRow = [pscustomobject]@{
    group = 'shared_a'; source_rel = 'shared/a.txt'
    target_root_rel = 'analyses/shared_a'; target_file = 'a.txt'
    source_sha256 = (Get-FileHash -LiteralPath $sharedA -Algorithm SHA256).Hash.ToLowerInvariant()
    gate = 'ready'; contract_file = 'reviewed_code.csv'; contract_sha256 = $contractHash
  }
  $sharedRow | Export-Csv -LiteralPath $plan -NoTypeInformation
  $owners = @(
    [pscustomobject]@{ source_rel = 'shared/a.txt'; owner_group = 'shared_a'; state = 'ready' },
    [pscustomobject]@{ source_rel = 'shared/b.txt'; owner_group = 'shared_b'; state = 'requires_review' }
  )
  $ownersPath = Join-Path $sandbox 'owners.csv'
  $owners | Export-Csv -LiteralPath $ownersPath -NoTypeInformation
  $ownersHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()
  Expect-Failure {
    & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo
  } 'unplanned neighbour without ownership manifest'
  & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $ownersHash | Out-Null
  $sharedExtra = Join-Path $root 'shared\unexpected.txt'
  Write-Text $sharedExtra 'not assigned'
  Expect-Failure {
    & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $ownersHash
  } 'unexpected file beneath shared source root'
  Remove-Item -LiteralPath $sharedExtra
  Expect-Failure {
    & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 ('0' * 64)
  } 'changed ownership manifest hash'
  $owners[0].owner_group = 'shared_b'
  $owners | Export-Csv -LiteralPath $ownersPath -NoTypeInformation
  $wrongOwnerHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()
  Expect-Failure {
    & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $wrongOwnerHash
  } 'source assigned to the wrong group'
  $owners[0].owner_group = 'shared_a'
  $owners[0].state = 'requires_review'
  $owners | Export-Csv -LiteralPath $ownersPath -NoTypeInformation
  $blockedOwnerHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()
  $pending = & $script -Action Inspect -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $blockedOwnerHash
  if ($pending.source_hashes -ne 'PASS' -or $pending.ownership_ready) {
    throw 'Read-only inspection failed to distinguish verified hashes from pending ownership review'
  }
  Expect-Failure {
    & $script -Action Prepare -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $blockedOwnerHash
  } 'ownership review gate before preparation'
  $owners[0].state = 'ready'
  $owners | Export-Csv -LiteralPath $ownersPath -NoTypeInformation
  $ownersHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()
  & $script -Action Prepare -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $ownersHash | Out-Null
  Expect-Failure {
    & $script -Action Verify -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo
  } 'prepared receipt requires its reviewed ownership manifest'
  & $script -Action Verify -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $ownersHash | Out-Null
  & $script -Action Activate -Group shared_a -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $ownersHash | Out-Null
  if (-not (Test-Path -LiteralPath $sharedA) -or -not (Test-Path -LiteralPath $sharedB) -or
      -not (Test-Path -LiteralPath (Join-Path $root 'analyses\shared_a\a.txt'))) {
    throw 'Partitioned migration did not retain both originals and the copied target'
  }

  # Stage 14 adds only derived metadata after the verified copy and before
  # activation. Recover an interrupted move with a partially written index.
  $dashboardSource = Join-Path $root 'dashboard_old'
  $dashboardPub = Join-Path $dashboardSource 'figures\publication_panels\Fig_example.svg'
  $dashboardRootPlot = Join-Path $dashboardSource 'figures\Fig_integrated_systems_dashboard.svg'
  New-Item -ItemType Directory -Path (Split-Path -Parent $dashboardPub) -Force | Out-Null
  Write-Text $dashboardPub 'publication figure fixture'
  Write-Text $dashboardRootPlot 'authored root figure fixture'
  $dashboardRows = @(
    [pscustomobject]@{
      group = 'systems_dashboard_5min'; source_rel = 'dashboard_old/figures/publication_panels/Fig_example.svg'
      target_root_rel = 'analyses/systems_dashboard/5min'
      target_file = 'figures/publication_panels/Fig_example.svg'
      source_sha256 = (Get-FileHash -LiteralPath $dashboardPub -Algorithm SHA256).Hash.ToLowerInvariant()
      gate = 'ready'; contract_file = 'reviewed_code.csv'; contract_sha256 = $contractHash
    },
    [pscustomobject]@{
      group = 'systems_dashboard_5min'; source_rel = 'dashboard_old/figures/Fig_integrated_systems_dashboard.svg'
      target_root_rel = 'analyses/systems_dashboard/5min'
      target_file = 'figures/Fig_integrated_systems_dashboard.svg'
      source_sha256 = (Get-FileHash -LiteralPath $dashboardRootPlot -Algorithm SHA256).Hash.ToLowerInvariant()
      gate = 'ready'; contract_file = 'reviewed_code.csv'; contract_sha256 = $contractHash
    }
  )
  $dashboardRows | Export-Csv -LiteralPath $plan -NoTypeInformation
  $dashboardOwners = @(
    [pscustomobject]@{ source_rel = $dashboardRows[0].source_rel; owner_group = 'systems_dashboard_5min'; state = 'ready' },
    [pscustomobject]@{ source_rel = $dashboardRows[1].source_rel; owner_group = 'systems_dashboard_5min'; state = 'ready' }
  )
  $dashboardOwners | Export-Csv -LiteralPath $ownersPath -NoTypeInformation
  $dashboardOwnerHash = (Get-FileHash -LiteralPath $ownersPath -Algorithm SHA256).Hash.ToLowerInvariant()
  & $script -Action Prepare -Group systems_dashboard_5min -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $dashboardOwnerHash | Out-Null
  & $script -Action Verify -Group systems_dashboard_5min -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $dashboardOwnerHash | Out-Null
  $dashboardTarget = Join-Path $root 'analyses\systems_dashboard\5min'
  $dashboardStage = Join-Path $root '_migration_control\incoming\systems_dashboard_5min'
  New-Item -ItemType Directory -Path (Split-Path -Parent $dashboardTarget) -Force | Out-Null
  Move-Item -LiteralPath $dashboardStage -Destination $dashboardTarget
  $partialIndex = Join-Path $dashboardTarget 'tables\output_figure_inventory.csv'
  New-Item -ItemType Directory -Path (Split-Path -Parent $partialIndex) -Force | Out-Null
  Write-Text $partialIndex 'interrupted metadata fixture'
  & $script -Action Activate -Group systems_dashboard_5min -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -OwnershipManifest $ownersPath -OwnershipManifestSha256 $dashboardOwnerHash | Out-Null
  $dashboardReceipt = Get-Content -LiteralPath (Join-Path $root '_migration_control\systems_dashboard_5min.json') -Raw | ConvertFrom-Json
  $figureIndex = @(Import-Csv -LiteralPath $partialIndex)
  if ($dashboardReceipt.state -ne 'activated' -or
      @($dashboardReceipt.generated_metadata).Count -ne 7 -or
      $figureIndex.Count -ne 2 -or
      @($figureIndex | Where-Object { -not (Test-Path -LiteralPath (Join-Path $dashboardTarget $_.harmonized_path)) }).Count -ne 0 -or
      @(Get-ChildItem -LiteralPath $dashboardTarget -File -Recurse).Count -ne 9 -or
      -not (Test-Path -LiteralPath $dashboardPub) -or
      -not (Test-Path -LiteralPath $dashboardRootPlot)) {
    throw 'Dashboard activation did not recover metadata and preserve both scientific originals'
  }

  # History destinations require an explicit mode and one of the nine exact
  # old-resolution to semantic-resolution mappings.
  $historyGroup = 'history_social_networks_10min'
  $historyOldRoot = Join-Path $root '06_behavioral_dynamics\social_networks\10min_based'
  $historySource = Join-Path $historyOldRoot 'tables\animal_features.csv'
  New-Item -ItemType Directory -Path (Split-Path -Parent $historySource) -Force | Out-Null
  Write-Text $historySource "AnimalNum,value`n1,2"
  $historyRow = [pscustomobject]@{
    group = $historyGroup
    source_rel = '06_behavioral_dynamics/social_networks/10min_based/tables/animal_features.csv'
    target_root_rel = 'history/social_networks/10min'
    target_file = 'tables/animal_features.csv'
    source_sha256 = (Get-FileHash -LiteralPath $historySource -Algorithm SHA256).Hash.ToLowerInvariant()
    gate = 'blocked_review'
    contract_file = 'reviewed_code.csv'
    contract_sha256 = $contractHash
  }
  $historyRow | Export-Csv -LiteralPath $plan -NoTypeInformation
  Expect-Failure {
    & $script -Action Inspect -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo
  } 'history requires explicit mode'
  $inspection = & $script -Action Inspect -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination
  if ($inspection.gate -ne 'blocked_review' -or $inspection.source_hashes -ne 'PASS') {
    throw 'Historical blocked inspection failed'
  }
  Expect-Failure {
    & $script -Action Prepare -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination
  } 'history review gate'
  $historyRow.target_root_rel = 'history/social_networks/1min'
  $historyRow | Export-Csv -LiteralPath $plan -NoTypeInformation
  Expect-Failure {
    & $script -Action Inspect -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination
  } 'wrong historical resolution target'
  $historyRow.target_root_rel = 'history/social_networks/10min'
  $historyRow.gate = 'ready'
  $historyRow | Export-Csv -LiteralPath $plan -NoTypeInformation
  & $script -Action Prepare -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination | Out-Null
  & $script -Action Verify -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination | Out-Null
  & $script -Action Activate -Group $historyGroup -AnalysisReadyRoot $root -Plan $plan -RepositoryRoot $repo -HistoricalDestination | Out-Null
  $historyTarget = Join-Path $root 'history\social_networks\10min\tables\animal_features.csv'
  $historyReceipt = Get-Content -LiteralPath (Join-Path $root "_migration_control\$historyGroup.json") -Raw | ConvertFrom-Json
  if (-not (Test-Path -LiteralPath $historySource) -or
      -not (Test-Path -LiteralPath $historyTarget) -or
      $historyReceipt.state -ne 'activated' -or
      (Get-FileHash -LiteralPath $historyTarget -Algorithm SHA256).Hash.ToLowerInvariant() -cne
        $historyRow.source_sha256) {
    throw 'Historical fixture did not retain a byte-identical original and activated copy'
  }
  'PASS: migration fixture, blocked gate, hashes, activation, and change detection'
}
finally {
  $resolved = [System.IO.Path]::GetFullPath($sandbox)
  $tempRoot = [System.IO.Path]::GetFullPath([System.IO.Path]::GetTempPath()).TrimEnd('\', '/')
  if ($resolved.StartsWith($tempRoot + [System.IO.Path]::DirectorySeparatorChar,
                           [System.StringComparison]::OrdinalIgnoreCase) -and
      (Split-Path -Leaf $resolved) -match '^mmm_migration_test_[0-9a-f]{32}$') {
    Remove-Item -LiteralPath $resolved -Recurse -Force -ErrorAction SilentlyContinue
  }
}
