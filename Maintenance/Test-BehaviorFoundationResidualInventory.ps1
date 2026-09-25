#Requires -Version 7.2
param(
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [string] $RepositoryRoot = (Join-Path $PSScriptRoot '..')
)

# Read-only verification of the Stage 01 foundation cutover and the 32 files
# retained only under the numbered derived-metrics root. The retained root is
# found through its archive receipt, so the check also runs after the move.
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'BehaviorNumberedRootLocation.ps1')
$ready = [System.IO.Path]::GetFullPath($AnalysisReadyRoot).TrimEnd('\', '/')
$repo = [System.IO.Path]::GetFullPath($RepositoryRoot).TrimEnd('\', '/')
$planPath = Join-Path $repo 'docs\behavior_output_activated_plans\derived_metrics_stage01_ready_plan.csv'
$ownersPath = Join-Path $repo 'docs\behavior_output_activated_plans\derived_metrics_stage01_ready_ownership.csv'
# Files that appeared in the numbered root after its activation. The ownership
# snapshot is pinned by the activation receipt and is not edited.
$additionsPath = Join-Path $repo 'docs\behavior_output_activated_plans\derived_metrics_post_activation_additions.csv'
$rows = @(Import-Csv -LiteralPath $planPath)
$owners = @(Import-Csv -LiteralPath $ownersPath)
$additions = @(Import-Csv -LiteralPath $additionsPath)
$receipt = Get-Content -LiteralPath (Join-Path $ready '_migration_control\behavior_metrics_foundation.json') -Raw |
  ConvertFrom-Json
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
$planText = (@($rows | Sort-Object source_rel | ForEach-Object {
  @($_.group, $_.source_rel, $_.target_root_rel, $_.target_file,
    $_.source_sha256, $_.gate, $_.contract_file, $_.contract_sha256) -join [char]31
}) -join "`n")
$ownerHash = FileHash $ownersPath
if ($rows.Count -ne 20 -or $owners.Count -ne 52 -or
    $receipt.group -cne 'behavior_metrics_foundation' -or
    $receipt.state -cne 'activated' -or -not $receipt.source_retained -or
    [int]$receipt.files -ne $rows.Count -or
    $receipt.group_plan_sha256 -cne (Sha256Text $planText) -or
    $receipt.ownership_manifest_sha256 -cne $ownerHash) {
  throw 'Stage 01 foundation receipt differs from the reviewed plan or ownership snapshot'
}
$sourceSet = [System.Collections.Generic.HashSet[string]]::new(
  [System.StringComparer]::OrdinalIgnoreCase)
$targetSet = [System.Collections.Generic.HashSet[string]]::new(
  [System.StringComparer]::OrdinalIgnoreCase)
foreach ($row in $rows) {
  $source = Resolve-BehaviorRetainedPath $ready $row.source_rel
  $target = Join-Path $ready (($row.target_root_rel + '/' + $row.target_file).Replace('/', '\'))
  $want = $row.source_sha256.ToLowerInvariant()
  if ((FileHash $source) -cne $want -or (FileHash $target) -cne $want) {
    throw "Stage 01 original or semantic copy differs: $($row.source_rel)"
  }
  if (-not $sourceSet.Add($row.source_rel) -or -not $targetSet.Add($target)) {
    throw 'Duplicate Stage 01 plan path'
  }
}
$semanticRoot = Join-Path $ready 'foundations\behavior_metrics'
$semanticFiles = @(Get-ChildItem -LiteralPath $semanticRoot -File -Recurse |
                   Where-Object { $_.Name -ine 'Thumbs.db' })
if ($semanticFiles.Count -ne $targetSet.Count -or
    @($semanticFiles | Where-Object { -not $targetSet.Contains($_.FullName) }).Count) {
  throw 'Stage 01 semantic inventory differs from its 20-file plan'
}
$oldRoot = Resolve-BehaviorNumberedRoot $ready '03_derived_metrics'
$oldFiles = @(Get-ChildItem -LiteralPath $oldRoot -File -Recurse |
              Where-Object { $_.Name -ine 'Thumbs.db' })
$oldRel = @($oldFiles | ForEach-Object {
  '03_derived_metrics/' + $_.FullName.Substring($oldRoot.Length + 1).Replace('\', '/')
})
foreach ($addition in $additions) {
  $path = Resolve-BehaviorRetainedPath $ready $addition.source_rel
  if ($addition.source_rel -in $owners.source_rel -or
      $addition.classification -cne 'retained_only_unpromoted' -or
      -not (Test-Path -LiteralPath $path -PathType Leaf) -or
      (Get-Item -LiteralPath $path).Length -ne [long]$addition.size_bytes -or
      (FileHash $path) -cne $addition.sha256.ToLowerInvariant()) {
    throw "Post-activation addition differs from its reviewed record: $($addition.source_rel)"
  }
}
$expectedRel = @($owners.source_rel) + @($additions.source_rel)
if ($oldFiles.Count -ne 52 + $additions.Count -or
    @($oldRel | Where-Object { $_ -notin $expectedRel }).Count -or
    @($expectedRel | Where-Object { $_ -notin $oldRel }).Count) {
  throw 'Numbered derived-metrics tree differs from its 52-row ownership snapshot and reviewed additions'
}
$retained = @($owners | Where-Object { -not $sourceSet.Contains($_.source_rel) })
$identity = @($retained | Where-Object { $_.owner_group -ceq 'cross_scale_identity_validation' })
$spatial = @($retained | Where-Object { $_.owner_group -ceq 'stage19_retained_original' })
$metadata = @($retained | Where-Object { $_.owner_group -ceq 'stage01_generated_metadata_retained' })
if ($retained.Count -ne 32 -or $identity.Count -ne 8 -or
    $spatial.Count -ne 14 -or $metadata.Count -ne 10) {
  throw 'Stage 01 retained-only ownership split changed'
}
Write-Output 'PASS: 20 Stage 01 originals and semantic copies match the plan and receipt'
Write-Output 'PASS: 52 numbered files; 32 retained-only (8 identity, 14 spatial, 10 metadata)'
Write-Output "PASS: $($additions.Count) reviewed post-activation addition(s), retained only: $oldRoot"
