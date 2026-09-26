#Requires -Version 7.2
param(
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [string] $RepositoryRoot = (Join-Path $PSScriptRoot '..'),
  # Reviewed plans; each activated group must appear in exactly one of them.
  [string[]] $PlanPaths,
  # Optional per-row report. It must lie outside analysis_ready.
  [string] $ReportCsv,
  # Reviewed producer reruns (BehaviorProducerReruns.ps1).
  [string] $RerunDir
)

# Read-only check of every activated output group: its receipt against its
# reviewed plan, and each planned file in both the semantic copy and the
# retained original, which is found through the root's archive receipt. A copy
# that a reviewed producer rerun changed must match the recorded rerun hash;
# the original must still match the plan. Nothing under analysis_ready is
# written.
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'BehaviorNumberedRootLocation.ps1')
. (Join-Path $PSScriptRoot 'BehaviorProducerReruns.ps1')
$ready = [System.IO.Path]::GetFullPath($AnalysisReadyRoot).TrimEnd('\', '/')
$repo = [System.IO.Path]::GetFullPath($RepositoryRoot).TrimEnd('\', '/')
if (-not $RerunDir) { $RerunDir = Join-Path $repo 'docs\behavior_output_producer_reruns' }
$reruns = Import-BehaviorProducerReruns $RerunDir
if (-not $PlanPaths) {
  $PlanPaths = @(
    'docs\BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv',
    'docs\BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv',
    'docs\BEHAVIOR_REMAINING_MIGRATION_PLAN.csv',
    'docs\behavior_output_activated_plans\derived_metrics_stage01_ready_plan.csv',
    'docs\behavior_output_activated_plans\rfid_domain_comparison_activated_plan.csv',
    'docs\behavior_output_activated_plans\rfid_leading_bin_seed_activated_plan.csv',
    'docs\behavior_output_activated_plans\rfid_remaining_four_activated_plan.csv',
    'docs\behavior_output_activated_plans\stage14_dashboard_activated_plan.csv'
  ) | ForEach-Object { Join-Path $repo $_ }
}
if ($ReportCsv) {
  $report = [System.IO.Path]::GetFullPath($ReportCsv)
  if ($report.StartsWith($ready + '\', [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "The report must lie outside analysis_ready: $report"
  }
}

function Sha256Text([string] $value) {
  $algorithm = [System.Security.Cryptography.SHA256]::Create()
  try {
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($value)
    [System.BitConverter]::ToString($algorithm.ComputeHash($bytes)).Replace('-', '').ToLowerInvariant()
  } finally { $algorithm.Dispose() }
}
function FileHash([string] $path) {
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return 'missing' }
  (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant()
}
function PlanText([object[]] $rows) {
  (@($rows | ForEach-Object {
    @($_.group, $_.source_rel, $_.target_root_rel, $_.target_file,
      $_.source_sha256, $_.gate, $_.contract_file, $_.contract_sha256) -join [char]31
  }) -join "`n")
}
# Invoke-BehaviorOutputMigration.ps1 hashes the rows after a culture-aware
# Sort-Object, so the hash depends on the order the writing process used;
# four live receipts hold the ordinal or plan-file order of their rows. Every
# accepted order is a permutation of the same plan rows; the one that matched
# is reported.
function PlanOrder([object[]] $rows, [string] $want) {
  $ordinal = [System.Collections.Generic.List[object]]::new([object[]] $rows)
  $ordinal.Sort([Comparison[object]] { param($a, $b) [string]::CompareOrdinal($a.source_rel, $b.source_rel) })
  $orders = [ordered]@{
    culture = @($rows | Sort-Object source_rel)
    ordinal = @($ordinal)
    plan_file = $rows
  }
  foreach ($name in $orders.Keys) {
    if ((Sha256Text (PlanText $orders[$name])) -ceq $want) { return $name }
  }
  return $null
}

$planRows = @{}
foreach ($path in $PlanPaths) {
  $name = [System.IO.Path]::GetFileName($path)
  foreach ($row in @(Import-Csv -LiteralPath $path)) {
    if (-not $planRows.ContainsKey($row.group)) { $planRows[$row.group] = @{} }
    if (-not $planRows[$row.group].ContainsKey($name)) {
      $planRows[$row.group][$name] = [System.Collections.Generic.List[object]]::new()
    }
    $planRows[$row.group][$name].Add($row)
  }
}

$control = Join-Path $ready '_migration_control'
$receipts = @(Get-ChildItem -LiteralPath $control -Filter '*.json' -File | ForEach-Object {
  $record = Get-Content -LiteralPath $_.FullName -Raw | ConvertFrom-Json
  if ($record.PSObject.Properties.Name -contains 'group') { $record }
} | Sort-Object group)
if ($receipts.Count -eq 0) { throw "No output group receipt under $control" }

$targetRoots = @($receipts | Group-Object target_root_rel | Where-Object Count -gt 1)
if ($targetRoots.Count) { throw "Groups share a target root: $($targetRoots.Name -join ', ')" }

$summary = [System.Collections.Generic.List[object]]::new()
$details = [System.Collections.Generic.List[object]]::new()
$extraFiles = [System.Collections.Generic.List[string]]::new()
foreach ($receipt in $receipts) {
  $group = $receipt.group
  if (-not $planRows.ContainsKey($group) -or $planRows[$group].Count -ne 1) {
    throw "Group $group must appear in exactly one reviewed plan"
  }
  $planName = @($planRows[$group].Keys)[0]
  $rows = @($planRows[$group][$planName])
  $order = PlanOrder $rows $receipt.group_plan_sha256
  if ($receipt.state -cne 'activated' -or -not $receipt.source_retained -or
      [int] $receipt.files -ne $rows.Count -or $null -eq $order -or
      @($rows.target_root_rel | Sort-Object -Unique) -cne $receipt.target_root_rel -or
      @($rows.contract_sha256 | Sort-Object -Unique) -cne $receipt.contract_sha256) {
    throw "Receipt for $group differs from its reviewed plan in $planName"
  }
  $originalOk = 0; $copyOk = 0; $copyRerun = 0; $planned = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase)
  foreach ($row in $rows) {
    $want = $row.source_sha256.ToLowerInvariant()
    $wantCopy = Resolve-BehaviorRerunHash $reruns $group $row.target_file $want
    $original = Resolve-BehaviorRetainedPath $ready $row.source_rel
    $copy = Join-Path $ready (($row.target_root_rel + '/' + $row.target_file).Replace('/', '\'))
    [void] $planned.Add($copy)
    $originalHash = FileHash $original
    $copyHash = FileHash $copy
    if ($originalHash -ceq $want) { $originalOk++ }
    if ($copyHash -ceq $wantCopy) { if ($wantCopy -ceq $want) { $copyOk++ } else { $copyRerun++ } }
    $details.Add([pscustomobject]@{
      group = $group; source_rel = $row.source_rel
      target_rel = $row.target_root_rel + '/' + $row.target_file
      plan_sha256 = $want
      original = if ($originalHash -ceq $want) { 'match' } elseif ($originalHash -ceq 'missing') { 'missing' } else { 'differs' }
      copy = if ($copyHash -ceq $wantCopy) { if ($wantCopy -ceq $want) { 'match' } else { 'rerun' } }
             elseif ($copyHash -ceq 'missing') { 'missing' } else { 'differs' }
    })
  }
  # Metadata the migration generated at activation is pinned by the receipt.
  $targetRoot = Join-Path $ready $receipt.target_root_rel.Replace('/', '\')
  $metadata = @(if ($receipt.PSObject.Properties.Name -contains 'generated_metadata') {
    $receipt.generated_metadata
  })
  foreach ($entry in $metadata) {
    $path = Join-Path $targetRoot $entry.path.Replace('/', '\')
    $wantMeta = Resolve-BehaviorRerunHash $reruns $group $entry.path $entry.sha256
    if ((FileHash $path) -cne $wantMeta) {
      throw "Generated metadata recorded in the $group receipt changed: $path"
    }
    [void] $planned.Add($path)
  }
  # Files a reviewed rerun added must match their record.
  $added = @(Get-BehaviorRerunAddedFiles $reruns $group)
  foreach ($file in $added) {
    $path = Join-Path $targetRoot $file.Replace('/', '\')
    if ((FileHash $path) -cne (Resolve-BehaviorRerunHash $reruns $group $file '')) {
      throw "File added by a recorded rerun is missing or changed: $path"
    }
    [void] $planned.Add($path)
  }
  # Files an unrecorded producer run added beside the copy are reported, not failed.
  $extra = @(Get-ChildItem -LiteralPath $targetRoot -File -Recurse -Force |
             Where-Object { $_.Name -ine 'Thumbs.db' -and -not $planned.Contains($_.FullName) })
  foreach ($file in $extra) { $extraFiles.Add($file.FullName.Substring($ready.Length + 1)) }
  $summary.Add([pscustomobject]@{
    group = $group; plan = $planName; plan_order = $order; files = $rows.Count
    originals_match = $originalOk; copies_match = $copyOk; copies_rerun = $copyRerun
    rerun_added = $added.Count; receipt_metadata = $metadata.Count; extra_in_copy_root = $extra.Count
  })
}

if ($ReportCsv) { $details | Export-Csv -LiteralPath $report -NoTypeInformation -Encoding utf8 }
$summary | Format-Table -AutoSize | Out-String -Width 200 | Write-Output
$files = ($summary | Measure-Object files -Sum).Sum
$originals = ($summary | Measure-Object originals_match -Sum).Sum
$copies = ($summary | Measure-Object copies_match -Sum).Sum
$rerunCopies = ($summary | Measure-Object copies_rerun -Sum).Sum
$rerunAdded = ($summary | Measure-Object rerun_added -Sum).Sum
$extras = ($summary | Measure-Object extra_in_copy_root -Sum).Sum
Write-Output ("{0} activated groups, {1} planned files: {2} originals and {3} copies match the plan, {4} copies match a recorded rerun; {5} file(s) added by recorded reruns; {6} unplanned file(s) beside the copies" -f
  $summary.Count, $files, $originals, $copies, $rerunCopies, $rerunAdded, $extras)
$extraFiles | ForEach-Object { Write-Output "  unplanned: $_" }
if ($originals -ne $files -or ($copies + $rerunCopies) -ne $files) {
  @($details | Where-Object { $_.original -cne 'match' -or $_.copy -notin @('match', 'rerun') }) |
    Format-Table group, source_rel, original, copy -AutoSize | Out-String -Width 250 | Write-Output
  throw 'Copy parity check failed'
}
Write-Output 'PASS: every planned original matches its reviewed hash, and every copy matches it or a recorded rerun'
