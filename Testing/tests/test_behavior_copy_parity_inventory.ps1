Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
# Temporary fixture only; the parity check never reads or writes a live path.
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\Test-BehaviorCopyParityInventory.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-copy-parity-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$control = Join-Path $ready '_migration_control'
$root = '15_behavioral_adaptation_kinetics'
New-Item -ItemType Directory -Path (Join-Path $control 'numbered_root_archive') -Force | Out-Null

function Sha256Text([string] $value) {
  $algorithm = [System.Security.Cryptography.SHA256]::Create()
  try {
    [System.BitConverter]::ToString($algorithm.ComputeHash(
      [System.Text.Encoding]::UTF8.GetBytes($value))).Replace('-', '').ToLowerInvariant()
  } finally { $algorithm.Dispose() }
}
function Expect-Failure([scriptblock] $Block, [string] $Pattern) {
  try { & $Block | Out-Null } catch {
    if ($_.Exception.Message -notmatch $Pattern) {
      throw "Unexpected parity rejection (expected /$Pattern/): $($_.Exception.Message)"
    }
    return
  }
  throw "Expected parity rejection: $Pattern"
}

# One group, three files whose names sort differently under culture-aware and
# ordinal comparison, copied and then archived under receipts.
$files = @('tables/a_b.csv', 'tables/a-c.csv', 'tables/B.csv')
$target = 'analyses/adaptation_kinetics/10min'
$rows = foreach ($file in $files) {
  $content = "content of $file"
  foreach ($base in @("$root/10min_based", $target)) {
    $path = Join-Path $ready (($base + '/' + $file).Replace('/', '\'))
    New-Item -ItemType Directory -Path (Split-Path -Parent $path) -Force | Out-Null
    [System.IO.File]::WriteAllText($path, $content)
  }
  [pscustomobject]@{
    group = 'adaptation_kinetics_10min'; source_rel = "$root/10min_based/$file"
    target_root_rel = $target; target_file = $file; source_sha256 = (Sha256Text $content)
    gate = 'ready'; contract_file = 'docs/behavior_output_code_contracts/x.csv'
    contract_sha256 = ('b' * 64)
  }
}
$plan = Join-Path $fixture 'plan.csv'
$rows | Export-Csv -LiteralPath $plan -NoTypeInformation -Encoding utf8
function PlanHash([object[]] $ordered) {
  Sha256Text ((@($ordered | ForEach-Object {
    @($_.group, $_.source_rel, $_.target_root_rel, $_.target_file,
      $_.source_sha256, $_.gate, $_.contract_file, $_.contract_sha256) -join [char]31
  }) -join "`n"))
}
function Write-GroupReceipt([string] $planHash) {
  [pscustomobject]@{
    group = 'adaptation_kinetics_10min'; state = 'activated'; files = 3
    target_root_rel = $target; group_plan_sha256 = $planHash
    contract_sha256 = ('b' * 64); source_retained = $true
  } | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $control 'adaptation_kinetics_10min.json') -Encoding utf8
}
Write-GroupReceipt (PlanHash @($rows | Sort-Object source_rel))
New-Item -ItemType Directory -Path (Join-Path $ready 'history\original_layout') -Force | Out-Null
Move-Item -LiteralPath (Join-Path $ready $root) -Destination (Join-Path $ready "history\original_layout\$root")
[pscustomobject]@{
  root = $root; source_root_rel = $root; archive_root_rel = "history/original_layout/$root"
  state = 'activated'; files = 3; bytes = 1; manifest_sha256 = ('c' * 64)
  reader_gate_kind = 'ArchivePath'; reader_gate_sha256 = ('e' * 64); reader_queue_sha256 = ('f' * 64)
} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $control "numbered_root_archive\$root.json") -Encoding utf8

$report = Join-Path $fixture 'report.csv'
$out = & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -ReportCsv $report
if (-not ($out -match '^PASS: ') -or @(Import-Csv -LiteralPath $report).Count -ne 3 -or
    -not ($out -match 'culture')) {
  throw 'An intact copy and archived original did not pass'
}

# A receipt hashed over the ordinal order of the same rows is accepted.
$ordinal = [System.Collections.Generic.List[object]]::new([object[]] $rows)
$ordinal.Sort([Comparison[object]] { param($a, $b) [string]::CompareOrdinal($a.source_rel, $b.source_rel) })
if ((PlanHash @($ordinal)) -ceq (PlanHash @($rows | Sort-Object source_rel))) {
  throw 'Fixture names do not separate the two orders'
}
Write-GroupReceipt (PlanHash @($ordinal))
$out = & $tool -AnalysisReadyRoot $ready -PlanPaths $plan
if (-not ($out -match '^PASS: ') -or -not ($out -match 'ordinal')) { throw 'An ordinal-order receipt was refused' }

# An edited plan row, a changed copy, a missing original and a report inside
# analysis_ready are each refused.
Write-GroupReceipt ('d' * 64)
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan } 'differs from its reviewed plan'
Write-GroupReceipt (PlanHash @($ordinal))
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -ReportCsv (Join-Path $ready 'r.csv') } 'outside analysis_ready'
if (Test-Path -LiteralPath (Join-Path $ready 'r.csv')) { throw 'A report was written inside analysis_ready' }
$copy = Join-Path $ready 'analyses\adaptation_kinetics\10min\tables\B.csv'
[System.IO.File]::WriteAllText($copy, 'regenerated')
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan } 'Copy parity check failed'
[System.IO.File]::WriteAllText($copy, 'content of tables/B.csv')
$archivedFile = Join-Path $ready "history\original_layout\$root\10min_based\tables\a_b.csv"
Move-Item -LiteralPath $archivedFile -Destination (Join-Path $fixture 'held.csv')
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan } 'Copy parity check failed'
Move-Item -LiteralPath (Join-Path $fixture 'held.csv') -Destination $archivedFile

# Metadata the receipt pins is hash-checked, not reported as unplanned.
$meta = Join-Path $ready 'analyses\adaptation_kinetics\10min\figures\README.txt'
New-Item -ItemType Directory -Path (Split-Path -Parent $meta) -Force | Out-Null
[System.IO.File]::WriteAllText($meta, 'guide')
$receiptPath = Join-Path $control 'adaptation_kinetics_10min.json'
$record = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
$record | Add-Member -NotePropertyName generated_metadata -NotePropertyValue @(
  [pscustomobject]@{ path = 'figures/README.txt'; sha256 = (Sha256Text 'guide') })
$record | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath $receiptPath -Encoding utf8
$out = & $tool -AnalysisReadyRoot $ready -PlanPaths $plan
if (-not ($out -match '^PASS: ') -or -not ($out -match '0 unplanned file')) {
  throw 'Receipt-pinned metadata was not accepted'
}
[System.IO.File]::WriteAllText($meta, 'edited')
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan } 'Generated metadata recorded'
[System.IO.File]::WriteAllText($meta, 'guide')

# A file a later run added beside the copy is reported, not failed.
[System.IO.File]::WriteAllText((Join-Path $ready 'analyses\adaptation_kinetics\10min\tables\new.csv'), 'x')
$out = & $tool -AnalysisReadyRoot $ready -PlanPaths $plan
if (-not ($out -match '^PASS: ') -or -not ($out -match '1 unplanned file')) {
  throw 'An unplanned file beside the copy was not reported'
}

# A reviewed producer rerun: B.csv rewritten and new.csv added in the copy.
# The copy must match the record, the original must still match the plan.
$rerunDir = Join-Path $fixture 'reruns'
New-Item -ItemType Directory -Path $rerunDir | Out-Null
[System.IO.File]::WriteAllText($copy, 'rerun output')
function Write-Rerun([string] $priorB) {
  @([pscustomobject]@{ group = 'adaptation_kinetics_10min'; target_root_rel = $target; target_file = 'tables/B.csv'
                       prior_sha256 = $priorB; sha256 = (Sha256Text 'rerun output'); change = 'rewritten' },
    [pscustomobject]@{ group = 'adaptation_kinetics_10min'; target_root_rel = $target; target_file = 'tables/new.csv'
                       prior_sha256 = ''; sha256 = (Sha256Text 'x'); change = 'added' }) |
    Export-Csv -LiteralPath (Join-Path $rerunDir 'rerun_20260926.csv') -NoTypeInformation -Encoding utf8
}
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -RerunDir $rerunDir } 'Copy parity check failed'
Write-Rerun (Sha256Text 'content of tables/B.csv')
$out = & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -RerunDir $rerunDir
if (-not ($out -match '^PASS: ') -or -not ($out -match '1 copies match a recorded rerun') -or
    -not ($out -match '1 file\(s\) added by recorded reruns') -or -not ($out -match '0 unplanned file')) {
  throw 'A recorded rerun was not accepted'
}
Write-Rerun ('e' * 64)
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -RerunDir $rerunDir } 'does not follow its previous state'
Write-Rerun (Sha256Text 'content of tables/B.csv')
$archivedB = Join-Path $ready "history\original_layout\$root\10min_based\tables\B.csv"
[System.IO.File]::WriteAllText($archivedB, 'tampered original')
Expect-Failure { & $tool -AnalysisReadyRoot $ready -PlanPaths $plan -RerunDir $rerunDir } 'Copy parity check failed'
Remove-Item -LiteralPath $fixture -Recurse -Force
Write-Output 'Copy parity inventory: PASS'
