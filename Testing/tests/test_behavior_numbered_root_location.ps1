Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot '..\..\Maintenance\BehaviorNumberedRootLocation.ps1')
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-numbered-root-location-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$root = '12_systems_neuroscience_summary'
$original = Join-Path $ready $root
$archived = Join-Path $ready "history\original_layout\$root"
$receipt = Join-Path $ready "_migration_control\numbered_root_archive\$root.json"
New-Item -ItemType Directory -Path (Join-Path $original '5min_based') -Force | Out-Null
New-Item -ItemType Directory -Path (Split-Path -Parent $receipt) -Force | Out-Null

function Expect-Failure([scriptblock] $Block, [string] $Pattern) {
  try {
    & $Block | Out-Null
  } catch {
    if ($_.Exception.Message -notmatch $Pattern) {
      throw "Unexpected location rejection (expected /$Pattern/): $($_.Exception.Message)"
    }
    return
  }
  throw "Expected numbered-root location rejection: $Pattern"
}
function Write-Receipt([string] $State, [string] $ArchiveRel = "history/original_layout/$root") {
  [pscustomobject]@{
    root = $root; source_root_rel = $root; archive_root_rel = $ArchiveRel; state = $State
    files = 1; bytes = 1; manifest_sha256 = ('c' * 64); reader_gate_kind = 'ArchivePath'
    reader_gate_sha256 = ('e' * 64); reader_queue_sha256 = ('f' * 64)
  } | ConvertTo-Json | Set-Content -LiteralPath $receipt -Encoding utf8
}

if ((Resolve-BehaviorNumberedRoot $ready $root) -cne $original) { throw 'Unreceipted root did not resolve to its original' }
$mapped = Resolve-BehaviorRetainedPath $ready "$root/5min_based/tables/x.csv"
if ($mapped -cne (Join-Path $original '5min_based\tables\x.csv')) { throw 'Retained path did not map to the original' }
if ((Resolve-BehaviorRetainedPath $ready 'analyses/systems_dashboard/5min/x.csv') -cne
      (Join-Path $ready 'analyses\systems_dashboard\5min\x.csv')) {
  throw 'A semantic path was remapped'
}
Expect-Failure { Resolve-BehaviorNumberedRoot $ready 'history' } 'Unknown numbered behavioral root'

# Directory existence alone never selects the archive.
New-Item -ItemType Directory -Path $archived -Force | Out-Null
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'exists without a receipt'
Write-Receipt 'prepared'
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'Prepared numbered source archive has unexpected locations'
Remove-Item -LiteralPath $archived
if ((Resolve-BehaviorNumberedRoot $ready $root) -cne $original) { throw 'Prepared root did not resolve to its original' }
Write-Receipt 'transferring'
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'not readable.*transferring'
Write-Receipt 'activated'
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'Activated numbered source archive has unexpected locations'
Move-Item -LiteralPath $original -Destination $archived
if ((Resolve-BehaviorNumberedRoot $ready $root) -cne $archived -or
    (Resolve-BehaviorRetainedPath $ready "$root/5min_based/tables/x.csv") -cne
      (Join-Path $archived '5min_based\tables\x.csv')) {
  throw 'Activated root did not resolve to its archive'
}
New-Item -ItemType Directory -Path $original -Force | Out-Null
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'Activated numbered source archive has unexpected locations'
Remove-Item -LiteralPath $original

# Mismatched, malformed, or unknown receipts stop the caller.
Write-Receipt 'activated' 'history/original_layout/another_root'
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'Invalid numbered-root archive receipt'
Set-Content -LiteralPath $receipt -Value '{' -Encoding utf8
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'Invalid numbered-root archive receipt'
Write-Receipt 'unknown'
Expect-Failure { Resolve-BehaviorNumberedRoot $ready $root } 'not readable.*unknown'

# A tree listed after 03/06/12 resolves the same way under its own receipt.
$late = '16_manuscript_behavior_report'
$lateOriginal = Join-Path $ready $late
$lateArchived = Join-Path $ready "history\original_layout\$late"
New-Item -ItemType Directory -Path (Join-Path $lateOriginal '10min_based') -Force | Out-Null
if ((Resolve-BehaviorNumberedRoot $ready $late) -cne $lateOriginal) { throw 'Late root did not resolve to its original' }
Move-Item -LiteralPath $lateOriginal -Destination $lateArchived
[pscustomobject]@{
  root = $late; source_root_rel = $late; archive_root_rel = "history/original_layout/$late"
  state = 'activated'; files = 1; bytes = 1; manifest_sha256 = ('c' * 64)
  reader_gate_kind = 'ArchivePath'; reader_gate_sha256 = ('e' * 64); reader_queue_sha256 = ('f' * 64)
} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path (Split-Path -Parent $receipt) "$late.json") -Encoding utf8
if ((Resolve-BehaviorRetainedPath $ready "$late/10min_based/x.csv") -cne (Join-Path $lateArchived '10min_based\x.csv')) {
  throw 'Late root did not map to its archive'
}
Remove-Item -LiteralPath $fixture -Recurse -Force
Write-Output 'Numbered root location resolver fixture: PASS'
