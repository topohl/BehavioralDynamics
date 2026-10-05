#Requires -Version 7.2
# Move retired behaviour outputs under analysis_ready/history/retired/ with a manifest and a receipt.
#
# Nothing is deleted: every file is copied, its SHA-256 is verified at the target, and only then is the original
# removed. The default is a dry run that writes nothing. Run with -Execute only after the retirement was decided.
#
#   pwsh -File Maintenance/Invoke-BehaviorOutputRetirement.ps1 -AnalysisReady <root> -Label <label> `
#        -Source pipeline/21_cc1_active_longitudinal_gamm -Target history/retired/21_cc1_active_longitudinal_gamm
#   ... -Source pipeline/20_first_night_gamm/10min -Files <rel>,<rel> -Target history/retired/20_first_night_gamm_20260907
#
# -Source names a folder relative to analysis_ready. Without -Files the whole folder moves; with -Files only the
# listed paths (relative to -Source) move and the folder stays. -Target must lie under history/retired/ and must not
# exist. The manifest (rel, bytes, last_write_utc, sha256) is written to
# analysis_ready/_migration_control/retired_outputs/<label>_manifest.csv before anything moves; the receipt
# <label>.json records source, target, files, bytes, the manifest hash and the state (archived).
param(
  [Parameter(Mandatory = $true)] [string] $AnalysisReady,
  [Parameter(Mandatory = $true)] [ValidatePattern('^[a-z0-9_]+$')] [string] $Label,
  [Parameter(Mandatory = $true)] [string] $Source,
  [Parameter(Mandatory = $true)] [string] $Target,
  [string[]] $Files,
  [int] $MaxPathChars = 250,
  [switch] $Execute
)
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ChildPath([string] $Root, [string] $Relative) {
  if ([string]::IsNullOrWhiteSpace($Relative) -or [IO.Path]::IsPathRooted($Relative) -or
      $Relative -match '(^|[\\/])\.{1,2}([\\/]|$)') { throw "Unsafe relative path: $Relative" }
  $full = [IO.Path]::GetFullPath((Join-Path $Root $Relative))
  if (-not $full.StartsWith($Root + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
    throw "Path escapes analysis_ready: $Relative"
  }
  $full
}
function Sha([string] $Path) { (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant() }

$root = [IO.Path]::GetFullPath($AnalysisReady).TrimEnd('\', '/')
if ((Split-Path $root -Leaf) -ne 'analysis_ready' -or -not (Test-Path -LiteralPath $root -PathType Container)) {
  throw "Not an analysis_ready folder: $root"
}
$Target = $Target.Replace('\', '/').TrimEnd('/')
if ($Target -notmatch '^history/retired/[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)*$') { throw "Target must lie under history/retired/: $Target" }
$src = ChildPath $root $Source.Replace('\', '/').TrimEnd('/')
$dst = ChildPath $root $Target
if (-not (Test-Path -LiteralPath $src -PathType Container)) { throw "Missing source folder: $src" }
if (Test-Path -LiteralPath $dst) { throw "Target already exists: $dst" }
$control = Join-Path $root '_migration_control\retired_outputs'
$manifestPath = Join-Path $control "${Label}_manifest.csv"
$receiptPath = Join-Path $control "$Label.json"
foreach ($p in @($manifestPath, $receiptPath)) { if (Test-Path -LiteralPath $p) { throw "Already recorded: $p" } }

if ($Files) {
  $items = @($Files | ForEach-Object {
    $f = ChildPath $src $_.Replace('\', '/')
    if (-not (Test-Path -LiteralPath $f -PathType Leaf)) { throw "Listed file missing: $_" }
    Get-Item -LiteralPath $f -Force })
} else {
  $items = @(Get-ChildItem -LiteralPath $src -Recurse -File -Force)
}
if (-not $items.Count) { throw "Nothing to move under $src" }
$rows = foreach ($it in $items) {
  $rel = $it.FullName.Substring($src.Length + 1).Replace('\', '/')
  $to = Join-Path $dst $rel.Replace('/', '\')
  [pscustomobject]@{ rel = $rel; bytes = $it.Length; last_write_utc = $it.LastWriteTimeUtc.ToString('o')
                     sha256 = Sha $it.FullName; source = $it.FullName; target = $to }
}
$longest = ($rows | ForEach-Object { $_.target.Length } | Measure-Object -Maximum).Maximum
$bytes = ($rows | Measure-Object -Property bytes -Sum).Sum
Write-Output ("{0}: {1} files, {2} bytes, longest target path {3} characters -> {4}" -f $Label, $rows.Count, $bytes, $longest, $dst)
if ($longest -gt $MaxPathChars) { throw "A target path has $longest characters (limit $MaxPathChars); choose a shorter target" }
if (-not $Execute) { Write-Output 'Dry run: nothing written.'; return }

New-Item -ItemType Directory -Force -Path $control | Out-Null
$rows | Select-Object rel, bytes, last_write_utc, sha256 | Export-Csv -LiteralPath $manifestPath -NoTypeInformation -Encoding utf8
foreach ($r in $rows) {
  New-Item -ItemType Directory -Force -Path (Split-Path $r.target -Parent) | Out-Null
  Copy-Item -LiteralPath $r.source -Destination $r.target
  # -Force throughout: Explorer's Thumbs.db caches are hidden system files, invisible to Get-Item without it.
  (Get-Item -LiteralPath $r.target -Force).LastWriteTimeUtc = (Get-Item -LiteralPath $r.source -Force).LastWriteTimeUtc
  if ((Sha $r.target) -cne $r.sha256) { throw "Copy differs from the original: $($r.rel); nothing was removed" }
}
foreach ($r in $rows) { Remove-Item -LiteralPath $r.source -Force }
if (-not $Files) {
  Get-ChildItem -LiteralPath $src -Recurse -Directory -Force | Sort-Object { $_.FullName.Length } -Descending |
    ForEach-Object { if (-not (Get-ChildItem -LiteralPath $_.FullName -Force)) { Remove-Item -LiteralPath $_.FullName -Force } }
  if (-not (Get-ChildItem -LiteralPath $src -Force)) { Remove-Item -LiteralPath $src -Force }
}
$left = @($rows | Where-Object { Test-Path -LiteralPath $_.source })
if ($left.Count) { throw "$($left.Count) originals were not removed" }
[pscustomobject]@{
  label = $Label; source_rel = $Source; target_rel = $Target; files = $rows.Count; bytes = $bytes
  manifest_sha256 = Sha $manifestPath; state = 'archived'; archived_utc = [DateTime]::UtcNow.ToString('o')
} | ConvertTo-Json | Set-Content -LiteralPath $receiptPath -Encoding utf8
Write-Output "Archived $($rows.Count) files to $dst; receipt $receiptPath"
