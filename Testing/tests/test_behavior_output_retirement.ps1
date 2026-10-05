Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
# Temporary fixture only; the retirement tool never reads or writes a live path here.
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorOutputRetirement.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) ('behavior-retirement-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$stage = Join-Path $ready 'pipeline\21_fixture_gamm\10min'
New-Item -ItemType Directory -Path (Join-Path $stage 'tables'), (Join-Path $stage 'figures') -Force | Out-Null
Set-Content -LiteralPath (Join-Path $stage 'tables\a.csv') -Value 'x,y'
Set-Content -LiteralPath (Join-Path $stage 'tables\old_b.csv') -Value 'old'
Set-Content -LiteralPath (Join-Path $stage 'figures\c.svg') -Value '<svg/>'
# An Explorer thumbnail cache: hidden and system, as on the live share.
$thumbs = Join-Path $stage 'figures\Thumbs.db'
Set-Content -LiteralPath $thumbs -Value 'cache'
(Get-Item -LiteralPath $thumbs -Force).Attributes = [IO.FileAttributes]::Hidden -bor [IO.FileAttributes]::System
function Sha([string] $Path) { (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant() }
function Listing { @(Get-ChildItem -LiteralPath $ready -Recurse -File -Force | ForEach-Object { $_.FullName } | Sort-Object) }
function Assert-Rejected([scriptblock] $Block, [string] $Pattern) {
  try { & $Block | Out-Null } catch {
    if ($_.Exception.Message -notmatch $Pattern) { throw "Unexpected rejection (expected /$Pattern/): $($_.Exception.Message)" }
    return
  }
  throw "Expected rejection: $Pattern"
}
try {
  $before = Listing
  $hashA = Sha (Join-Path $stage 'tables\a.csv')

  # Dry run: reports, writes nothing.
  $out = & $tool -AnalysisReady $ready -Label fixture_all -Source pipeline/21_fixture_gamm -Target history/retired/21_fixture_gamm
  if (-not ($out -match 'Dry run: nothing written')) { throw 'Dry run did not say so' }
  if (Compare-Object $before (Listing)) { throw 'Dry run changed the tree' }

  # Refusals: target outside history/retired, path too long, missing listed file.
  Assert-Rejected { & $tool -AnalysisReady $ready -Label bad -Source pipeline/21_fixture_gamm -Target pipeline/elsewhere } 'history/retired'
  Assert-Rejected { & $tool -AnalysisReady $ready -Label bad -Source pipeline/21_fixture_gamm -Target history/retired/x -MaxPathChars 20 } 'limit 20'
  Assert-Rejected { & $tool -AnalysisReady $ready -Label bad -Source pipeline/21_fixture_gamm/10min -Files tables/none.csv -Target history/retired/x } 'Listed file missing'

  # Listed files only: the folder stays, the file moves with a manifest and a receipt.
  & $tool -AnalysisReady $ready -Label fixture_old -Source pipeline/21_fixture_gamm/10min -Files tables/old_b.csv `
    -Target history/retired/21_fixture_gamm_old -Execute | Out-Null
  if (Test-Path -LiteralPath (Join-Path $stage 'tables\old_b.csv')) { throw 'Listed file was not moved' }
  if (-not (Test-Path -LiteralPath (Join-Path $ready 'history\retired\21_fixture_gamm_old\tables\old_b.csv'))) { throw 'Listed file missing at the target' }
  if (-not (Test-Path -LiteralPath (Join-Path $stage 'tables\a.csv'))) { throw 'An unlisted file moved' }

  # Whole folder: every file arrives byte-identical, the source folder is gone, manifest and receipt agree.
  & $tool -AnalysisReady $ready -Label fixture_all -Source pipeline/21_fixture_gamm -Target history/retired/21_fixture_gamm -Execute | Out-Null
  if (Test-Path -LiteralPath (Join-Path $ready 'pipeline\21_fixture_gamm')) { throw 'Source folder still exists' }
  $moved = Join-Path $ready 'history\retired\21_fixture_gamm\10min\tables\a.csv'
  if ((Sha $moved) -cne $hashA) { throw 'Moved file differs' }
  if (-not (Test-Path -LiteralPath (Join-Path $ready 'history\retired\21_fixture_gamm\10min\figures\Thumbs.db'))) { throw 'The hidden file did not move' }
  $control = Join-Path $ready '_migration_control\retired_outputs'
  $manifest = @(Import-Csv -LiteralPath (Join-Path $control 'fixture_all_manifest.csv'))
  $receipt = Get-Content -LiteralPath (Join-Path $control 'fixture_all.json') -Raw | ConvertFrom-Json
  if ($manifest.Count -ne 3 -or $receipt.files -ne 3 -or $receipt.state -ne 'archived' -or
      $receipt.manifest_sha256 -cne (Sha (Join-Path $control 'fixture_all_manifest.csv'))) { throw 'Manifest or receipt wrong' }

  # A label or target is never reused.
  New-Item -ItemType Directory -Path (Join-Path $ready 'pipeline\22_fixture') -Force | Out-Null
  Set-Content -LiteralPath (Join-Path $ready 'pipeline\22_fixture\d.csv') -Value 'd'
  Assert-Rejected { & $tool -AnalysisReady $ready -Label fixture_all -Source pipeline/22_fixture -Target history/retired/22_fixture -Execute } 'Already recorded'
  Assert-Rejected { & $tool -AnalysisReady $ready -Label fixture_22 -Source pipeline/22_fixture -Target history/retired/21_fixture_gamm -Execute } 'already exists'
  Write-Output 'PASS: behaviour output retirement'
} finally {
  Remove-Item -LiteralPath $fixture -Recurse -Force -ErrorAction SilentlyContinue
}
