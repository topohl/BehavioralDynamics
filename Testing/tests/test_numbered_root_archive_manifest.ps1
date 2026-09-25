Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorNumberedRootArchiveManifest.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-numbered-archive-manifest-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$source = Join-Path $ready '06_behavioral_dynamics'
$tables = Join-Path $source 'dyadic_contacts\tables'
$manifest = Join-Path $fixture 'archive-manifest.csv'
New-Item -ItemType Directory -Path $tables -Force | Out-Null
$first = Join-Path $tables 'first.csv'
$second = Join-Path $source 'metadata.csv'
[System.IO.File]::WriteAllText($first, "AnimalNum,value`n1,2`n")
[System.IO.File]::WriteAllText($second, "name,value`na,b`n")
# Names whose order differs between ordinal and culture-aware sorting, as in
# the live 06 and 12 manifests (for example README.txt beside lower-case files).
foreach ($name in @('README.txt', 'hmm_state_occupancy.pdf', 'a-b.csv', 'ab.csv',
                    'Z.csv', 'a.csv')) {
  [System.IO.File]::WriteAllText((Join-Path $tables $name), $name)
}
$fileCount = 8

function Invoke-Manifest([string] $Action) {
  & $tool -Action $Action -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest $manifest
}
# Every rejection must fail for the reviewed reason, not for an unrelated error.
function Expect-Failure([scriptblock] $Block, [string] $Pattern) {
  try {
    & $Block | Out-Null
  } catch {
    if ($_.Exception.Message -notmatch $Pattern) {
      throw "Unexpected manifest rejection (expected /$Pattern/): $($_.Exception.Message)"
    }
    return
  }
  throw "Expected archive manifest rejection: $Pattern"
}

$built = Invoke-Manifest 'Build'
$verified = Invoke-Manifest 'Verify'
if ($built.files -ne $fileCount -or $verified.files -ne $fileCount -or
    $built.manifest_sha256 -cne $verified.manifest_sha256 -or
    $verified.hashes -cne 'PASS') {
  throw 'Archive manifest build/verify did not agree'
}
# Verification does not depend on the globalization mode, and a manifest in
# the older culture-aware row order (the committed live manifests) still
# verifies because rows are matched by path.
# The globalization mode is read when the child process starts.
foreach ($mode in @('DOTNET_SYSTEM_GLOBALIZATION_INVARIANT', 'DOTNET_SYSTEM_GLOBALIZATION_USENLS')) {
  Set-Item -Path "Env:$mode" -Value '1'
  try {
    $childResult = & pwsh -NoProfile -Command ("`$r = & '$tool' -Action Verify " +
      "-AnalysisReadyRoot '$ready' -RootName '06_behavioral_dynamics' -Manifest '$manifest'; " +
      "`$r.hashes") 2>&1
  } finally {
    Remove-Item -Path "Env:$mode"
  }
  if ($LASTEXITCODE -ne 0 -or ($childResult | Select-Object -Last 1) -cne 'PASS') {
    throw "Manifest verification depends on $mode : $($childResult -join ' | ')"
  }
}
$cultureManifest = Join-Path $fixture 'culture-order-manifest.csv'
@(Import-Csv -LiteralPath $manifest | Sort-Object -Property relative_path -CaseSensitive) |
  Export-Csv -LiteralPath $cultureManifest -NoTypeInformation -Encoding utf8
if ((Get-FileHash -LiteralPath $cultureManifest).Hash -ceq (Get-FileHash -LiteralPath $manifest).Hash) {
  throw 'Fixture names did not produce a different culture-aware order'
}
if ((& $tool -Action Verify -AnalysisReadyRoot $ready -RootName '06_behavioral_dynamics' `
      -Manifest $cultureManifest).hashes -cne 'PASS') {
  throw 'A culture-ordered manifest did not verify'
}
# Windows PowerShell 5.1 is refused rather than run with different semantics.
if (Get-Command powershell.exe -ErrorAction SilentlyContinue) {
  $legacy = & powershell.exe -NoProfile -File $tool -Action Verify -AnalysisReadyRoot $ready `
    -RootName '06_behavioral_dynamics' -Manifest $manifest 2>&1
  if ($LASTEXITCODE -eq 0 -or ($legacy -join ' ') -notmatch 'requires') {
    throw "Windows PowerShell ran the manifest tool: $($legacy -join ' | ')"
  }
  $global:LASTEXITCODE = 0
}
Expect-Failure { Invoke-Manifest 'Build' } 'Manifest already exists'
Expect-Failure {
  & $tool -Action Build -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest (Join-Path $source 'self-manifest.csv')
} 'cannot be written inside the source'
Expect-Failure {
  & $tool -Action Verify -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest $source
} 'cannot be written inside the source'

[System.IO.File]::AppendAllText($first, "2,3`n")
Expect-Failure { Invoke-Manifest 'Verify' } 'differs from numbered source'
[System.IO.File]::WriteAllText($first, "AnimalNum,value`n1,2`n")
$extra = Join-Path $source 'unexpected.csv'
[System.IO.File]::WriteAllText($extra, 'extra')
Expect-Failure { Invoke-Manifest 'Verify' } 'file count or uniqueness differs'
Move-Item -LiteralPath $extra -Destination (Join-Path $fixture 'unexpected.csv')
Move-Item -LiteralPath $second -Destination (Join-Path $fixture 'metadata.csv')
Expect-Failure { Invoke-Manifest 'Verify' } 'file count or uniqueness differs'
Move-Item -LiteralPath (Join-Path $fixture 'metadata.csv') -Destination $second

# Hidden system files such as Explorer's Thumbs.db are inventoried; a new or
# changed thumbnail cache therefore invalidates the reviewed snapshot.
$thumbs = Join-Path $tables 'Thumbs.db'
[System.IO.File]::WriteAllText($thumbs, 'thumbnail cache')
(Get-Item -LiteralPath $thumbs -Force).Attributes = 'Hidden, System'
Expect-Failure { Invoke-Manifest 'Verify' } 'file count or uniqueness differs'
Remove-Item -LiteralPath $thumbs -Force
# A case-only rename changes the recorded relative path and fails closed.
Rename-Item -LiteralPath $first -NewName 'First.csv'
Expect-Failure { Invoke-Manifest 'Verify' } 'differs from numbered source'
Rename-Item -LiteralPath (Join-Path $tables 'First.csv') -NewName 'first.csv'
if ((Invoke-Manifest 'Verify').hashes -cne 'PASS') {
  throw 'Restored fixture did not verify'
}

$rows = @(Import-Csv -LiteralPath $manifest)
$rows[0].relative_path = '../escape.csv'
$rows | Export-Csv -LiteralPath $manifest -NoTypeInformation -Encoding utf8
Expect-Failure { Invoke-Manifest 'Verify' } 'Invalid archive manifest row'

# Create a fresh fixture manifest before verifying the same files after a
# same-volume move to the proposed archive location.
$manifest = Join-Path $fixture 'archive-location-manifest.csv'
Invoke-Manifest 'Build' | Out-Null
$archived = Join-Path $ready 'history\original_layout\06_behavioral_dynamics'
New-Item -ItemType Directory -Path (Split-Path -Parent $archived) -Force | Out-Null
Move-Item -LiteralPath $source -Destination $archived
$archiveCheck = & $tool -Action Verify -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest $manifest -Location Archived
if ($archiveCheck.hashes -cne 'PASS' -or $archiveCheck.location -cne 'Archived') {
  throw 'Archived source did not match the original manifest'
}
Expect-Failure { & $tool -Action Build -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest (Join-Path $fixture 'invalid-build.csv') `
  -Location Archived } 'Build is allowed only from the original'
New-Item -ItemType Directory -Path $source -Force | Out-Null
Expect-Failure { & $tool -Action Verify -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest $manifest -Location Archived } 'recreated original root'
Remove-Item -LiteralPath $fixture -Recurse -Force
Write-Output 'Numbered root archive manifest fixture: PASS'
