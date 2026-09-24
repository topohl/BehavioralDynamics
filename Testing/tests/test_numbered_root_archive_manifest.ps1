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

function Invoke-Manifest([string] $Action) {
  & $tool -Action $Action -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest $manifest
}
function Expect-Failure([scriptblock] $Block) {
  try {
    & $Block | Out-Null
    throw 'Expected archive manifest rejection'
  } catch {
    if ($_.Exception.Message -ceq 'Expected archive manifest rejection') { throw }
  }
}

$built = Invoke-Manifest 'Build'
$verified = Invoke-Manifest 'Verify'
if ($built.files -ne 2 -or $verified.files -ne 2 -or
    $built.manifest_sha256 -cne $verified.manifest_sha256 -or
    $verified.hashes -cne 'PASS') {
  throw 'Archive manifest build/verify did not agree'
}
Expect-Failure { Invoke-Manifest 'Build' }
Expect-Failure {
  & $tool -Action Build -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest (Join-Path $source 'self-manifest.csv')
}

[System.IO.File]::AppendAllText($first, "2,3`n")
Expect-Failure { Invoke-Manifest 'Verify' }
[System.IO.File]::WriteAllText($first, "AnimalNum,value`n1,2`n")
$extra = Join-Path $source 'unexpected.csv'
[System.IO.File]::WriteAllText($extra, 'extra')
Expect-Failure { Invoke-Manifest 'Verify' }
Move-Item -LiteralPath $extra -Destination (Join-Path $fixture 'unexpected.csv')
Move-Item -LiteralPath $second -Destination (Join-Path $fixture 'metadata.csv')
Expect-Failure { Invoke-Manifest 'Verify' }
Move-Item -LiteralPath (Join-Path $fixture 'metadata.csv') -Destination $second

$rows = @(Import-Csv -LiteralPath $manifest)
$rows[0].relative_path = '../escape.csv'
$rows | Export-Csv -LiteralPath $manifest -NoTypeInformation -Encoding utf8
Expect-Failure { Invoke-Manifest 'Verify' }

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
  -Location Archived }
New-Item -ItemType Directory -Path $source -Force | Out-Null
Expect-Failure { & $tool -Action Verify -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest $manifest -Location Archived }
Write-Output 'Numbered root archive manifest fixture: PASS'
