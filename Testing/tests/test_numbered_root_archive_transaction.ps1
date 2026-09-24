Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorNumberedRootArchive.ps1'
$manifestTool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorNumberedRootArchiveManifest.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-numbered-archive-transaction-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$source = Join-Path $ready '06_behavioral_dynamics'
$archive = Join-Path $ready 'history\original_layout\06_behavioral_dynamics'
$manifest = Join-Path $fixture 'archive-manifest.csv'
$queue = Join-Path $fixture 'reader-queue.csv'
$gate = Join-Path $fixture 'reviewed-reader-gate.csv'
New-Item -ItemType Directory -Path (Join-Path $source 'dyadic_contacts') -Force | Out-Null
$sample = Join-Path $source 'dyadic_contacts\feature.csv'
[System.IO.File]::WriteAllText($sample, "AnimalNum,value`n1,2`n")
& $manifestTool -Action Build -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest $manifest | Out-Null
$manifestHash = (Get-FileHash -LiteralPath $manifest -Algorithm SHA256).Hash.ToLowerInvariant()
@([pscustomobject]@{ script = 'Testing/audits/example.R'; review_state = 'needs_reader_writer_review' }) |
  Export-Csv -LiteralPath $queue -NoTypeInformation -Encoding utf8
@([pscustomobject]@{ script = 'Testing/audits/example.R'; review_state = 'ready' }) |
  Export-Csv -LiteralPath $gate -NoTypeInformation -Encoding utf8
$gateHash = (Get-FileHash -LiteralPath $gate -Algorithm SHA256).Hash.ToLowerInvariant()

function Invoke-Archive([string] $Action) {
  & $tool -Action $Action -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest $manifest -ManifestSha256 $manifestHash `
    -ReaderQueue $queue -ReviewedReaderGate $gate `
    -ReviewedReaderGateSha256 $gateHash
}
function Expect-Failure([scriptblock] $Block) {
  try {
    & $Block | Out-Null
    throw 'Expected numbered-root archive rejection'
  } catch {
    if ($_.Exception.Message -ceq 'Expected numbered-root archive rejection') { throw }
  }
}

if ((Invoke-Archive 'Inspect').state -cne 'unprepared') {
  throw 'Initial archive inspection did not report unprepared'
}
if ((Invoke-Archive 'Prepare').state -cne 'prepared' -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive preparation did not verify'
}
Expect-Failure { Invoke-Archive 'Prepare' }

# The source is rehashed immediately before activation.
[System.IO.File]::AppendAllText($sample, "2,3`n")
Expect-Failure { Invoke-Archive 'Activate' }
[System.IO.File]::WriteAllText($sample, "AnimalNum,value`n1,2`n")

# A changed review gate cannot be used to activate a previously prepared plan.
$oldGateHash = $gateHash
@([pscustomobject]@{ script = 'Testing/audits/example.R'; review_state = 'needs_reader_writer_review' }) |
  Export-Csv -LiteralPath $gate -NoTypeInformation -Encoding utf8
$gateHash = (Get-FileHash -LiteralPath $gate -Algorithm SHA256).Hash.ToLowerInvariant()
Expect-Failure { Invoke-Archive 'Activate' }
@([pscustomobject]@{ script = 'Testing/audits/example.R'; review_state = 'ready' }) |
  Export-Csv -LiteralPath $gate -NoTypeInformation -Encoding utf8
$gateHash = (Get-FileHash -LiteralPath $gate -Algorithm SHA256).Hash.ToLowerInvariant()
if ($gateHash -cne $oldGateHash) { throw 'Fixture gate did not return to its original hash' }

if ((Invoke-Archive 'Activate').state -cne 'activated' -or
    (Test-Path -LiteralPath $source) -or
    -not (Test-Path -LiteralPath $archive) -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive activation did not preserve the fixture inventory'
}
Expect-Failure { Invoke-Archive 'Activate' }
New-Item -ItemType Directory -Path $source -Force | Out-Null
Expect-Failure { Invoke-Archive 'Verify' }
Move-Item -LiteralPath $source -Destination (Join-Path $fixture 'recreated_root')

if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source) -or
    (Test-Path -LiteralPath $archive) -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive rollback did not restore the fixture inventory'
}

# Interrupted before the directory move: the original remains and Rollback
# can return the receipt to prepared after a fresh hash check.
$receiptPath = Join-Path $ready '_migration_control\numbered_root_archive\06_behavioral_dynamics.json'
$receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
$receipt.state = 'transferring'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source)) {
  throw 'Pre-move interruption did not recover'
}

# Interrupted after the directory move: the archive is verified and moved
# back. A recreated old root would instead make Rollback fail closed.
$receipt.state = 'transferring'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
New-Item -ItemType Directory -Path (Split-Path -Parent $archive) -Force | Out-Null
Move-Item -LiteralPath $source -Destination $archive
if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source) -or (Test-Path -LiteralPath $archive)) {
  throw 'Post-move interruption did not recover'
}
Write-Output 'Numbered root archive transaction fixture: PASS'
