Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorNumberedRootArchive.ps1'
$manifestTool = Join-Path $PSScriptRoot '..\..\Maintenance\Invoke-BehaviorNumberedRootArchiveManifest.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-numbered-archive-transaction-' + [guid]::NewGuid().ToString('N'))
$ready = Join-Path $fixture 'analysis_ready'
$source = Join-Path $ready '06_behavioral_dynamics'
$history = Join-Path $ready 'history'
$archive = Join-Path $ready 'history\original_layout\06_behavioral_dynamics'
$manifest = Join-Path $fixture 'archive-manifest.csv'
$queue = Join-Path $fixture 'reader-queue.csv'
$gate = Join-Path $fixture 'reviewed-reader-gate.csv'
$receiptPath = Join-Path $ready '_migration_control\numbered_root_archive\06_behavioral_dynamics.json'
New-Item -ItemType Directory -Path (Join-Path $source 'dyadic_contacts') -Force | Out-Null
$sample = Join-Path $source 'dyadic_contacts\feature.csv'
$second = Join-Path $source 'dyadic_contacts\second.csv'
[System.IO.File]::WriteAllText($sample, "AnimalNum,value`n1,2`n")
[System.IO.File]::WriteAllText($second, "AnimalNum,value`n3,4`n")
& $manifestTool -Action Build -AnalysisReadyRoot $ready -RootName `
  '06_behavioral_dynamics' -Manifest $manifest | Out-Null
$manifestHash = (Get-FileHash -LiteralPath $manifest -Algorithm SHA256).Hash.ToLowerInvariant()
$scripts = @('Testing/audits/audit_first_night_time_anchor.R',
             'Testing/audits/audit_phase_bug_impact.R')
$scriptHashes = @{}
foreach ($script in $scripts) {
  $scriptPath = Join-Path $PSScriptRoot ('..\..\' + ($script -replace '/', '\'))
  $scriptHashes[$script] = (Get-FileHash -LiteralPath $scriptPath -Algorithm SHA256).Hash.ToLowerInvariant()
}
$semantic = Join-Path $ready 'analyses\dyadic_contacts'
New-Item -ItemType Directory -Path $semantic -Force | Out-Null
$control = Join-Path $ready '_migration_control'
New-Item -ItemType Directory -Path $control -Force | Out-Null
@{
  group = 'dyadic_contacts'; state = 'activated'; files = 1
  target_root_rel = 'analyses/dyadic_contacts'
  group_plan_sha256 = ('a' * 64); contract_sha256 = ('b' * 64)
  source_retained = $true
} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $control 'dyadic_contacts.json') -Encoding utf8

function Write-Queue([string] $State = 'needs_reader_writer_review') {
  @($scripts | ForEach-Object { [pscustomobject]@{ script = $_; review_state = $State } }) |
    Export-Csv -LiteralPath $queue -NoTypeInformation -Encoding utf8
}
function Write-Gate([string] $State, [string[]] $Rows = $scripts,
                    [string] $StaleScript = '', [switch] $Evidence) {
  @($Rows | ForEach-Object {
    $row = [ordered]@{
      script = $_; review_state = $State
      script_sha256 = $(if ($_ -ceq $StaleScript) { '0' * 64 } else { $scriptHashes[$_] })
    }
    if ($Evidence) {
      $row.path_review_evidence = 'fixture path resolver'
      $row.writer_review_evidence = 'fixture writer guard'
    }
    [pscustomobject]$row
  }) | Export-Csv -LiteralPath $gate -NoTypeInformation -Encoding utf8
  $script:gateHash = (Get-FileHash -LiteralPath $gate -Algorithm SHA256).Hash.ToLowerInvariant()
}
function Invoke-Archive([string] $Action, [string] $GateKind = 'ScientificReplay',
                        [string] $ManifestSha256 = $manifestHash) {
  & $tool -Action $Action -AnalysisReadyRoot $ready -RootName `
    '06_behavioral_dynamics' -Manifest $manifest -ManifestSha256 $ManifestSha256 `
    -ReaderQueue $queue -ReviewedReaderGate $gate `
    -ReviewedReaderGateSha256 $gateHash -ReaderGateKind $GateKind
}
# Every rejection must fail for the reviewed reason, not for an unrelated error.
function Expect-Failure([scriptblock] $Block, [string] $Pattern) {
  try {
    & $Block | Out-Null
  } catch {
    if ($_.Exception.Message -notmatch $Pattern) {
      throw "Unexpected archive rejection (expected /$Pattern/): $($_.Exception.Message)"
    }
    return
  }
  throw "Expected numbered-root archive rejection: $Pattern"
}
function Receipt-State {
  (Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json).state
}
function Assert-Source-Intact {
  if (-not (Test-Path -LiteralPath $source -PathType Container) -or
      (Test-Path -LiteralPath $archive) -or
      @(Get-ChildItem -LiteralPath $source -Recurse -File -Force).Count -ne 2) {
    throw 'Numbered source is not intact at its original location'
  }
}
function Assert-R-Routes([string] $ExpectedSource) {
  $env:MMM_TEST_ARCHIVE_FIXTURE = $fixture
  $env:MMM_TEST_ARCHIVE_SOURCE = Join-Path $ExpectedSource 'dyadic_contacts'
  try {
    $result = & Rscript -e 'source("Functions/project_paths.R"); r <- Sys.getenv("MMM_TEST_ARCHIVE_FIXTURE"); expected <- normalizePath(Sys.getenv("MMM_TEST_ARCHIVE_SOURCE"), winslash="/", mustWork=TRUE); actual <- normalizePath(mmm_behavior_retained_source_root("dyadic_contacts", r), winslash="/", mustWork=TRUE); stopifnot(identical(actual, expected), identical(normalizePath(mmm_behavior_output_active_root("dyadic_contacts", r), winslash="/", mustWork=TRUE), normalizePath(file.path(r,"analysis_ready","analyses","dyadic_contacts"), winslash="/", mustWork=TRUE)))' 2>&1
    if ($LASTEXITCODE -ne 0) { throw "R resolver rejected the archive transaction receipt: $($result -join ' | ')" }
  } finally {
    Remove-Item Env:MMM_TEST_ARCHIVE_FIXTURE -ErrorAction SilentlyContinue
    Remove-Item Env:MMM_TEST_ARCHIVE_SOURCE -ErrorAction SilentlyContinue
  }
}
# Writers must refuse the numbered root and the archive whenever a receipt
# written by the transaction tool exists, while semantic outputs stay writable.
function Assert-R-Writers([bool] $Blocked) {
  $env:MMM_TEST_ARCHIVE_FIXTURE = $fixture
  $env:MMM_TEST_WRITERS_BLOCKED = if ($Blocked) { 'TRUE' } else { 'FALSE' }
  try {
    $result = & Rscript -e 'source("Functions/project_paths.R"); r <- Sys.getenv("MMM_TEST_ARCHIVE_FIXTURE"); blocked <- identical(Sys.getenv("MMM_TEST_WRITERS_BLOCKED"), "TRUE"); err <- function(x) inherits(try(x, silent=TRUE), "try-error"); ready <- file.path(r, "analysis_ready"); semantic <- file.path(ready, "analyses", "dyadic_contacts"); stopifnot(identical(err(mmm_behavior_numbered_writer_root("06_behavioral_dynamics", r)), blocked), identical(err(mmm_behavior_guard_numbered_output_path(file.path(ready, "06_behavioral_dynamics", "hmm_states", "1min_based"), r)), blocked), err(mmm_behavior_guard_numbered_output_path(file.path(ready, "history", "original_layout", "06_behavioral_dynamics", "x"), r)), identical(mmm_behavior_guard_numbered_output_path(semantic, r), semantic))' 2>&1
    if ($LASTEXITCODE -ne 0) { throw "R writer guard did not match the receipt state: $($result -join ' | ')" }
  } finally {
    Remove-Item Env:MMM_TEST_ARCHIVE_FIXTURE -ErrorAction SilentlyContinue
    Remove-Item Env:MMM_TEST_WRITERS_BLOCKED -ErrorAction SilentlyContinue
  }
}
function Assert-R-Rejects {
  $env:MMM_TEST_ARCHIVE_FIXTURE = $fixture
  try {
    $result = & Rscript -e 'source("Functions/project_paths.R"); r <- Sys.getenv("MMM_TEST_ARCHIVE_FIXTURE"); stopifnot(inherits(try(mmm_behavior_retained_source_root("dyadic_contacts", r), silent=TRUE), "try-error"), inherits(try(mmm_behavior_output_active_root("dyadic_contacts", r), silent=TRUE), "try-error"))' 2>&1
    if ($LASTEXITCODE -ne 0) { throw "R resolver did not fail closed: $($result -join ' | ')" }
  } finally {
    Remove-Item Env:MMM_TEST_ARCHIVE_FIXTURE -ErrorAction SilentlyContinue
  }
}

Write-Queue
Write-Gate 'ready'
if ((Invoke-Archive 'Inspect').state -cne 'unprepared') {
  throw 'Initial archive inspection did not report unprepared'
}
Assert-R-Writers $false

# Without -ReaderQueue the repository's 37-row audit queue applies, which a
# two-row fixture gate cannot satisfy.
Expect-Failure {
  & $tool -Action Prepare -AnalysisReadyRoot $ready -RootName '06_behavioral_dynamics' `
    -Manifest $manifest -ManifestSha256 $manifestHash -ReviewedReaderGate $gate `
    -ReviewedReaderGateSha256 $gateHash
} 'unique row for every queued script'
# A lock left by another action blocks every changing action.
$lockPath = Join-Path (Split-Path -Parent $receiptPath) '06_behavioral_dynamics.lock'
New-Item -ItemType Directory -Path (Split-Path -Parent $receiptPath) -Force | Out-Null
[System.IO.File]::WriteAllText($lockPath, 'held')
Expect-Failure { Invoke-Archive 'Prepare' } 'Another archive action holds'
Remove-Item -LiteralPath $lockPath

# A stale manifest hash, an incomplete or duplicated gate, and a ready label
# with a stale script hash must not prepare the transaction.
Expect-Failure { Invoke-Archive 'Prepare' -ManifestSha256 ('0' * 64) } 'changed archive manifest'
Write-Gate 'ready' -Rows @($scripts[0])
Expect-Failure { Invoke-Archive 'Prepare' } 'unique row for every queued script'
Write-Gate 'ready' -Rows @($scripts[0], $scripts[0])
Expect-Failure { Invoke-Archive 'Prepare' } 'unique row for every queued script'
Write-Gate 'ready' -StaleScript $scripts[1]
Expect-Failure { Invoke-Archive 'Prepare' } 'Reviewed audit script changed'
Write-Gate 'ready'

# A pre-existing destination, or an extra or missing source file, blocks
# Prepare and leaves no receipt.
New-Item -ItemType Directory -Path $archive -Force | Out-Null
Expect-Failure { Invoke-Archive 'Inspect' } 'Unreceipted archive destination exists'
Expect-Failure { Invoke-Archive 'Prepare' } 'Archive receipt or destination already exists'
Remove-Item -LiteralPath $archive
$extra = Join-Path $source 'dyadic_contacts\unexpected.csv'
[System.IO.File]::WriteAllText($extra, 'extra')
Expect-Failure { Invoke-Archive 'Prepare' } 'file count or uniqueness differs'
Remove-Item -LiteralPath $extra
Move-Item -LiteralPath $second -Destination (Join-Path $fixture 'second.csv')
Expect-Failure { Invoke-Archive 'Prepare' } 'file count or uniqueness differs'
Move-Item -LiteralPath (Join-Path $fixture 'second.csv') -Destination $second
if (Test-Path -LiteralPath $receiptPath) { throw 'A refused Prepare wrote a receipt' }

if ((Invoke-Archive 'Prepare').state -cne 'prepared' -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive preparation did not verify'
}
Assert-R-Routes $source
Assert-R-Writers $true
Expect-Failure { Invoke-Archive 'Prepare' } 'Archive receipt or destination already exists'
if (Test-Path -LiteralPath $lockPath) { throw 'A finished action left its lock behind' }

# A prepared root can leave archive control without moving anything. The
# receipt is kept for review and writers are released.
Expect-Failure { Invoke-Archive 'Abandon' } 'requires -AbandonReason'
& $tool -Action Abandon -AnalysisReadyRoot $ready -RootName '06_behavioral_dynamics' `
  -Manifest $manifest -ManifestSha256 $manifestHash -AbandonReason 'fixture' | Out-Null
$abandonedReceipts = @(Get-ChildItem -LiteralPath (Join-Path (Split-Path -Parent $receiptPath) 'abandoned') -Filter '06_behavioral_dynamics-*.json')
if ((Test-Path -LiteralPath $receiptPath) -or $abandonedReceipts.Count -ne 1 -or
    ((Get-Content -LiteralPath $abandonedReceipts[0].FullName -Raw | ConvertFrom-Json).abandon_reason -cne 'fixture')) {
  throw 'Abandon did not retire the prepared receipt'
}
Assert-Source-Intact
Assert-R-Writers $false
if ((Invoke-Archive 'Prepare').state -cne 'prepared') { throw 'Prepare after Abandon failed' }

# A failed receipt replacement keeps the previous receipt and leaves no
# temporary file; nothing has moved.
$receiptBefore = [System.IO.File]::ReadAllBytes($receiptPath)
$receiptHold = [System.IO.File]::Open($receiptPath, 'Open', 'Read', 'Read')
try {
  Expect-Failure { Invoke-Archive 'Activate' } 'denied|being used'
} finally {
  $receiptHold.Dispose()
}
if ([Convert]::ToBase64String([System.IO.File]::ReadAllBytes($receiptPath)) -cne
      [Convert]::ToBase64String($receiptBefore) -or
    @(Get-ChildItem -LiteralPath (Split-Path -Parent $receiptPath) -Filter '.numbered-root-*.tmp' -Force).Count -ne 0) {
  throw 'A failed receipt write changed or lost the prepared receipt'
}
Assert-Source-Intact

# The source is rehashed immediately before activation. A changed file or a
# file added by a writer after Prepare (as Stage 01 once did) blocks the move.
[System.IO.File]::AppendAllText($sample, "2,3`n")
Expect-Failure { Invoke-Archive 'Activate' } 'Archive manifest differs from numbered source'
[System.IO.File]::WriteAllText($sample, "AnimalNum,value`n1,2`n")
[System.IO.File]::WriteAllText($extra, 'written after prepare')
Expect-Failure { Invoke-Archive 'Activate' } 'file count or uniqueness differs'
Remove-Item -LiteralPath $extra
if ((Receipt-State) -cne 'prepared') { throw 'Refused activation changed the receipt state' }

# A changed review gate or queue cannot be used to activate a prepared plan.
$oldGateHash = $gateHash
Write-Gate 'needs_reader_writer_review'
Expect-Failure { Invoke-Archive 'Activate' } 'does not resolve every queued script'
# A gate that is still valid but differs from the prepared one is refused.
Write-Gate 'ready' -Rows @($scripts[1], $scripts[0])
Expect-Failure { Invoke-Archive 'Activate' } 'Reader gate changed after Prepare'
Write-Gate 'ready'
if ($gateHash -cne $oldGateHash) { throw 'Fixture gate did not return to its original hash' }
$oldQueueHash = (Get-FileHash -LiteralPath $queue -Algorithm SHA256).Hash
Write-Queue 'changed_after_prepare'
Expect-Failure { Invoke-Archive 'Activate' } 'Reader queue changed after Prepare'
Write-Queue
if ((Get-FileHash -LiteralPath $queue -Algorithm SHA256).Hash -cne $oldQueueHash) {
  throw 'Fixture queue did not return to its original hash'
}

# A destination created after Prepare, or a reparse point in the archive
# path, stops before the receipt changes. No directory is created through
# the reparse point.
New-Item -ItemType Directory -Path $archive -Force | Out-Null
Expect-Failure { Invoke-Archive 'Activate' } 'Archive destination already exists'
Remove-Item -LiteralPath $history -Recurse
$junctionTarget = Join-Path $fixture 'junction_target'
New-Item -ItemType Directory -Path $junctionTarget -Force | Out-Null
New-Item -ItemType Junction -Path $history -Target $junctionTarget | Out-Null
Expect-Failure { Invoke-Archive 'Activate' } 'reparse point'
if (@(Get-ChildItem -LiteralPath $junctionTarget -Force).Count -ne 0) {
  throw 'Activation created a directory through a reparse point'
}
[System.IO.Directory]::Delete($history)
if ((Receipt-State) -cne 'prepared') { throw 'Refused activation changed the receipt state' }
Assert-Source-Intact

# A file held open during the move must not split the root across both
# locations. The receipt stays transferring, readers fail closed, and
# Rollback restores prepared after rehashing the untouched original.
$lock = [System.IO.File]::Open($second, 'Open', 'Read', 'Read')
try {
  Expect-Failure { Invoke-Archive 'Activate' } 'denied|being used'
} finally {
  $lock.Dispose()
}
if ((Receipt-State) -cne 'transferring') { throw 'Failed move did not leave the receipt transferring' }
Assert-Source-Intact
Assert-R-Rejects
Expect-Failure { Invoke-Archive 'Inspect' } 'Archive needs recovery: transferring'
Expect-Failure { Invoke-Archive 'Verify' } 'Archive needs recovery: transferring'
if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Rollback after a failed move did not restore prepared'
}
Assert-Source-Intact

if ((Invoke-Archive 'Activate').state -cne 'activated' -or
    (Test-Path -LiteralPath $source) -or
    -not (Test-Path -LiteralPath $archive) -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive activation did not preserve the fixture inventory'
}
Assert-R-Routes $archive
Expect-Failure { Invoke-Archive 'Activate' } 'Activate requires a prepared archive receipt'
New-Item -ItemType Directory -Path $source -Force | Out-Null
Expect-Failure { Invoke-Archive 'Verify' } 'recreated original root'
Expect-Failure { Invoke-Archive 'Inspect' } 'recreated original root'
Assert-R-Rejects
Move-Item -LiteralPath $source -Destination (Join-Path $fixture 'recreated_root')

# Invalid receipts fail closed. Byte totals above 2 GiB (the live 03 and 06
# roots) are compared as integers instead of failing to parse.
$receiptText = Get-Content -LiteralPath $receiptPath -Raw
$receipt = $receiptText | ConvertFrom-Json
$receipt.manifest_sha256 = 'e' * 64
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Expect-Failure { Invoke-Archive 'Inspect' } 'Archive receipt pins manifest e{64}, not'
$receipt = $receiptText | ConvertFrom-Json
$receipt.state = 'unknown'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Expect-Failure { Invoke-Archive 'Inspect' } 'Invalid numbered-root archive receipt'
$receipt = $receiptText | ConvertFrom-Json
$receipt.files = '2'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Expect-Failure { Invoke-Archive 'Inspect' } 'Invalid numbered-root archive receipt'
Set-Content -LiteralPath $receiptPath -Value '{' -Encoding utf8
Expect-Failure { Invoke-Archive 'Inspect' } 'Invalid numbered-root archive receipt'
$receipt = $receiptText | ConvertFrom-Json
$receipt.PSObject.Properties.Remove('reader_gate_kind')
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Expect-Failure { Invoke-Archive 'Inspect' } 'Invalid numbered-root archive receipt'
$receipt = $receiptText | ConvertFrom-Json
$receipt.bytes = 18194653380
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Expect-Failure { Invoke-Archive 'Verify' } 'Archive receipt inventory differs'
Assert-R-Routes $archive
[System.IO.File]::WriteAllText($receiptPath, $receiptText, [System.Text.UTF8Encoding]::new($false))

if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source) -or
    (Test-Path -LiteralPath $archive) -or
    (Invoke-Archive 'Verify').hashes -cne 'PASS') {
  throw 'Archive rollback did not restore the fixture inventory'
}
Assert-R-Routes $source

# Interrupted before the directory move: the original remains and Rollback
# can return the receipt to prepared after a fresh hash check.
$receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
$receipt.state = 'transferring'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source)) {
  throw 'Pre-move interruption did not recover'
}

# Interrupted after the directory move: the archive is verified and moved
# back. Both locations present makes Rollback fail closed.
$receipt.state = 'transferring'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
New-Item -ItemType Directory -Path (Split-Path -Parent $archive) -Force | Out-Null
Move-Item -LiteralPath $source -Destination $archive
New-Item -ItemType Directory -Path $source -Force | Out-Null
Expect-Failure { Invoke-Archive 'Rollback' } 'both or neither'
Remove-Item -LiteralPath $source
if ((Invoke-Archive 'Rollback').state -cne 'prepared' -or
    -not (Test-Path -LiteralPath $source) -or (Test-Path -LiteralPath $archive)) {
  throw 'Post-move interruption did not recover'
}

# Drift inside the archive after the move (for example a new thumbnail cache)
# makes the normal Rollback refuse and readers fail closed. The explicit drift
# recovery renames the root back without trusting it; Abandon then releases
# the unverified original for review.
$receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
$receipt.state = 'transferring'
$receipt | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $receiptPath -Encoding utf8
Move-Item -LiteralPath $source -Destination $archive
$drift = Join-Path $archive 'dyadic_contacts\Thumbs.db'
[System.IO.File]::WriteAllText($drift, 'thumbnail cache')
Expect-Failure { Invoke-Archive 'Rollback' } 'file count or uniqueness differs'
if ((Receipt-State) -cne 'transferring' -or (Test-Path -LiteralPath $source) -or
    -not (Test-Path -LiteralPath $drift)) {
  throw 'Refused drift rollback changed the archive state'
}
Assert-R-Rejects
Expect-Failure { Invoke-Archive 'Abandon' } 'Abandon requires a prepared receipt'
& $tool -Action Rollback -AnalysisReadyRoot $ready -RootName '06_behavioral_dynamics' `
  -Manifest $manifest -ManifestSha256 $manifestHash -AcceptInventoryDrift | Out-Null
if ((Receipt-State) -cne 'transferring' -or -not (Test-Path -LiteralPath $source) -or
    (Test-Path -LiteralPath $archive) -or
    -not ((Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json).PSObject.Properties.Name -contains 'drift_detected_at_utc')) {
  throw 'Drift recovery did not return the root with a transferring receipt'
}
Assert-R-Rejects
Expect-Failure { Invoke-Archive 'Rollback' } 'file count or uniqueness differs'
& $tool -Action Abandon -AnalysisReadyRoot $ready -RootName '06_behavioral_dynamics' `
  -Manifest $manifest -ManifestSha256 $manifestHash -AbandonReason 'fixture drift' | Out-Null
if (Test-Path -LiteralPath $receiptPath) { throw 'Abandon after drift recovery kept the receipt' }
Assert-R-Writers $false
Remove-Item -LiteralPath (Join-Path $source 'dyadic_contacts\Thumbs.db')
if ((Invoke-Archive 'Prepare').state -cne 'prepared') { throw 'Prepare after drift recovery failed' }

# The path-and-writer gate is an explicit alternative. It cannot be confused
# with replay-ready, and the selected kind is pinned through activation.
Remove-Item -LiteralPath $receiptPath
Write-Gate 'archive_path_ready'
Expect-Failure { Invoke-Archive 'Prepare' } 'does not resolve every queued script'
Expect-Failure { Invoke-Archive 'Prepare' 'ArchivePath' } 'requires path and writer review evidence'
Write-Gate 'archive_path_ready' -Evidence
Expect-Failure { Invoke-Archive 'Prepare' } 'does not resolve every queued script'
if ((Invoke-Archive 'Prepare' 'ArchivePath').state -cne 'prepared') {
  throw 'Explicit archive path gate did not prepare'
}
$pathReceipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
if ($pathReceipt.reader_gate_kind -cne 'ArchivePath') {
  throw 'Archive path gate kind was not recorded in the receipt'
}
Expect-Failure { Invoke-Archive 'Activate' } 'Reader gate kind changed after Prepare'
if ((Invoke-Archive 'Activate' 'ArchivePath').state -cne 'activated' -or
    (Invoke-Archive 'Rollback' 'ArchivePath').state -cne 'prepared') {
  throw 'Explicit archive path transaction did not complete and roll back'
}
Remove-Item -LiteralPath $fixture -Recurse -Force
Write-Output 'Numbered root archive transaction fixture: PASS'
