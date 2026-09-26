#Requires -Version 7.2
param(
  [Parameter(Mandatory = $true)] [string] $Output,
  [string] $ReaderQueue = '',
  # An ArchivePath review covers one numbered root's move; the archive tool
  # refuses a path gate whose rows name another root.
  [ValidateScript({ $_ -eq '' -or $(. (Join-Path $PSScriptRoot 'BehaviorNumberedRootLocation.ps1'); Test-BehaviorNumberedRootName $_) },
                  ErrorMessage = 'Unknown numbered behavioral root: {0}')]
  [string] $RootName = ''
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'BehaviorArchiveSharedCode.ps1')
function FullPath([string] $Path) {
  [System.IO.Path]::GetFullPath(
    $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path))
}
$repo = FullPath (Join-Path $PSScriptRoot '..')
$queuePath = if ([string]::IsNullOrWhiteSpace($ReaderQueue)) {
  Join-Path $repo 'docs\behavior_output_archive_audit_script_queue.csv'
} else { FullPath $ReaderQueue }
$outputPath = FullPath $Output
if (-not (Test-Path -LiteralPath $queuePath -PathType Leaf) -or
    (Test-Path -LiteralPath $outputPath) -or
    -not (Test-Path -LiteralPath (Split-Path -Parent $outputPath) -PathType Container)) {
  throw 'Reader queue is missing, output exists, or output parent is missing'
}
$queue = @(Import-Csv -LiteralPath $queuePath)
if ($queue.Count -eq 0 -or
    -not ($queue[0].PSObject.Properties.Name -contains 'script') -or
    -not ($queue[0].PSObject.Properties.Name -contains 'review_state') -or
    @($queue | Select-Object -ExpandProperty script -Unique).Count -ne $queue.Count) {
  throw 'Reader queue must contain unique audit scripts'
}
# Text hashes normalize line endings (see BehaviorArchiveSharedCode.ps1).
$queueHash = Get-BehaviorArchiveTextSha256 $queuePath
$sharedHash = Get-BehaviorArchiveSharedCodeSha256 $repo
$rows = foreach ($entry in $queue) {
  if ($entry.script -cnotmatch '^Testing/audits/[A-Za-z0-9_.-]+\.R$') {
    throw "Unsafe queued audit script: $($entry.script)"
  }
  $scriptPath = [System.IO.Path]::GetFullPath(
    (Join-Path $repo ($entry.script -replace '/', '\')))
  if (-not (Test-Path -LiteralPath $scriptPath -PathType Leaf)) {
    throw "Queued audit script is missing: $($entry.script)"
  }
  [pscustomobject]@{
    script = $entry.script
    review_state = 'needs_reader_writer_review'
    script_sha256 = Get-BehaviorArchiveTextSha256 $scriptPath
    queue_state = $entry.review_state
    archive_root = $RootName
    queue_sha256 = $queueHash
    shared_code_sha256 = $sharedHash
    path_review_evidence = ''
    writer_review_evidence = ''
  }
}
$rows | Export-Csv -LiteralPath $outputPath -NoTypeInformation -Encoding utf8
[pscustomobject]@{
  template = $outputPath
  scripts = $rows.Count
  ready = @($rows | Where-Object { $_.review_state -ceq 'ready' }).Count
  queue_sha256 = $queueHash
  shared_code_sha256 = $sharedHash
  sha256 = Get-BehaviorArchiveTextSha256 $outputPath
}
