param(
  [Parameter(Mandatory = $true)] [string] $Output,
  [string] $ReaderQueue = 'docs/behavior_output_archive_audit_script_queue.csv'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$repo = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$queuePath = [System.IO.Path]::GetFullPath($ReaderQueue)
$outputPath = [System.IO.Path]::GetFullPath($Output)
if (-not (Test-Path -LiteralPath $queuePath -PathType Leaf) -or
    (Test-Path -LiteralPath $outputPath) -or
    -not (Test-Path -LiteralPath (Split-Path -Parent $outputPath) -PathType Container)) {
  throw 'Reader queue is missing, output exists, or output parent is missing'
}
$queue = @(Import-Csv -LiteralPath $queuePath)
if ($queue.Count -ne 37 -or
    -not ($queue[0].PSObject.Properties.Name -contains 'script') -or
    -not ($queue[0].PSObject.Properties.Name -contains 'review_state') -or
    @($queue | Select-Object -ExpandProperty script -Unique).Count -ne $queue.Count) {
  throw 'Reader queue must contain 37 unique audit scripts'
}
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
    script_sha256 = (Get-FileHash -LiteralPath $scriptPath -Algorithm SHA256).Hash.ToLowerInvariant()
    queue_state = $entry.review_state
  }
}
$rows | Export-Csv -LiteralPath $outputPath -NoTypeInformation -Encoding utf8
[pscustomobject]@{
  template = $outputPath
  scripts = $rows.Count
  ready = @($rows | Where-Object { $_.review_state -ceq 'ready' }).Count
  sha256 = (Get-FileHash -LiteralPath $outputPath -Algorithm SHA256).Hash.ToLowerInvariant()
}
