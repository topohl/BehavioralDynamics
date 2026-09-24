Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\New-BehaviorArchiveReaderGateTemplate.ps1'
$file = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-reader-gate-' + [guid]::NewGuid().ToString('N') + '.csv')
$queueCopy = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-reader-queue-' + [guid]::NewGuid().ToString('N') + '.csv')
$resetFile = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-reader-gate-reset-' + [guid]::NewGuid().ToString('N') + '.csv')
try {
  $result = & $tool -Output $file
  $rows = @(Import-Csv -LiteralPath $file)
  if ($result.scripts -ne 37 -or $result.ready -ne 0 -or $rows.Count -ne 37 -or
      @($rows | Where-Object { $_.script_sha256 -cnotmatch '^[0-9a-f]{64}$' }).Count -ne 0 -or
      @($rows | Where-Object { $_.review_state -cne 'needs_reader_writer_review' }).Count -ne 0 -or
      -not ($rows[0].PSObject.Properties.Name -contains 'queue_state')) {
    throw 'Reader gate template marked an unreviewed audit ready or omitted hashes'
  }
  try {
    & $tool -Output $file | Out-Null
    throw 'Expected existing template refusal'
  } catch {
    if ($_.Exception.Message -ceq 'Expected existing template refusal') { throw }
  }
  $queue = @(Import-Csv -LiteralPath 'docs/behavior_output_archive_audit_script_queue.csv')
  $queue[0].review_state = 'ready'
  $queue | Export-Csv -LiteralPath $queueCopy -NoTypeInformation -Encoding utf8
  & $tool -Output $resetFile -ReaderQueue $queueCopy | Out-Null
  $reset = @(Import-Csv -LiteralPath $resetFile)
  if ($reset[0].review_state -cne 'needs_reader_writer_review' -or
      $reset[0].queue_state -cne 'ready') {
    throw 'Regenerated gate silently carried a ready state forward'
  }
  Write-Output 'Behavior archive reader gate template: PASS'
} finally {
  if (Test-Path -LiteralPath $file) { Remove-Item -LiteralPath $file }
  if (Test-Path -LiteralPath $queueCopy) { Remove-Item -LiteralPath $queueCopy }
  if (Test-Path -LiteralPath $resetFile) { Remove-Item -LiteralPath $resetFile }
}
