Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\New-BehaviorArchiveReaderGateTemplate.ps1'
$repo = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..'))
. (Join-Path $repo 'Maintenance\BehaviorArchiveSharedCode.ps1')
$liveQueue = Join-Path $repo 'docs\behavior_output_archive_audit_script_queue.csv'
$work = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-reader-gate-' + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $work | Out-Null
function Expect-Failure([scriptblock] $Block, [string] $Pattern) {
  try { & $Block | Out-Null } catch {
    if ($_.Exception.Message -notmatch $Pattern) {
      throw "Unexpected template rejection (expected /$Pattern/): $($_.Exception.Message)"
    }
    return
  }
  throw "Expected template rejection: $Pattern"
}
try {
  # Relative paths resolve against the PowerShell location, and the default
  # queue is the repository queue wherever the tool is called from.
  Push-Location $work
  try { $result = & $tool -Output 'template.csv' -RootName '03_derived_metrics' } finally { Pop-Location }
  $file = Join-Path $work 'template.csv'
  $rows = @(Import-Csv -LiteralPath $file)
  $queue = @(Import-Csv -LiteralPath $liveQueue)
  $queueHash = (Get-FileHash -LiteralPath $liveQueue -Algorithm SHA256).Hash.ToLowerInvariant()
  if ($result.scripts -ne $queue.Count -or $result.ready -ne 0 -or $rows.Count -ne $queue.Count -or
      (Compare-Object @($rows.script) @($queue.script)) -or
      @($rows | Where-Object { $_.script_sha256 -cnotmatch '^[0-9a-f]{64}$' }).Count -ne 0 -or
      @($rows | Where-Object { $_.review_state -cne 'needs_reader_writer_review' }).Count -ne 0 -or
      @($rows | Where-Object { $_.archive_root -cne '03_derived_metrics' }).Count -ne 0 -or
      @($rows | Where-Object { $_.queue_sha256 -cne $queueHash }).Count -ne 0 -or
      @($rows | Where-Object { $_.shared_code_sha256 -cne (Get-BehaviorArchiveSharedCodeSha256 $repo) }).Count -ne 0 -or
      -not ($rows[0].PSObject.Properties.Name -contains 'queue_state') -or
      -not ($rows[0].PSObject.Properties.Name -contains 'path_review_evidence') -or
      -not ($rows[0].PSObject.Properties.Name -contains 'writer_review_evidence')) {
    throw 'Reader gate template marked an unreviewed audit ready or omitted hashes'
  }
  Expect-Failure { & $tool -Output $file } 'output exists'
  Expect-Failure { & $tool -Output (Join-Path $work 'bad-root.csv') -RootName 'history' } 'RootName'

  # A regenerated template never carries a ready state forward, and the queue
  # length is not fixed: a newly queued reader is accepted.
  $queue[0].review_state = 'ready'
  $longer = @($queue) + [pscustomobject]@{
    script = 'Testing/audits/audit_stage10_spatial_feature_boundary.R'
    review_state = 'path_prepared_unvalidated' }
  $missing = @($queue) + [pscustomobject]@{ script = $queue[0].script.Replace('.R', '_copy.R');
                                             review_state = 'path_prepared_unvalidated' }
  $queueCopy = Join-Path $work 'queue.csv'
  $queue | Export-Csv -LiteralPath $queueCopy -NoTypeInformation -Encoding utf8
  & $tool -Output (Join-Path $work 'reset.csv') -ReaderQueue $queueCopy | Out-Null
  $reset = @(Import-Csv -LiteralPath (Join-Path $work 'reset.csv'))
  if ($reset[0].review_state -cne 'needs_reader_writer_review' -or
      $reset[0].queue_state -cne 'ready' -or $reset[0].archive_root -cne '') {
    throw 'Regenerated gate silently carried a ready state forward'
  }
  $longer | Export-Csv -LiteralPath $queueCopy -NoTypeInformation -Encoding utf8
  if ((& $tool -Output (Join-Path $work 'longer.csv') -ReaderQueue $queueCopy).scripts -ne $queue.Count + 1) {
    throw 'A longer reader queue was not accepted'
  }
  $missing | Export-Csv -LiteralPath $queueCopy -NoTypeInformation -Encoding utf8
  Expect-Failure { & $tool -Output (Join-Path $work 'missing.csv') -ReaderQueue $queueCopy } 'Queued audit script is missing'
  Write-Output 'Behavior archive reader gate template: PASS'
} finally {
  Remove-Item -LiteralPath $work -Recurse -Force
}
