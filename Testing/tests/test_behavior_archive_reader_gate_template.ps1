Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$tool = Join-Path $PSScriptRoot '..\..\Maintenance\New-BehaviorArchiveReaderGateTemplate.ps1'
$file = Join-Path ([System.IO.Path]::GetTempPath()) (
  'behavior-reader-gate-' + [guid]::NewGuid().ToString('N') + '.csv')
try {
  $result = & $tool -Output $file
  $rows = @(Import-Csv -LiteralPath $file)
  if ($result.scripts -ne 37 -or $result.ready -ne 0 -or $rows.Count -ne 37 -or
      @($rows | Where-Object { $_.script_sha256 -cnotmatch '^[0-9a-f]{64}$' }).Count -ne 0 -or
      @($rows | Where-Object { $_.review_state -ceq 'ready' }).Count -ne 0) {
    throw 'Reader gate template marked an unreviewed audit ready or omitted hashes'
  }
  try {
    & $tool -Output $file | Out-Null
    throw 'Expected existing template refusal'
  } catch {
    if ($_.Exception.Message -ceq 'Expected existing template refusal') { throw }
  }
  Write-Output 'Behavior archive reader gate template: PASS'
} finally {
  if (Test-Path -LiteralPath $file) { Remove-Item -LiteralPath $file }
}
