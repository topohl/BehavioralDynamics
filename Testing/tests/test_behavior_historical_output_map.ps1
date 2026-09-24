$ErrorActionPreference = 'Stop'
$validator = Join-Path $PSScriptRoot '..\..\Maintenance\Test-BehaviorHistoricalOutputMap.ps1'
$temporaryRoot = [System.IO.Path]::GetFullPath([System.IO.Path]::GetTempPath())
$sandbox = Join-Path $temporaryRoot ('mmm_history_map_test_' + [guid]::NewGuid().ToString('N'))
$root = Join-Path $sandbox 'analysis_ready'
$map = Join-Path $sandbox 'map.csv'

function Expect-Failure([scriptblock] $Block, [string] $Label) {
  $failed = $false
  try { & $Block | Out-Null } catch { $failed = $true }
  if (-not $failed) { throw "Expected historical-map failure: $Label" }
}

try {
  $sourceRoot = Join-Path $root '06_behavioral_dynamics\social_networks\10min_based'
  New-Item -ItemType Directory -Path (Join-Path $sourceRoot 'tables') -Force | Out-Null
  $files = @(
    (Join-Path $sourceRoot 'output_manifest.csv'),
    (Join-Path $sourceRoot 'tables\animal_features.csv')
  )
  [System.IO.File]::WriteAllText($files[0], 'manifest fixture')
  [System.IO.File]::WriteAllText($files[1], "AnimalNum,value`n1,2")
  $rows = @($files | ForEach-Object {
    $file = Get-Item -LiteralPath $_
    $suffix = $file.FullName.Substring($sourceRoot.Length + 1).Replace('\', '/')
    [pscustomobject]@{
      source_rel = "06_behavioral_dynamics/social_networks/10min_based/$suffix"
      proposed_target_rel = "history/social_networks/10min/$suffix"
      bytes = $file.Length
      last_write_utc = $file.LastWriteTimeUtc.ToString('o')
      sha256 = (Get-FileHash -LiteralPath $file.FullName -Algorithm SHA256).Hash.ToLowerInvariant()
    }
  })
  $rows | Export-Csv -LiteralPath $map -NoTypeInformation
  $result = & $validator -AnalysisReadyRoot $root -Map $map
  if ($result.result -ne 'PASS' -or $result.files -ne 2 -or
      $result.source_roots -ne 1 -or -not $result.targets_absent) {
    throw 'Historical-map fixture did not pass exact inventory validation'
  }

  $rows[0].proposed_target_rel = 'history/social_networks/5min/output_manifest.csv'
  $rows | Export-Csv -LiteralPath $map -NoTypeInformation
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'wrong resolution target'
  $rows[0].proposed_target_rel = 'history/social_networks/10min/output_manifest.csv'

  @($rows[0], $rows[0], $rows[1]) | Export-Csv -LiteralPath $map -NoTypeInformation
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'duplicate map row'
  $rows[0].proposed_target_rel = 'history/social_networks/10min/../output_manifest.csv'
  $rows | Export-Csv -LiteralPath $map -NoTypeInformation
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'unsafe target path'
  $rows[0].proposed_target_rel = 'history/social_networks/10min/output_manifest.csv'

  $cache = Join-Path $sourceRoot 'figures\Thumbs.db'
  New-Item -ItemType Directory -Path (Split-Path -Parent $cache) -Force | Out-Null
  [System.IO.File]::WriteAllText($cache, 'Windows cache fixture')
  $rows | Export-Csv -LiteralPath $map -NoTypeInformation
  if ((& $validator -AnalysisReadyRoot $root -Map $map).result -ne 'PASS') {
    throw 'Thumbs.db cache should not affect the scientific inventory'
  }

  $extra = Join-Path $sourceRoot 'tables\unplanned.csv'
  [System.IO.File]::WriteAllText($extra, 'unplanned')
  $rows | Export-Csv -LiteralPath $map -NoTypeInformation
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'unplanned source file'
  Remove-Item -LiteralPath $extra

  $target = Join-Path $root 'history\social_networks\10min\output_manifest.csv'
  New-Item -ItemType Directory -Path (Split-Path -Parent $target) -Force | Out-Null
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'existing empty destination root'
  [System.IO.File]::WriteAllText($target, 'existing')
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'existing target'
  Remove-Item -LiteralPath $target

  [System.IO.File]::AppendAllText($files[0], 'changed')
  Expect-Failure { & $validator -AnalysisReadyRoot $root -Map $map } 'changed source'
  Write-Output 'Historical output map read-only validation fixture: PASS'
} finally {
  $safe = [System.IO.Path]::GetFullPath($sandbox)
  if ($safe.StartsWith($temporaryRoot, [System.StringComparison]::OrdinalIgnoreCase) -and
      (Split-Path -Leaf $safe) -like 'mmm_history_map_test_*' -and
      (Test-Path -LiteralPath $safe)) {
    Remove-Item -LiteralPath $safe -Recurse -Force
  }
}
