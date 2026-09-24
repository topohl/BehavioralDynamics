param(
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [Parameter(Mandatory = $true)] [string] $Map
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function ChildPath([string] $Root, [string] $Relative) {
  if ([string]::IsNullOrWhiteSpace($Relative) -or
      [System.IO.Path]::IsPathRooted($Relative) -or
      $Relative -match '(^|[\\/])\.{1,2}([\\/]|$)') {
    throw "Unsafe relative path: $Relative"
  }
  $rootFull = [System.IO.Path]::GetFullPath($Root).TrimEnd('\', '/')
  $child = [System.IO.Path]::GetFullPath((Join-Path $rootFull $Relative))
  if (-not $child.StartsWith($rootFull + [System.IO.Path]::DirectorySeparatorChar,
                            [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Path escapes analysis_ready: $Relative"
  }
  return $child
}

$root = [System.IO.Path]::GetFullPath($AnalysisReadyRoot).TrimEnd('\', '/')
if (-not (Test-Path -LiteralPath $root -PathType Container)) {
  throw "Missing analysis_ready root: $root"
}
if (-not (Test-Path -LiteralPath $Map -PathType Leaf)) { throw "Missing map: $Map" }
$rows = @(Import-Csv -LiteralPath $Map)
if ($rows.Count -eq 0) { throw 'Historical map is empty' }
foreach ($column in @('source_rel', 'proposed_target_rel', 'bytes',
                      'last_write_utc', 'sha256')) {
  if (-not ($rows[0].PSObject.Properties.Name -contains $column)) {
    throw "Historical map is missing column: $column"
  }
}

$allowed = @{
  social_networks = @('10sec_based', '1min_based', '10min_based', '30min_based')
  state_space = @('1min_based', '10min_based')
  temporal_instability = @('1min_based', '5min_based')
  gamm_features = @('30min_based')
}
$sources = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
$targets = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
$roots = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
$totalBytes = [int64]0
foreach ($row in $rows) {
  $sourceRel = $row.source_rel.Replace('\', '/')
  $targetRel = $row.proposed_target_rel.Replace('\', '/')
  $parts = $sourceRel.Split('/')
  if ($parts.Count -lt 4 -or $parts[0] -cne '06_behavioral_dynamics' -or
      -not $allowed.ContainsKey($parts[1]) -or
      $parts[2] -cnotin $allowed[$parts[1]]) {
    throw "Unexpected historical source root: $sourceRel"
  }
  $sourceRoot = ($parts[0..2] -join '/')
  $expectedTarget = 'history/' + $parts[1] + '/' +
    $parts[2].Substring(0, $parts[2].Length - '_based'.Length) + '/' +
    ($parts[3..($parts.Count - 1)] -join '/')
  if ($targetRel -cne $expectedTarget) {
    throw "Historical target is not the exact semantic counterpart: $targetRel"
  }
  $source = ChildPath $root $sourceRel
  $target = ChildPath $root $targetRel
  $targetRoot = ChildPath $root ('history/' + $parts[1] + '/' +
    $parts[2].Substring(0, $parts[2].Length - '_based'.Length))
  if (-not $sources.Add($source) -or -not $targets.Add($target)) {
    throw "Duplicate historical source or destination: $sourceRel"
  }
  [void]$roots.Add($sourceRoot)
  if (-not (Test-Path -LiteralPath $source -PathType Leaf)) { throw "Missing source: $source" }
  if (Test-Path -LiteralPath $targetRoot) {
    throw "Historical destination root already exists: $targetRoot"
  }
  $file = Get-Item -LiteralPath $source
  [int64]$expectedBytes = 0
  if (-not [int64]::TryParse($row.bytes, [ref]$expectedBytes) -or
      $file.Length -ne $expectedBytes) {
    throw "Source size changed: $source"
  }
  $expectedTime = [datetime]::MinValue
  if (-not [datetime]::TryParse($row.last_write_utc,
       [System.Globalization.CultureInfo]::InvariantCulture,
       [System.Globalization.DateTimeStyles]::RoundtripKind,
       [ref]$expectedTime) -or $file.LastWriteTimeUtc -ne $expectedTime.ToUniversalTime()) {
    throw "Source modification time changed: $source"
  }
  if ($row.sha256 -cnotmatch '^[0-9a-f]{64}$' -or
      (Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash.ToLowerInvariant() -cne $row.sha256) {
    throw "Source SHA-256 changed: $source"
  }
  $totalBytes += $file.Length
}

foreach ($sourceRoot in $roots) {
  $directory = ChildPath $root $sourceRoot
  $actual = @(Get-ChildItem -LiteralPath $directory -Force -File -Recurse |
              Where-Object { $_.Name -ine 'Thumbs.db' })
  foreach ($file in $actual) {
    if (-not $sources.Contains($file.FullName)) {
      throw "Source tree has an unplanned file: $($file.FullName)"
    }
  }
}

[pscustomobject]@{
  result = 'PASS'
  source_roots = $roots.Count
  files = $rows.Count
  bytes = $totalBytes
  targets_absent = $true
  source_hashes = 'PASS'
}
