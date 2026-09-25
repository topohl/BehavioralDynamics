#Requires -Version 7.2
param(
  [Parameter(Mandatory = $true)]
  [ValidateSet('Build', 'Verify')]
  [string] $Action,
  [Parameter(Mandatory = $true)] [string] $AnalysisReadyRoot,
  [Parameter(Mandatory = $true)]
  [ValidateSet('03_derived_metrics', '06_behavioral_dynamics',
               '12_systems_neuroscience_summary')]
  [string] $RootName,
  [Parameter(Mandatory = $true)] [string] $Manifest,
  [ValidateSet('Original', 'Archived')]
  [string] $Location = 'Original'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Relative arguments resolve against the PowerShell location, not the
# process working directory.
function FullPath([string] $Path) {
  [System.IO.Path]::GetFullPath(
    $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path)
  ).TrimEnd('\', '/')
}

function IsChild([string] $Path, [string] $Parent) {
  $Path.StartsWith($Parent + [System.IO.Path]::DirectorySeparatorChar,
                   [System.StringComparison]::OrdinalIgnoreCase)
}

function Sha256([string] $Path) {
  (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
}

$ready = FullPath $AnalysisReadyRoot
$original = FullPath (Join-Path $ready $RootName)
$archived = FullPath (Join-Path $ready ("history\original_layout\$RootName"))
$source = if ($Location -ceq 'Original') { $original } else { $archived }
$manifestPath = FullPath $Manifest
if ($Action -ceq 'Build' -and $Location -cne 'Original') {
  throw 'Build is allowed only from the original numbered source'
}
if (-not (Test-Path -LiteralPath $ready -PathType Container) -or
    -not (Test-Path -LiteralPath $source -PathType Container)) {
  throw "Missing analysis_ready or numbered source root: $source"
}
if (((Get-Item -LiteralPath $source -Force).Attributes -band
      [System.IO.FileAttributes]::ReparsePoint) -ne 0) {
  throw "Numbered source is a reparse point: $source"
}
if ($Location -ceq 'Archived' -and (Test-Path -LiteralPath $original)) {
  throw "Archived verification found a recreated original root: $original"
}
if ((IsChild $manifestPath $source) -or $manifestPath -ieq $source) {
  throw 'The manifest cannot be written inside the source being inventoried'
}

$entries = @(Get-ChildItem -LiteralPath $source -Recurse -Force)
if (@($entries | Where-Object {
      ($_.Attributes -band [System.IO.FileAttributes]::ReparsePoint) -ne 0
    }).Count -gt 0) {
  throw "Numbered source contains a reparse point: $source"
}
$files = @($entries | Where-Object { -not $_.PSIsContainer })
if ($files.Count -eq 0) { throw "Numbered source contains no files: $source" }
$rows = @($files | ForEach-Object {
  $relative = $_.FullName.Substring($source.Length + 1).Replace('\', '/')
  [pscustomobject][ordered]@{
    relative_path = $relative
    size_bytes = [long]$_.Length
    sha256 = Sha256 $_.FullName
  }
})
# Ordinal order does not depend on the culture or PowerShell edition.
[string[]] $keys = @($rows | ForEach-Object relative_path)
[object[]] $rows = $rows
[Array]::Sort($keys, $rows, [System.StringComparer]::Ordinal)
$seen = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::Ordinal)
foreach ($key in $keys) {
  if (-not $seen.Add($key)) { throw 'Numbered source has duplicate relative paths' }
}

if ($Action -ceq 'Build') {
  if (Test-Path -LiteralPath $manifestPath) {
    throw "Manifest already exists: $manifestPath"
  }
  $parent = Split-Path -Parent $manifestPath
  if (-not (Test-Path -LiteralPath $parent -PathType Container)) {
    throw "Manifest parent does not exist: $parent"
  }
  $lines = @($rows | ConvertTo-Csv -NoTypeInformation)
  [System.IO.File]::WriteAllLines(
    $manifestPath, $lines, [System.Text.UTF8Encoding]::new($false))
  $manifestHash = Sha256 $manifestPath
} else {
  if (-not (Test-Path -LiteralPath $manifestPath -PathType Leaf)) {
    throw "Missing archive manifest: $manifestPath"
  }
  # Hash and parse the same bytes, so the returned hash describes what was
  # compared.
  $bytes = [System.IO.File]::ReadAllBytes($manifestPath)
  $manifestHash = [Convert]::ToHexString(
    [System.Security.Cryptography.SHA256]::HashData($bytes)).ToLowerInvariant()
  $expected = @([System.Text.Encoding]::UTF8.GetString($bytes).TrimStart([char]0xFEFF) |
    ConvertFrom-Csv)
  if ($expected.Count -eq 0) { throw 'Archive manifest is empty' }
  $missingColumns = @(@('relative_path', 'size_bytes', 'sha256') |
    Where-Object { -not ($expected[0].PSObject.Properties.Name -contains $_) })
  if ($missingColumns.Count -gt 0) {
    throw "Archive manifest is missing columns: $($missingColumns -join ', ')"
  }
  $byPath = [System.Collections.Generic.Dictionary[string, object]]::new(
    [System.StringComparer]::Ordinal)
  foreach ($entry in $expected) {
    $relative = [string]$entry.relative_path
    if ([string]::IsNullOrWhiteSpace($relative) -or
        $relative.StartsWith('/') -or $relative.Contains('\') -or
        $relative -match '(^|/)\.\.?(/|$)' -or
        $entry.sha256 -cnotmatch '^[0-9a-f]{64}$' -or
        $entry.size_bytes -notmatch '^[0-9]+$') {
      throw "Invalid archive manifest row: $relative"
    }
    if ($byPath.ContainsKey($relative)) {
      throw 'Archive manifest file count or uniqueness differs from source'
    }
    $byPath.Add($relative, $entry)
  }
  if ($byPath.Count -ne $rows.Count) {
    throw 'Archive manifest file count or uniqueness differs from source'
  }
  # Compare by exact relative path, so a case-only rename still fails.
  foreach ($row in $rows) {
    $entry = $null
    if (-not $byPath.TryGetValue($row.relative_path, [ref] $entry) -or
        [long]$entry.size_bytes -ne $row.size_bytes -or
        $entry.sha256 -cne $row.sha256) {
      throw "Archive manifest differs from numbered source: $($row.relative_path)"
    }
  }
}

[pscustomobject]@{
  action = $Action
  root = $RootName
  location = $Location
  files = $rows.Count
  bytes = [long](($rows | Measure-Object -Property size_bytes -Sum).Sum)
  manifest = $manifestPath
  manifest_sha256 = $manifestHash
  hashes = 'PASS'
}
