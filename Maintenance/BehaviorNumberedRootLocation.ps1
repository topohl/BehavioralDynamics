# Dot-source to find where a numbered behavioral root's retained original is.
# Mirrors mmm_behavior_retained_source_root() in Functions/project_paths.R:
# the root archive receipt selects the location, never directory existence
# alone, and every intermediate or inconsistent state stops the caller.
$BehaviorNumberedRoots = @('03_derived_metrics', '06_behavioral_dynamics',
                           '12_systems_neuroscience_summary')

function Resolve-BehaviorNumberedRoot([string] $Ready, [string] $RootName) {
  if ($RootName -cnotin $BehaviorNumberedRoots) {
    throw "Unknown numbered behavioral root: $RootName"
  }
  $original = Join-Path $Ready $RootName
  $archived = Join-Path $Ready "history\original_layout\$RootName"
  $receipt = Join-Path $Ready "_migration_control\numbered_root_archive\$RootName.json"
  if (-not (Test-Path -LiteralPath $receipt -PathType Leaf)) {
    if (Test-Path -LiteralPath $archived) {
      throw "Numbered source archive exists without a receipt for $RootName"
    }
    return $original
  }
  $record = try { Get-Content -LiteralPath $receipt -Raw | ConvertFrom-Json } catch { $null }
  if ($null -eq $record -or $record -isnot [pscustomobject] -or
      @('root', 'source_root_rel', 'archive_root_rel', 'state' | Where-Object {
        -not ($record.PSObject.Properties.Name -contains $_) -or $record.$_ -isnot [string]
      }).Count -gt 0 -or
      $record.root -cne $RootName -or $record.source_root_rel -cne $RootName -or
      $record.archive_root_rel -cne "history/original_layout/$RootName") {
    throw "Invalid numbered-root archive receipt: $receipt"
  }
  if ($record.state -ceq 'prepared') {
    if (-not (Test-Path -LiteralPath $original -PathType Container) -or
        (Test-Path -LiteralPath $archived)) {
      throw "Prepared numbered source archive has unexpected locations for $RootName"
    }
    return $original
  }
  if ($record.state -ceq 'activated') {
    if ((Test-Path -LiteralPath $original) -or
        -not (Test-Path -LiteralPath $archived -PathType Container)) {
      throw "Activated numbered source archive has unexpected locations for $RootName"
    }
    return $archived
  }
  throw "Numbered source archive is not readable for ${RootName}: $($record.state)"
}

# Map a recorded analysis_ready-relative source path to its retained location.
# Recorded paths stay provenance; only the file that is read moves.
function Resolve-BehaviorRetainedPath([string] $Ready, [string] $SourceRel) {
  $parts = $SourceRel.Replace('\', '/').Split('/', 2)
  if ($parts.Count -eq 2 -and $parts[0] -cin $BehaviorNumberedRoots) {
    return Join-Path (Resolve-BehaviorNumberedRoot $Ready $parts[0]) ($parts[1].Replace('/', '\'))
  }
  Join-Path $Ready ($SourceRel.Replace('/', '\'))
}
