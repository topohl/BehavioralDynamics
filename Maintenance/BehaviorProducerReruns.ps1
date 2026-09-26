# Dot-source to follow reviewed producer reruns of activated output groups.
# An activated copy starts as a byte-identical snapshot of its original. A
# producer that later reruns into the semantic folder changes it, and the
# retained original stays as it was. docs/behavior_output_producer_reruns/
# holds one CSV per reviewed rerun, applied in file-name order, with columns
# group, target_root_rel, target_file, prior_sha256 ('' for an added file),
# sha256 and change.

function Import-BehaviorProducerReruns([string] $RerunDir) {
  $map = @{}
  if (-not (Test-Path -LiteralPath $RerunDir -PathType Container)) { return $map }
  foreach ($f in @(Get-ChildItem -LiteralPath $RerunDir -Filter '*.csv' -File | Sort-Object Name)) {
    foreach ($r in @(Import-Csv -LiteralPath $f.FullName)) {
      if (-not $r.group -or -not $r.target_file -or $r.sha256 -notmatch '^[0-9a-fA-F]{64}$' -or
          ($r.prior_sha256 -and $r.prior_sha256 -notmatch '^[0-9a-fA-F]{64}$')) {
        throw "Malformed producer rerun record in $($f.Name)"
      }
      $key = "$($r.group)|$($r.target_file)".ToLowerInvariant()
      if (-not $map.ContainsKey($key)) { $map[$key] = [System.Collections.Generic.List[object]]::new() }
      $map[$key].Add($r)
    }
  }
  $map
}

# The hash a target file must have now: its activation hash ('' for a file the
# activation did not create) carried through every recorded rerun. A record
# whose prior hash is not the state before it breaks the chain and stops.
function Resolve-BehaviorRerunHash($Reruns, [string] $Group, [string] $TargetFile, [string] $ActivationHash) {
  $h = $ActivationHash.ToLowerInvariant()
  $key = "$Group|$TargetFile".ToLowerInvariant()
  if ($Reruns.ContainsKey($key)) {
    foreach ($r in $Reruns[$key]) {
      if ($r.prior_sha256.ToLowerInvariant() -cne $h) {
        throw "Recorded rerun of $Group/$TargetFile does not follow its previous state"
      }
      $h = $r.sha256.ToLowerInvariant()
    }
  }
  $h
}

# Target files that recorded reruns added to a group (relative to its target root).
function Get-BehaviorRerunAddedFiles($Reruns, [string] $Group) {
  @($Reruns.Keys | Where-Object { $_.StartsWith("$Group|".ToLowerInvariant()) } | ForEach-Object {
    $first = $Reruns[$_][0]
    if (-not $first.prior_sha256) { $first.target_file }
  })
}
