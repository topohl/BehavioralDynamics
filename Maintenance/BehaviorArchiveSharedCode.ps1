# Dot-source to hash the shared code that decides where the queued audits
# read and write. A reviewed reader gate pins this hash beside each audit
# script hash, so a change to a path helper also closes the gate.

# Reviewed text files are hashed with CRLF normalized to LF. The worktree
# uses core.autocrlf=true and several pinned files have mixed line endings,
# so a checkout would otherwise change their hashes without a content change.
function Get-BehaviorArchiveTextSha256([string] $Path) {
  # Latin-1 maps every byte to one character, so the replacement is exact
  # for any encoding.
  $latin1 = [System.Text.Encoding]::Latin1
  $text = $latin1.GetString([System.IO.File]::ReadAllBytes($Path)).Replace("`r`n", "`n")
  [Convert]::ToHexString(
    [System.Security.Cryptography.SHA256]::HashData($latin1.GetBytes($text))).ToLowerInvariant()
}

function Get-BehaviorArchiveSharedCodeFiles([string] $RepoRoot) {
  $files = @(Get-ChildItem -LiteralPath (Join-Path $RepoRoot 'Functions') -Filter '*.R' -File |
             ForEach-Object { 'Functions/' + $_.Name })
  $files += @('Analysis/_pipeline_setup.R',
              'Analysis/10_systems_feature_prediction_ladder.R',
              'docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv',
              'docs/BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv')
  [string[]] $sorted = $files
  [Array]::Sort($sorted, [System.StringComparer]::Ordinal)
  $sorted
}

function Get-BehaviorArchiveSharedCodeSha256([string] $RepoRoot) {
  $lines = foreach ($relative in (Get-BehaviorArchiveSharedCodeFiles $RepoRoot)) {
    $path = Join-Path $RepoRoot ($relative.Replace('/', '\'))
    if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
      throw "Shared archive review file is missing: $relative"
    }
    $relative + "`t" + (Get-BehaviorArchiveTextSha256 $path)
  }
  $bytes = [System.Text.Encoding]::UTF8.GetBytes(($lines -join "`n") + "`n")
  [Convert]::ToHexString([System.Security.Cryptography.SHA256]::HashData($bytes)).ToLowerInvariant()
}
