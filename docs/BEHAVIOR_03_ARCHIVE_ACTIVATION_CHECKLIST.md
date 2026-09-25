# Activation checklist: archive `03_derived_metrics`

Scope: move `analysis_ready/03_derived_metrics/` unchanged to
`analysis_ready/history/original_layout/03_derived_metrics/` with
`Invoke-BehaviorNumberedRootArchive.ps1 -Action Activate`.
`06_behavioral_dynamics` and `12_systems_neuroscience_summary` are out of
scope. No step runs a scientific pipeline or edits a scientific output.

Starting state (2026-09-25 12:31 UTC): receipt
`_migration_control/numbered_root_archive/03_derived_metrics.json`, state
`prepared`, 53 files, 3,162,803,458 bytes, manifest
`53265050b732fd4e14ebef273e608453ec551e4362c292b4e5589320e811c5dd`, gate
`c01aa48a7e437a27595162d9a92022f97371d6c040cc0b817ea31f42cc3cbf61`
(`ArchivePath`), queue `31131d89c21049fdae79b07a643cc931684054f27edd69c2cda6690ab696c707`.

Run sections B to H in one PowerShell 7.2+ session. Start with this setup
and begin each section with `Set-Location $repo`:

```powershell
$repo  = 'C:\Users\topohl\Documents\GitHub\MMMSociability'
$sleap = 'C:\Users\topohl\Documents\GitHub\SLEAPanalyzer'
$ready = 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID\analysis_ready'
$tool  = Join-Path $repo 'Maintenance\Invoke-BehaviorNumberedRootArchive.ps1'
$manTool = Join-Path $repo 'Maintenance\Invoke-BehaviorNumberedRootArchiveManifest.ps1'
$work  = 'C:\Users\topohl\Documents\rfid_numbered_root_archive_review_20260924'
$rcpt  = Join-Path $ready '_migration_control\numbered_root_archive\03_derived_metrics.json'
$archiveArgs = @{
  AnalysisReadyRoot = $ready; RootName = '03_derived_metrics'
  Manifest = Join-Path $repo 'docs\behavior_output_archive_manifests\03_derived_metrics_20260924.csv'
  ManifestSha256 = '53265050b732fd4e14ebef273e608453ec551e4362c292b4e5589320e811c5dd'
}
$gateArgs = @{
  ReviewedReaderGate = Join-Path $repo 'docs\behavior_output_archive_gate_03_derived_metrics.csv'
  ReviewedReaderGateSha256 = 'c01aa48a7e437a27595162d9a92022f97371d6c040cc0b817ea31f42cc3cbf61'
  ReaderGateKind = 'ArchivePath'
}
$env:MMM_BEHAVIOR_PROJECT_ROOT = $null; $env:MMM_PROJECT_ROOT = $null
Set-Location $repo; . .\Maintenance\BehaviorArchiveSharedCode.ps1
```

## A. Approvals required before step D

1. `Activate` for `03_derived_metrics` only.
2. SLEAPanalyzer commits `817de68` and `5c0d89d` on `exp9-validation-study`
   (local, two ahead of origin). Push them, or confirm that bundle builds run
   only from this checkout. Without them, scripts 01, 07 and 09, and the bundle
   build that runs 09, stop at their `03` reads. The unversioned iCloud copies
   under `correlate_sleap_boris` stay unchanged by your decision and will stop
   at the same reads.
3. Other branches. This branch (`refactor/rfid-four-domain-characterization`)
   is unpushed. On `main`, `publication/e9-manuscript-rc1`, `rc2`,
   `audit/hmm-state-architecture` and `exp9-upstream-endpoint-corrections`,
   Stages 00-25 read `analysis_ready/03_derived_metrics/` by fixed path. Stage
   01 (`main` line 69) and Stage 19 (`main` line 110) also write there by
   default, and there is no receipt check. After D, no stage from those
   branches may run against this RFID tree until it carries the archive-aware
   resolver. The same holds today for the prepared receipt: it refuses writes
   only from code on this branch.
4. The live navigation writes in section F: backups, README copies, and the
   output-index refresh.

## B. Repository checks (no live writes)

1. Branch, clean tree, reviewed history, and unchanged tools since this
   checklist was reviewed. The loop must print `True` three times, and the
   final `git diff` must print nothing:
   ```powershell
   git -C $repo branch --show-current          # refactor/rfid-four-domain-characterization
   git -C $repo status --untracked-files=all --porcelain   # empty
   foreach ($c in '3aa33e6','997b084','38cf6e0','1b89ab5','3dd8167') {
     git -C $repo merge-base --is-ancestor $c HEAD; $LASTEXITCODE -eq 0 }
   $reviewed = git -C $repo log -n 1 --format=%H -- docs/BEHAVIOR_03_ARCHIVE_ACTIVATION_CHECKLIST.md
   git -C $repo diff $reviewed -- Maintenance Analysis/16_manuscript_behavior_report.R
   ```
   Repeat the last two lines immediately before D.
2. The receipt, gate, queue, scripts, shared code and manifest still match.
   Every value must be `True`:
   ```powershell
   $gatePath = $gateArgs.ReviewedReaderGate
   $queuePath = Join-Path $repo 'docs\behavior_output_archive_audit_script_queue.csv'
   $rec = Get-Content -Raw -LiteralPath $rcpt | ConvertFrom-Json
   $g = @(Import-Csv -LiteralPath $gatePath); $q = @(Import-Csv -LiteralPath $queuePath)
   $qh = Get-BehaviorArchiveTextSha256 $queuePath; $sh = Get-BehaviorArchiveSharedCodeSha256 $repo
   [ordered]@{
     receipt_prepared = $rec.state -ceq 'prepared' -and $rec.reader_gate_kind -ceq 'ArchivePath'
     manifest_raw     = (Get-FileHash -LiteralPath $archiveArgs.Manifest -Algorithm SHA256).Hash.ToLowerInvariant() -ceq $archiveArgs.ManifestSha256
     gate_sha         = (Get-BehaviorArchiveTextSha256 $gatePath) -ceq $rec.reader_gate_sha256
     queue_sha        = $qh -ceq $rec.reader_queue_sha256
     same_scripts     = $g.Count -eq $q.Count -and -not (Compare-Object @($g.script | Sort-Object) @($q.script | Sort-Object))
     rows_reviewed    = @($g | Where-Object { $_.review_state -cne 'archive_path_ready' -or $_.archive_root -cne '03_derived_metrics' -or [string]::IsNullOrWhiteSpace($_.path_review_evidence) -or [string]::IsNullOrWhiteSpace($_.writer_review_evidence) }).Count -eq 0
     rows_queue       = @($g | Where-Object queue_sha256 -cne $qh).Count -eq 0
     rows_shared      = @($g | Where-Object shared_code_sha256 -cne $sh).Count -eq 0
     rows_scripts     = @($g | Where-Object { $_.script_sha256.ToLowerInvariant() -cne (Get-BehaviorArchiveTextSha256 (Join-Path $repo $_.script)) }).Count -eq 0
   }
   ```
   If any value is `False`, stop. `Activate` would refuse before changing
   anything. Restore the pinned file to its reviewed content. If the change is
   intended, retire the receipt with `Abandon` and prepare again with a newly
   reviewed gate, which needs a new approval.
3. The whole suite passes. This must print nothing:
   ```powershell
   @(Get-ChildItem Testing/tests -Filter 'test_*.R' | ForEach-Object { Rscript $_.FullName *> $null; if ($LASTEXITCODE) { $_.Name } }) +
   @(Get-ChildItem Testing/tests -Filter 'test_*.ps1' | ForEach-Object { pwsh -NoProfile -File $_.FullName *> $null; if ($LASTEXITCODE) { $_.Name } })
   ```

## C. Writer freeze and pre-move checks (read-only except C0)

0. Snapshot the manifest outside the repository, so a line-ending rewrite by
   git cannot block the tool during D or a recovery:
   ```powershell
   $manSnap = Join-Path $work '03_derived_metrics_20260924.csv'
   if (Test-Path -LiteralPath $manSnap) { throw "Snapshot exists: $manSnap" }
   Copy-Item -LiteralPath $archiveArgs.Manifest -Destination $manSnap
   if ((Get-FileHash -LiteralPath $manSnap -Algorithm SHA256).Hash.ToLowerInvariant() -cne $archiveArgs.ManifestSha256) { throw 'Snapshot hash differs' }
   $archiveArgs.Manifest = $manSnap
   ```
1. Freeze until section F is finished: no pipeline stage, audit, Stage 16,
   SLEAPanalyzer bundle build, or ad hoc R session writes to the RFID tree.
   Peer Claude and Codex sessions and other machines must be idle; they cannot
   be seen from here.
2. Process scan. Accept only the VS Code R language and help servers
   (`base::source(base::commandArgs(TRUE))`), idle R terminals with `CpuMs`
   near 0, and this terminal. The scheduled-task query must return nothing:
   ```powershell
   $pat = 'analysis_ready|Behavior[\\/]RFID|Analysis[\\/]\d|Testing[\\/](audits|tests)|Maintenance[\\/]|exp9_boris|correlate_sleap_boris|build_exp9_behavior_bundle|robocopy'
   $watch = 'R.exe','Rterm.exe','Rscript.exe','rsession.exe','pwsh.exe','powershell.exe','python.exe','robocopy.exe','bash.exe'
   $t0 = @{}; Get-Process | ForEach-Object { $t0[$_.Id] = $_.TotalProcessorTime }
   Start-Sleep -Seconds 10
   Get-CimInstance Win32_Process | Where-Object { $_.Name -in $watch -or $_.CommandLine -match $pat } | ForEach-Object {
     $p = Get-Process -Id $_.ProcessId -ErrorAction SilentlyContinue
     [pscustomobject]@{ Id = $_.ProcessId; Name = $_.Name
       CpuMs = if ($p -and $t0.ContainsKey($p.Id)) { [int]($p.TotalProcessorTime - $t0[$p.Id]).TotalMilliseconds }
       Cmd = $_.CommandLine } } | Format-Table -Wrap
   Get-ScheduledTask | Where-Object State -ne 'Disabled' | Where-Object { ($_.Actions | ForEach-Object { "$($_.Execute) $($_.Arguments)" }) -match 'Rscript|R\.exe|analysis_ready|RFID|robocopy' }
   ```
3. Close Explorer windows and Excel files under `03_derived_metrics`. An open
   file does not block the rename on this share, but a Thumbs.db update or any
   write during the move makes the post-move hash check fail.
4. The newest `03` file is still `qc\first_night_seed_provenance.csv` at
   2026-09-24 16:59:39, and the control folder holds only
   `03_derived_metrics.json`:
   ```powershell
   Get-ChildItem -LiteralPath "$ready\03_derived_metrics" -Recurse -Force -File | Sort-Object LastWriteTime -Descending | Select-Object -First 1 FullName, LastWriteTime
   Get-ChildItem -Force -LiteralPath (Split-Path $rcpt)
   ```
5. `& $tool -Action Inspect @archiveArgs` returns `prepared`, 53, `PASS`.
6. Record the prepared-state outputs of two read-only checks for E:
   ```powershell
   Rscript Testing/audits/audit_first_night_candidate_set_algebra_crosscheck.R *> "$work\algebra_pre_03.txt"; "exit=$LASTEXITCODE"
   Rscript Maintenance/Show-BehaviorNumberedRootRouting.R 03_derived_metrics $manSnap *> "$work\routing_pre_03.txt"
   ```

## D. Activate (the only step that moves data)

```powershell
git -C $repo diff $reviewed -- Maintenance Analysis/16_manuscript_behavior_report.R   # must print nothing
& $tool -Action Activate @archiveArgs @gateArgs
```

The tool takes its lock and re-checks the gate, queue, script and shared-code
hashes. It confirms that the destination is absent and that every archived
path is under 260 characters (the longest is 198), then rehashes the original
(53 files). After checking for reparse points, it creates
`history/original_layout/` and writes `transferring`. It renames the root with
`Directory.Move`, rehashes the archive (about 30 s for 3.16 GB), and writes
`activated`. Expected result: `state activated`, `files 53`, `hashes PASS`, in
one to two minutes.

While the receipt says `transferring`, every R and PowerShell resolver for
`03` stops by design. That includes every current reader of
`foundations/behavior_metrics/` and `analyses/spatial_occupancy/{tables,audit}`
through `mmm_derived_metrics_output_root()` and the group resolvers (Stages
00, 01, 03-15, 19, 20-25, 28), the Stage 16 index, the index refresh, and
SLEAPanalyzer 01. SLEAPanalyzer 07 and 09 read the foundation copy directly
and are unaffected.

### If D throws or is interrupted: determine the state first

```powershell
Test-Path -LiteralPath $ready
Get-Content -Raw -LiteralPath $rcpt | ConvertFrom-Json | Select-Object state, activated_at_utc, drift_detected_at_utc
'original=' + (Test-Path -LiteralPath "$ready\03_derived_metrics") + ' archive=' + (Test-Path -LiteralPath "$ready\history\original_layout\03_derived_metrics")
Get-ChildItem -Force -LiteralPath (Split-Path $rcpt)
```

| Finding | Action |
| --- | --- |
| `$ready` unreachable | Do nothing until the share is back. |
| "Missing analysis_ready root or changed archive manifest" | Check `Test-Path $ready` and the snapshot hash. Never `Abandon` because of this message. |
| `03_derived_metrics.lock` present | Confirm the Activate terminal has exited and wait for the server to release the handle. Then `Remove-Item -LiteralPath (Join-Path (Split-Path $rcpt) '03_derived_metrics.lock')`. If that says "used by another process", the lock is still held; stop. |
| `.numbered-root-*.tmp` present | Delete it only after the receipt JSON parses. |
| `prepared`, original only | Nothing moved; an empty `history/original_layout/` may exist and is harmless. Fix the cause and repeat D, or `& $tool -Action Abandon @archiveArgs -AbandonReason '<reason>'`. |
| `transferring`, original only | `& $tool -Action Rollback @archiveArgs` rehashes the original and returns `prepared`. |
| `transferring`, archive only | `& $tool -Action Rollback @archiveArgs` verifies the archive and renames it back; then D may be repeated. |
| `transferring`, both present | Stop; delete nothing. List the recreated top-level root, and with approval move it aside outside `history/original_layout/`; then `Rollback`. |
| Rollback refuses on a hash (drift) | `& $tool -Action Rollback @archiveArgs -AcceptInventoryDrift`. It renames an archive back without trusting it, or flags a changed original in place, and leaves `transferring`. Review with `& $manTool -Action Build -AnalysisReadyRoot $ready -RootName 03_derived_metrics -Manifest "$env:TEMP\03_drift_inventory.csv"` and `Compare-Object (Import-Csv $manSnap) (Import-Csv "$env:TEMP\03_drift_inventory.csv") -Property relative_path,size_bytes,sha256`. Then `& $tool -Action Abandon @archiveArgs -AbandonReason '<reason>'`. |

Never edit or delete a receipt by hand. After `Abandon`, writes into `03` are
allowed again until a new `Prepare`; re-freeze writers. A new `Prepare` needs a
newly reviewed manifest and a new approval.

## E. Post-move verification (read-only)

1. `& $tool -Action Verify @archiveArgs` and `& $tool -Action Inspect @archiveArgs`
   return `activated`, 53, `PASS`. There is no top-level `03_derived_metrics/`,
   `history/original_layout/03_derived_metrics/` exists, and the control folder
   holds no `.lock` or `.numbered-root-*.tmp`.
2. `Rscript Maintenance/Show-BehaviorNumberedRootRouting.R 03_derived_metrics $manSnap`
   shows receipt `activated`, retained source
   `.../history/original_layout/03_derived_metrics`, and active roots
   `foundations/behavior_metrics`, `analyses/spatial_occupancy/tables` and
   `.../audit`. It also shows `ERROR` for both guard lines and
   `R can open at retained location 53`. Never test write refusal with
   `ensure_dir()` or a real write.
3. `& .\Maintenance\Test-BehaviorFoundationResidualInventory.ps1 -AnalysisReadyRoot $ready`
   passes, and its last line names `history\original_layout\03_derived_metrics`.
4. `Rscript Testing/audits/audit_first_night_candidate_set_algebra_crosscheck.R *> "$work\algebra_post_03.txt"`
   exits 0. `Compare-Object (Get-Content "$work\algebra_pre_03.txt") (Get-Content "$work\algebra_post_03.txt")`
   shows no difference apart from locale or package warning lines.
5. `git -C $sleap rev-parse --short HEAD` prints `5c0d89d` or a descendant with
   a clean status, and
   `Rscript Maintenance/Check-SleapAnalyzerArchiveLookup.R $sleap 's:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress'`
   prints the `history/original_layout/03_derived_metrics` root, then `TRUE`, `TRUE`.
6. `qc\first_night_seed_provenance.csv` is in the archive and absent from
   `foundations\behavior_metrics\qc\`; it stays retained and unpromoted.
7. `Rscript Maintenance/Refresh-BehaviorOutputIndex.R` (dry run) ends with
   `5 changed cell(s) in 63 rows`, all in `notes`:
   - `[01]` gains `Retained original: analysis_ready/history/original_layout/03_derived_metrics/`
   - `[01-identity-history]` gains `Retained original: analysis_ready/history/original_layout/03_derived_metrics/qc/`
   - `[19-tables]` and `[19-audit]` gain `Retained original: analysis_ready/history/original_layout/03_derived_metrics/spatial_occupancy/`
   - `[08-audit-history]` takes the wording committed in `c8c0a0a`: `... Audit reruns now require separate fresh replay folders; this string records the saved original layout.`

   Any other change is a stop.

If E1 passes and a later E step fails, do not start F;
`& $tool -Action Rollback @archiveArgs` returns the root and the receipt to
`prepared`. If E1 itself fails while the receipt says `activated`, do not roll
back first: readers still resolve the archive. Find the drift with the Build
and Compare commands above. `-AcceptInventoryDrift` would block every
foundation reader until `Abandon`.

## F. Live navigation updates (after E passes)

1. Before editing anything, confirm each live copy equals its repository
   source and back it up to a new file:
   ```powershell
   $mc = Join-Path $ready '_migration_control'
   $nav = @(
     @('README.md',             'docs\BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md', 'analysis_ready_README_before_03_archive_20260925.md'),
     @('foundations\README.md', 'docs\BEHAVIOR_FOUNDATIONS_DIRECTORY_README.md',    'foundations_README_before_03_archive_20260925.md'),
     @('history\README.md',     'docs\BEHAVIOR_HISTORY_DIRECTORY_README.md',        'history_README_before_03_archive_20260925.md'),
     @('analyses\README.md',    'docs\BEHAVIOR_ANALYSES_DIRECTORY_README.md',       'analyses_README_before_03_archive_20260925.md'))
   foreach ($n in $nav) {
     $live = Join-Path $ready $n[0]; $bak = Join-Path $mc $n[2]
     if ((Get-BehaviorArchiveTextSha256 $live) -cne (Get-BehaviorArchiveTextSha256 (Join-Path $repo $n[1]))) { throw "Live copy differs from repository: $live" }
     if (Test-Path -LiteralPath $bak) { throw "Backup exists: $bak" }
     Copy-Item -LiteralPath $live -Destination $bak }
   ```
2. Make these repository edits, then commit them with
   `git -C $repo commit --only -m '<message>' -- <the seven paths>`:
   - `docs/BEHAVIOR_ANALYSIS_READY_DIRECTORY_README.md`, replace line 6 with:
     ```text
     Current Stage 01 metric/QC foundations live under `foundations/behavior_metrics/`; see `foundations/README.md`. Their numbered `03_derived_metrics/` original, including the Stage 19 spatial originals, is retained unchanged under `history/original_layout/03_derived_metrics/`.
     ```
   - `docs/BEHAVIOR_FOUNDATIONS_DIRECTORY_README.md`: this is a substring
     replacement from `The numbered` at the end of line 11 through the end of
     line 14; line 11 keeps its receipt path. Replace
     ```text
     The numbered
     `../03_derived_metrics/` tree remains intact for historical scripts. Its
     older cross-scale identity reports, Stage 19 spatial outputs, and original
     run manifests were not copied into `behavior_metrics/`.
     ```
     with
     ```text
     The numbered
     original is retained unchanged at
     `../history/original_layout/03_derived_metrics/`; its archive receipt is
     `../_migration_control/numbered_root_archive/03_derived_metrics.json`. Its
     older cross-scale identity reports, Stage 19 spatial outputs, original run
     manifests, and the unpromoted 2026-09-24 `qc/first_night_seed_provenance.csv`
     were not copied into `behavior_metrics/`.
     ```
   - `docs/BEHAVIOR_HISTORY_DIRECTORY_README.md`: after line 8 (ending
     `records its producer role and status.`), insert a blank line and
     ```text
     `original_layout/` is different: it holds complete numbered output roots
     moved unchanged, not copies, each under a root archive receipt in
     `../_migration_control/numbered_root_archive/`; currently only
     `03_derived_metrics/` (53 files). Current analyses read
     `../foundations/behavior_metrics/` and `../analyses/spatial_occupancy/`, but
     their receipt checks require this archive, so never move, rename, or edit it.
     Historical replays and the SLEAPanalyzer BORIS metadata script read it
     through the receipt.
     ```
     and after the table's last row (`gamm_features/30min/`) add
     ```text
     | `original_layout/03_derived_metrics/` | Complete numbered Stage 01 and Stage 19 root, moved unchanged under its archive receipt; not a copy. |
     ```
   - `docs/BEHAVIOR_ANALYSES_DIRECTORY_README.md`, lines 81-82, replace
     `The numbered original folders remain intact for provenance and historical`
     / `readers.` with
     ```text
     The numbered original folders remain intact for provenance and historical
     readers; the Stage 19 table and audit originals are retained unchanged under
     `../history/original_layout/03_derived_metrics/spatial_occupancy/`.
     ```
   - `docs/DATA_AND_OUTPUTS.md`, lines 123-125, replace
     `The` / `numbered root retains all 52 originals, including separate identity-audit` /
     `and spatial outputs.` with
     ```text
     The
     numbered root, with all 53 originals including separate identity-audit and
     spatial outputs, is retained unchanged under
     `analysis_ready/history/original_layout/03_derived_metrics/`.
     ```
   - `docs/BEHAVIOR_OUTPUT_MIGRATION.md`, lines 162-163, replace
     ``all 52 files under `03_derived_metrics/` `` / `remain for historical paths and provenance.` with
     ```text
     all 53 original files of `03_derived_metrics/`
     are retained unchanged under `history/original_layout/03_derived_metrics/`.
     ```
   - `Testing/README.md`, line 89, replace
     ``its August reports under `03_derived_metrics/qc/` remain historical.`` with
     ```text
     its August reports, retained under `history/original_layout/03_derived_metrics/qc/`, remain historical.
     ```
3. Copy the four README sources to the live tree and confirm. This must print
   `True` four times:
   ```powershell
   foreach ($n in $nav) { Copy-Item -LiteralPath (Join-Path $repo $n[1]) -Destination (Join-Path $ready $n[0]) -Force }
   foreach ($n in $nav) { (Get-BehaviorArchiveTextSha256 (Join-Path $ready $n[0])) -ceq (Get-BehaviorArchiveTextSha256 (Join-Path $repo $n[1])) }
   ```
4. Refresh the index, record the SHA-256 it prints, and confirm that a
   following dry run ends with `0 changed cell(s) in 63 rows`:
   ```powershell
   Rscript Maintenance/Refresh-BehaviorOutputIndex.R --write --backup=output_index_before_03_archive_20260925.csv
   Rscript Maintenance/Refresh-BehaviorOutputIndex.R
   ```
5. Leave `output_index_contract_*.csv` unchanged; they are dated snapshots.
6. Record the activation, the index SHA-256 and the backups in
   `docs/BEHAVIOR_NUMBERED_ROOT_RETIREMENT_DESIGN.md` and
   `docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`, and commit with
   `git -C $repo commit --only`.

## G. Consequences to expect

- Current analyses keep reading `foundations/behavior_metrics/` and
  `analyses/spatial_occupancy/`, and their values do not change. Their receipt
  checks now require `history/original_layout/03_derived_metrics/` to exist and
  a top-level `03_derived_metrics/` to be absent. Never move, rename or edit the
  archive. If anything recreates the old folder (an Explorer "New folder",
  robocopy, or a Stage 01 or 19 run from another branch), Stages 00-28, Stage 16
  and the index refresh stop with "Activated numbered source archive has
  unexpected locations". Remove only an empty recreated folder, after review.
- Any reader of `analysis_ready/03_derived_metrics/` by fixed path stops with a
  missing-file error; none is silently redirected. This includes every checkout
  other than this unpushed branch (see A3). It also includes the iCloud
  SLEAPanalyzer copies, the SLEAPanalyzer origin branch without `817de68`, old
  manuscript provenance strings, and ad hoc scripts.
- Writes into the old or archived location are refused only by code on this
  branch.
- `Invoke-BehaviorOutputMigration.ps1` refuses all actions for
  `behavior_metrics_foundation`, `spatial_tables` and `spatial_audit` while the
  receipt exists. The residual inventory re-verifies the 20 Stage 01 copies;
  the spatial copies have no re-verification tool.
- Saved manifests of Stages 03, 09 and 20-25 and the rc1 release record
  `analysis_ready/03_derived_metrics/...` inputs. Those files now sit under
  `history/original_layout/03_derived_metrics/`, and the output index notes
  give the new location.
- SLEAPanalyzer 01, 07 and 09 follow the current Stage 01 assignment table in
  `foundations/`, which a Stage 01 rerun can change. Script 01's phenotype
  table comes from the frozen archive.
- `06` and `12` are unchanged and still need long-path support, or a shorter
  destination, before they can be prepared.

## H. Final check (a few hours after F)

`& $tool -Action Inspect @archiveArgs` returns `activated`, 53, `PASS`; there is
no top-level `03_derived_metrics/`; and `Rscript Maintenance/Refresh-BehaviorOutputIndex.R`
ends with `0 changed cell(s) in 63 rows`.
