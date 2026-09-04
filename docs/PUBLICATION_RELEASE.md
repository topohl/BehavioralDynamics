# Publication release process

How a BehavioralDynamics release candidate is frozen, validated and archived.

A "release" here means a **self-contained, hash-verified bundle** of exactly the
artifacts backing the manuscript, plus enough provenance for a reviewer or a
future reader to confirm that what is in the bundle is what the code produced.

---

## Principles

1. **Copy, never move.** The release builder never moves, deletes or rewrites
   anything in the live analysis tree. A release is a read-only projection.
2. **Canonical only.** Only artifacts resolved through the canonical-first
   contract are eligible. Quarantined trees are refused outright.
3. **Hash-verified end to end.** Every copied file has its source SHA-256 and its
   copied SHA-256 recorded, and the builder asserts they match.
4. **Fail closed.** A missing required artifact aborts the build. Nothing is
   silently omitted.
5. **Selection is editorial, not statistical.** What goes in the bundle comes
   from `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`, never from a significance
   threshold.
6. **Reproducible identity.** Every bundle records the git SHA, the build
   timestamp, `sessionInfo()` and the package inventory.

---

## Freezing a release candidate

### 1. Repository is clean and tagged

```bash
git status --porcelain          # must be empty
git rev-parse HEAD
```

The bundle records this SHA. A dirty tree means the bundle cannot be attributed
to a commit, so the builder records the dirty state explicitly.

### 2. Portable verification passes

```bash
for f in Testing/tests/test_*.R; do Rscript "$f" || echo "FAILED: $f"; done
```

All 17 must pass. This is also what CI enforces on every push.

### 3. Data-dependent verification passes

With the E9 dataset mounted, the manuscript-critical audits are re-run. At
minimum, Stage 16 validation must be **16/16 PASS** and all provenance hashes
must match:

```r
rfid <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
v <- read.csv(file.path(rfid, "analysis_ready/manuscript/behavior/validation.csv"))
stopifnot(all(v$status == "PASS"))
```

### 4. Registry is current

`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv` must reflect the current outputs, not
remembered ones. Every `current_status` and `effect_size` field is read from the
canonical artifacts. `code_git_sha` must match the release commit.

### 5. Dry run

```bash
Rscript Analysis/build_publication_release.R --dry-run
```

Resolves and hashes everything, verifies every required artifact exists, and
reports what *would* be written — without creating the bundle.

### 6. Build

```bash
Rscript Analysis/build_publication_release.R --release-id=rc1
```

Writes to:

```text
<RFID_ROOT>/releases/E9_behavior_manuscript_rc1/
```

### 7. Tag

```bash
git tag -a release/e9-behavior-rc1 -m "E9 behaviour manuscript release candidate 1"
```

The tag and the bundle's `code/git_sha.txt` must agree.

---

## Bundle structure

```text
E9_behavior_manuscript_<release_id>/
├── README.md                     what this bundle is, how it was built
├── SHA256SUMS.txt                every file in the bundle
├── code/
│   ├── git_sha.txt               commit, branch, dirty flag, build timestamp
│   ├── sessionInfo.txt           R and platform at build time
│   └── package_versions.csv      resolved versions of declared dependencies
├── figures/
│   ├── main/
│   └── supplementary/
├── source_data/                  per-panel source data (CSV)
├── tables/
│   ├── primary/
│   └── supplementary/
├── provenance/
│   ├── analysis_registry.csv     copy of the manuscript analysis registry
│   ├── artifact_manifest.csv     one row per copied file
│   ├── upstream_hashes.csv       source path, size, mtime, source SHA-256
│   └── validation.csv            build-time assertions and their results
└── qc/                           QC tables supporting the reported analyses
```

### `artifact_manifest.csv`

One row per copied artifact:

| Column | Meaning |
|---|---|
| `artifact_id` | Stable identifier from the registry |
| `bundle_path` | Path within the bundle |
| `source_path` | Absolute path it was copied from |
| `source_sha256` | Hash before copying |
| `copied_sha256` | Hash after copying |
| `hash_match` | Must be `TRUE` for every row |
| `resolution_class` | `canonical` or `legacy_fallback` |
| `required` | Whether a missing file aborts the build |
| `bytes`, `source_mtime` | Provenance |

### `validation.csv`

Build-time assertions, including: every required artifact present; every
hash matched; no source path under a quarantine tree; git SHA recorded; registry
present and non-empty. Any `FAIL` aborts the build.

---

## What the builder refuses to do

- Read from `analysis_ready/_quarantine_legacy_s09/` or any path containing
  `_quarantine`.
- Silently substitute a legacy artifact for a missing canonical one. Legacy
  resolution is permitted only where the resolution contract documents it, and it
  is recorded as `legacy_fallback` in the manifest.
- Continue past a missing required artifact.
- Write anywhere inside the repository, or anywhere inside `analysis_ready/`.
- Choose panels by p-value.

---

## Archiving

A frozen release candidate should be archived as:

1. The bundle directory, preserved as-is under `releases/`.
2. The git tag, pushed to the remote once the manuscript is submitted.
3. `MMMSociability/raw_data/` — separately and independently. The bundle does
   **not** contain raw data and the raw data cannot be regenerated. See
   `docs/DATA_AND_OUTPUTS.md`.

For a public archive (Zenodo or similar) after acceptance, deposit the tagged
repository and the release bundle, and mint the DOI at that point.
`CITATION.cff` deliberately contains no DOI or publication metadata until real
values exist.

---

## Release candidate vs final submission

The default build is a **release candidate**: it packages everything the registry
marks as manuscript-facing, including supplementary and conditional items, so
that the panel selection can be made with the full evidence in hand.

A final submission bundle is a *narrowing* of the candidate — an explicit
editorial decision recorded in the registry, then rebuilt with a new
`release_id`. That decision is not made by this repository. See the open items in
`manuscript/README.md`.

---

## Open items before a final release

These require a human decision and are deliberately not defaulted:

1. **Repository licence.** No `LICENSE` file exists. See
   `docs/LICENSE_DECISION_REQUIRED.md`.
2. **Final manuscript panel selection.** Which of the candidate panels form the
   figure.
3. **Inactive-QC production adoption.** Whether to implement the specified
   relative read-density classification before submission, which determines
   whether any inactive-phase result can be promoted. See
   `docs/KNOWN_LIMITATIONS.md` item 3.
4. **Stage 09 resolution sensitivity.** Whether to rerun Stage 09 at 5 min under
   the current contract so the declared sensitivity stops being unavailable.
5. **`renv` adoption.** See `docs/REPRODUCIBILITY.md`.
