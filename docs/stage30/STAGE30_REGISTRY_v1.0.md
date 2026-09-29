# Stage 30 exploratory registry v1.0 (Exp9 RFID behaviour)

**Status: FROZEN on 2026-09-29T11:21:10+0200.** This document is the human-readable companion to `stage30_registry_v1.0.json`. If the two differ, the JSON is authoritative.

- **Tier.** Exploratory, with decision basis POST_HOC_CONTEXT. These analyses were added after the frozen Stage 29 primary results, which were null, and after historical sleep and cookie results had been seen.
- **Status of results.** No result may be called primary, confirmatory or preregistered.
- **Separation from Stage 29.** Stage 29's model specification, endpoints and multiplicity are not changed by any Stage 30 finding.
- **No outcome contact before freeze.** No Stage 30 RES/SUS, Group or CombZ association was computed or inspected before this registry was frozen.
- **Decisions.** The user's decisions of 2026-09-29 are recorded verbatim in `evidence/wfE/USER_DECISIONS_2026-09-29_round5.txt`, with three follow-up answers:
  1. the global BH is a descriptive column only;
  2. each metric × question gets pooled, female, male and interaction analyses;
  3. this applies to all blocks and question types.

  A final correction (`evidence/wfE/USER_DECISIONS_2026-09-29_round6_POOL_estimation_only.txt`) makes POOL estimation-only. The discovery tests are therefore F, M and INT: **48** in total.

## 1. Measurement

**Movement.** The **RFID position-change rate** (position changes/h; identifier `crossing_rate`).
- At first use: vendor-defined RFID position changes, each requiring at least 200 grid units of displacement of the vendor-estimated position, i.e. about two antenna spacings.
- The carried-forward vendor position persists until the next qualifying change. A missing position change means no sufficiently large relocation was registered; it does not mean complete bodily immobility.
- All new locomotor measures use this canonical Stage 29 representation and code path, through `Functions/stage30_movement.R` (commit 4116498, sha256 `9a368491…`).
- The wrapper was validated as identical to the direct canonical calls in 888/888 Stage 29 windows, against an independent oracle on 312 arbitrary windows, and against a raw-export oracle on the cookie windows. Its contract test passes.
- Vendor ActivityIndex and raw-detection coverage are used for provenance only.

**Shared RFID-position occupancy** (identifier `shared_zone_use`). The time two cage-mates simultaneously carry the same carried-forward vendor position, as a fraction of their co-assigned time.
- Renamed from "shared antenna-zone use", because the metric does not use raw antenna identity.
- It is not sociability, coordination or huddling.

**RFID-defined sustained positional inactivity (≥ 40 s).** For each animal and canonical window, it is the share of observed time in intervals between consecutive canonical position changes that last at least 40 s.
- **Scoring:** an interval that reaches 40 s counts in full (Pack 2007; Fisher 2012; McShane 2012; Keenan 2020).
- **Window boundaries:** intervals are clipped to the window first, then qualify. There is no bridging across the 06:30/18:30 boundaries, and there are no recording interruptions or file ends inside the windows.
- **Windows:** active is [18:30, 06:30) and light is [06:30, 18:30), anchored at 18:30 on the logger clock on the cage-change day.
- **What the threshold means:** the 40-s threshold is literature-motivated, but this signal detects absence of large positional relocation, not total bodily immobility, and it is far coarser than video or EEG immobility detection. The measure may be discussed as an RFID-derived sleep/inactivity proxy, but its percentages are never estimates of physiological sleep duration.
- **Group-blind QC** (444 animal-windows per phase):

  | Property | Active | Light |
  |---|---|---|
  | Median fraction (IQR) | 0.942 (0.928–0.953) | 0.996 |
  | Windows ≥ 0.99 | – | 87% |
  | Spearman ρ with the position-change rate | −0.96 | −0.93 |
  | Residual reliability after the rate | 0.11 | 0.08 |

  The 40-s and 60-s versions agree with ρ ≈ 0.97. These properties constrain interpretation: associations that only recapitulate Movement are never described as independent sleep biology.
- **Sensitivity:** ≥ 60 s is a measurement sensitivity only.
- **Excluded:** the 1341.67-s threshold is not used.

**Home-cage cookie response (second presentation, EPM+1).**
- **Definition:** Δ60 = RFID position-change rate in POST60 = EPM+1 [17:00, 18:00) minus the rate in PRE60 = [16:00, 17:00), on the logger clock. PRE60, POST60 and Δ60 are all retained.
- **What it measures:** a locomotor challenge-response phenotype, i.e. the change in canonical home-cage activity from the hour before to the hour after presentation.
- **What it does not measure:** approach, proximity, investigation or consumption. It is also not a pure appetitive response, because presentation and experimenter effects cannot be separated.
- **Descriptive trace (group-blind; not used to choose windows):**
  - low baseline;
  - activity rises somewhat toward the end of PRE60, particularly in B4 (the cause is not established by records);
  - a strong response after 17:00, peaking at about 17:10–17:20;
  - decay by about 17:45–18:00.
- **Group-blind distribution:** median 18 (IQR 6–37) changes/h; 94/97 animals increased.
- **Consumption:** by the experimenter's documented recollection, the cookie was generally consumed within about 45–60 min, so no removal was needed. The scored B2 videos came from a different test and are not evidence here.
- **Sensitivity:** Δ45 = POST45 [17:00, 17:45) − PRE45 [16:15, 17:00). The historical filter included the 17:45 minute and ConsecInactive == 2 on vendor ActivityIndex.
- **Excluded:**
  - EPM+2 (procedure unresolved; not a no-cookie control);
  - the first presentation (confounded by EPM and the cage change);
  - B1 (no home-cage cookie).

## 2. Data

- **CC1–CC4.** Data version `v2_cage_label_correction_2026-09-28` (manifest sha `bb33a111…`), which contains board-based cage IDs. The four confirmed label errors are corrected: B1 CC2 OQ764 → sys.2, OQ770 → sys.5, OQ772 → sys.4; B6 CC4 OR646 → sys.5. The known-wrong labels are kept in provenance only.
- **Cookie.** Immutable data version `cookie_postEPM_v1` (manifest sha `0abe20ee…`, 21 files), built at commit 4116498 from the five SHA-registered B2–B6 post-EPM exports with the canonical preprocessing and the board-based OR646 override.
  - It is copied to `S:/…/RFID/MMMSociability/data_versions/cookie_postEPM_v1/` at freeze and set read-only.
  - The historical B6-only cookiehab outputs are preserved as provenance.
- **Counts and cage IDs.**
  - Cookie: 97 animals in 25 cages. SIS 77 in 20 cages; females 46 in 12 cages (B3, B4, B6); males 31 in 8 cages (B2, B5 only, a limited male batch structure).
  - CC1 SIS: 87 animals in 24 cages (females 46 in 12, males 41 in 12).
- **OR646.** Included, with her corrected cage B6|sys.5. Her re-housing with two former cage-mates is a documented protocol deviation, and no exclusion or exclusion sensitivity is applied.
- **Labels and outcome.** The canonical sus/con lists and the canonical later CombZ; their sha256 values are recorded at freeze.

## 3. Hypotheses (48 discovery tests) and local BH families

The analysis is sex-resolved. Every metric × question has three discovery tests and one estimate:
- **F:** within females (discovery);
- **M:** within males (discovery);
- **INT:** the formal test that the association differs by sex (discovery);
- **POOL:** pooled SIS, no phenotype × sex term. **ESTIMATION_ONLY:** estimate, 95% CI, N and diagnostics, with no p, no family, no global benchmark and no classification. These 16 estimates are listed under `estimation_only` in the JSON.

Sex specificity is never inferred from separate F and M results; INT is the formal test.

| Family (local BH) | Question and model | Metrics | m |
|---|---|---|---|
| SCREEN-CONT | CC1 continuous: F/M `CombZ ~ Batch + x` (within sex), INT `CombZ ~ Batch + x + x:sex_c`; CR2 by cage, Satterthwaite df | occupancy dispersion, fragmentation, light-phase position-change rate | 9 |
| SCREEN-L | CC1–CC4 CombZ trajectory: F/M KR joint F(3) of CombZ_wb:(c2, c3, c4) within sex; INT KR joint F(3) of CombZ_wb:(c2, c3, c4):sex_c; `(1 \| AnimalID) + (1 \| CageEpisodeID)` (position-change rate: `(1 + cc1 \|\| AnimalID)`) | position-change rate, shared RFID-position occupancy, occupancy dispersion, fragmentation, light-phase rate | 15 |
| SLEEP-CONT | CC1 continuous, as SCREEN-CONT | positional inactivity, active and light | 6 |
| SLEEP-CAT | CC1 RES/SUS: F/M `y ~ Batch + g_RS + (1 \| CageEpisodeID)` within sex, INT `y ~ Batch + g_RS + g_RS:sex_c + (1 \| CageEpisodeID)`; KR t | positional inactivity, active and light | 6 |
| SLEEP-L | CC1–CC4 trajectory, as SCREEN-L | positional inactivity, active and light | 6 |
| COOKIE-CONT | continuous, as SCREEN-CONT (cookie SIS) | Δ60 | 3 |
| COOKIE-CAT | RES/SUS, as SLEEP-CAT (cookie SIS) | Δ60 | 3 |

- **Totals:** 24 broad, 18 inactivity and 6 cookie, i.e. **48 discovery hypotheses.**
- **Broad-screen scope:** the broad screen tests only metric × question combinations that Stage 29 did not address. Stage 29-addressed combinations appear as carry-over rows from the corrected release (ebb_v101), with no new p.
- **Global BH over the 48:** a descriptive column only; never a classification criterion.

## 4. Inference and calibration

- **Engine.** The frozen Stage 29 engine (`Functions/rfid_canonical_inference.R`).
  - **CR2:** the Stage 29 section-6b lm route: CR2 by the corrected CageEpisodeID, Satterthwaite df, with the OLS t alongside (descriptive).
  - **KR:** REML with bobyqa; KR t or joint F(3); singular fits retained and flagged; the Stage 29 optimizer check.
  - **Failures:** FAILED or FAILED_RANK hypotheses enter with p = 1 (m unchanged).
- **Design gate.** Before any outcome is read, every CR2 hypothesis must have its declared rank and design-fixed df.
- **Null calibration (group-blind, pre-freeze).** Synthetic outcomes and labels on the real measurements (1.13 M fits). All 48 discovery tests, and the 16 POOL estimation models, are within [0.025, 0.075] at α = 0.05 at their design-appropriate null. Removing POOL deleted tests only, so the F/M/INT calibrations stand unchanged.
  - **LOW_DF (conservative):** SLEEP-CONT-IA40A-M (df 3.56) and SLEEP-CONT-IA40L-F (df 3.83).
  - **Watch items (in band):**
    - SLEEP-CAT-IA40A-M 0.068;
    - SLEEP-CAT-IA40L-M 0.071;
    - SCREEN-L-SPO-F 0.067 (always singular);
    - SCREEN-L-FRAG-INT 0.0625.
  - **Conservative:** COOKIE-CAT-DC60-M.
  - **Decision:** every test is accepted as registered, and each hypothesis carries its calibration note.
- **Reported per hypothesis:**
  - estimate, standardized estimate, SE, df, 95% CI;
  - raw p, local q and global q (descriptive);
  - N animals, cages, batches (and windows);
  - fit diagnostics;
  - LOBO signs and robustness labels;
  - the Movement-adjusted estimate (inactivity);
  - classification and reason code.

## 5. Sensitivities (estimation only; never in a family)

- **All hypotheses:**
  - leave-one-batch-out refits: same-sex batches only; the Batch term is dropped when a single batch remains;
  - animal influence.
- **CONT hypotheses:** the OLS t (descriptive).
- **Inactivity:**
  - the 60-s version;
  - Movement-adjusted incremental information: the same-window position-change rate added as a covariate.
- **Cookie:**
  - Δ45;
  - alternative inference: a KR mixed model for CONT, lm + CR2 for CAT;
  - Movement-adjusted incremental information: the Stage 29 CC1 active position-change rate as a covariate;
  - nested LOBO prediction for any hypothesis classified A.
- **Judgement:** sensitivities are judged on direction, magnitude and leverage, not on crossing 0.05.
- **Explicitly excluded:**
  - OR646 or cage exclusion;
  - the known-wrong label cages;
  - EPM+2;
  - ActivityIndex;
  - 1341.67 s;
  - the unrelated scored videos.

## 6. Result classification (applied in the order D, A, B, C)

| Class | Rule |
|---|---|
| **D. measurement- or fit-limited** | An inactivity result with raw p < 0.05 whose Movement-adjusted CI includes 0 (L: adjusted joint p ≥ 0.05), **or** a FAILED or FAILED_RANK primary fit. |
| **A. robust exploratory signal** | Local BH q < 0.05, **and** the focal sign is stable in every leave-one-batch-out refit, **and** no sensitivity is labelled SIGN_CHANGE. |
| **B. suggestive** | Raw p < 0.05 but not A: either multiplicity-negative, or q < 0.05 but not robust. |
| **C. clear null** | Raw p ≥ 0.05; reported as "no evidence of association at the registered test", with its CI. |

Wording rules:
- Within-sex results are described "in females" or "in males".
- "Female-specific" or "male-specific" is used only if the same metric × question's INT test has local q < 0.05.
- POOL estimates are flagged ESTIMATION_ONLY, receive no classification, and are described as "pooled across sexes (estimation only)".
- Every light-phase inactivity result states that the measure is saturated.

## 7. Run rules

- Every registered hypothesis is run once, and the run does not stop at a significant result.
- No new endpoints, windows, interactions, features or thresholds are added after outcome contact.
- The outcome driver `Analysis/30_exploratory_screen.R` is written after the freeze and refuses to fit unless the frozen registry hash matches.

## 8. Stage 29 corrected release (approved; executed separately)

- **Configuration.** Config v1.0.1: documentation only, plus a data-version binding to v2. The analytic specification is identity-gated to v1.0.0.
- **Outputs.** A new immutable release folder and bundle `ebb_v101`. This becomes the manuscript-facing Stage 29 release.
- **Preserved.** Config v1.0.0, bundle `ebb_v100_20260927_95e5dc8`, `pipeline/29_canonical_behavior`, and the archived 0555c90 run snapshot.
- **Effect of the correction:**
  - all CC1 results are identical;
  - longitudinal estimates move by at most 0.47 SE;
  - the shared-position P-TR p goes from 0.826 to 0.755;
  - no inferential or multiplicity conclusion changes.
- **Rehearsal.** The release was rehearsed completely in a sandbox:
  - identity gate;
  - CONTROL: 23/23 tables identical;
  - RELEASE: 23/23 tables identical to the corrected sandbox run;
  - every 16b gate.
