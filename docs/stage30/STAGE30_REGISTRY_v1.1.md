# Stage 30 registry v1.1 (companion to stage30_registry_v1.1.json; the JSON is authoritative)

**Status.** FROZEN 2026-10-19T17:59:48+0200. Re-registration of v1.0 (stage30_registry_v1.0.json (FROZEN 2026-09-29T11:21:10+0200, sha256 5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e ; REAL run v1.0_be71e2f on outcome version ov1)) on the corrected outcome.

**Reason.** erratum E13 (docs/BEHAVIOR_CONFIG_v1.0.0_ERRATA.md): the endpoint workbook was corrected upstream; CombZ changes for every animal and RES/SUS for OR419 and OR434 among the RFID animals .
**Decision.** user decision 2026-10-17: rerun every registered analysis on the corrected workbook (scope 'Everything') .

**Unchanged.** hypotheses, families, inference, measures, sensitivities, classification rules, populations other than their RES/SUS counts, run rules (the v1.0 companion STAGE30_REGISTRY_v1.0.md describes them).

**Changed.**

- Outcome: version ov2_corrected_workbook, CombZ table sha256 5acb9a25a05cb32e44ce9523f9477a0bc307f52287e6d8cbd8d05110b9a40e10.
- Stage 29 release: v102_dv2_a46be6a, bundle ebb_v102_20261019_677ed97; reference table sha256 0e8b4498d852fadd26f3c669f37c1fa2834fb0a24f80f3f9c13ebd2fb8c86ecb.
- RES/SUS counts:

| population | v1.0 | v1.1 |
|---|---|---|
| SIS_CC1 | 4/6; 14/2; 7/9; 9/6; 7/8; 12/3; 53/34; RES/SUS counts only | 4/6; 14/2; 5/11; 9/6; 7/8; 12/3; 51/36; RES/SUS counts only |
| COOKIE_SIS | RES 49, SUS 28 | RES 47, SUS 30 |
| COOKIE_SIS_F | RES 28, SUS 18 (B3 7/9, B4 9/6, B6 12/3) | RES 26, SUS 20 (B3 5/11, B4 9/6, B6 12/3) |
| COOKIE_SIS_M | RES 21, SUS 10 (B2 14/2, B5 7/8); within-batch RES-SUS information in males comes mainly from B5 because B2 has only 2 SUS | RES 21, SUS 10 (B2 14/2, B5 7/8); within-batch RES-SUS information in males comes mainly from B5 because B2 has only 2 SUS |

**Outcome contact before this freeze.** the v1.0 REAL screen on the pre-correction outcome (run v1.0_be71e2f, outcome version ov1) was computed and seen; no Stage 30 RES/SUS, Group or CombZ association on the corrected outcome was computed or inspected .
