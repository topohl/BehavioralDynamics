# Figure 1 Results — manuscript draft

Source repository `MMMSociability`, commit recorded in
`results/manuscript_bridge/figure1/export/figure1_repo_provenance.csv`. Every
statistic is read from the canonical Stage 09 artefacts; none is restated from
documentation. Citations are `[REF]` markers.

---

## Early spontaneous home-cage behaviour is prospectively associated with, and predicts, later stress burden

Adolescent social instability stress produces heterogeneous later outcomes, and
we asked whether that heterogeneity is foreshadowed by spontaneous behaviour
recorded before any outcome was measured. Continuous RFID home-cage monitoring
was used to quantify movement during the **first 12 h active phase following the
first cage change at P25** (18:30 to 06:30, 10-min bins), the earliest
standardised window of the paradigm. Later stress burden was summarised as a
composite score (CombZ), the unweighted mean of six standardised outcome
measures — novel-object recognition, sucrose preference, body-weight change,
corticosterone reactivity, adrenal weight and spleen weight — all collected
**after** the recording period, with higher CombZ indicating a more
resilient-like animal. Of the cohort, 111 animals had both a resolvable
first-night window and a CombZ endpoint (58 female, 53 male; 24 control, 49 later
resilient, 38 later susceptible). [METHODS: timeline, CombZ construction]

Because the resilient and susceptible labels are thresholded on CombZ itself,
every later behavioural and physiological measure that contributes to CombZ
differs between those groups **by construction**. We therefore do not report
those group differences as findings. The classification is used only to describe
the cohort; all prospective analyses below use the continuous CombZ endpoint and
exclude the group label entirely.

Early movement was inversely associated with later stress burden. Across the 111
animals, mean movement in the first active 12 h correlated negatively with CombZ
(Spearman ρ = −0.39, 95% bootstrap CI [−0.55, −0.21], *P* = 2.3 × 10⁻⁵, *q* = 6.9
× 10⁻⁵). Because higher CombZ denotes a more resilient-like outcome, **greater
early locomotor activity corresponded to greater later stress burden**. Movement
variability over the same window showed the same direction and also survived
correction (RMSSD: ρ = −0.23, CI [−0.41, −0.02], *q* = 0.026). A third
prespecified feature, the lag-1 autocorrelation of movement entropy, did **not**
reach FDR support at the primary resolution (ρ = −0.17, CI [−0.35, +0.02], *q* =
0.067) and is reported as inconclusive; its partial correlation with CombZ after
controlling for mean movement is −0.06, so it carries essentially no independent
information. All three features were declared in advance and corrected together
as one family of three.

The association is strong enough to support genuine out-of-sample prediction. A
fixed, prespecified behaviour-only model using mean movement alone predicted
CombZ in held-out animals under leave-one-animal-out cross-validation
(out-of-sample *R*² = 0.159), with a repeated grouped five-fold companion giving
a closely matching estimate (mean *R*² = 0.156, 2.5–97.5% interval across splits
[0.116, 0.179]) and a full-refit outcome permutation test yielding *P* = 1/1001.
An intercept-only baseline performed at chance (*R*² = −0.018), as expected. The
model registry was fixed before fitting, no predictor scaling or feature
selection was performed, and the outcome-derived group label never entered any
model, so no outcome information reaches the held-out animal.

Adding features did not improve prediction. The prespecified three-feature model
performed no better than movement alone (*R*² = 0.152 versus 0.159), and neither
did adding sex (0.152 and 0.142 for the one- and three-feature variants). We
therefore report mean movement as the single prospective predictor and do not
claim that temporal-organisation features contribute beyond it. No formal
model-comparison test was performed, so these differences are descriptive.

The association did not differ detectably between sexes. Formal feature-by-sex
interaction tests were negative for all three features (all *q* = 0.90), and the
sex-stratified estimates for the headline feature are near-identical (female ρ =
−0.41, n = 58; male ρ = −0.42, n = 53). We therefore describe the effect as
present in both sexes and explicitly do **not** describe it as female-specific or
as stronger in females. The study is not powered to detect a moderate
interaction, so this is absence of evidence rather than evidence of equivalence.

Together, spontaneous locomotor activity during the first 12 h of the paradigm —
recorded weeks before any outcome measure and before the resilient/susceptible
classification existed — is prospectively associated with later composite stress
burden and predicts it in held-out animals. The effect size is modest,
corresponding to roughly 16% of out-of-sample variance explained, and the
validation is internal: no independent cohort was tested. Animals were housed in
shared RFID cages and cage identity is not represented in the prediction design,
so the held-out estimate may be optimistic with respect to cage-level structure.
These are prospective associations, not evidence that early activity causes the
later outcome. [METHODS: prediction and cross-validation]
