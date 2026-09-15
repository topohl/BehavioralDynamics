# Figure 1 Methods — manuscript draft

Behavioural methods only. Every parameter traces to the canonical Stage 09
artefacts or to `docs/COMBZ_CANONICAL_DEFINITION.md`. Items that cannot be
recovered are marked `[METHOD DETAIL UNRESOLVED]` rather than filled from
convention.

---

## Animals and design

Behavioural analyses use 111 animals with both a resolvable first-night RFID
window and a complete later outcome endpoint: 58 female and 53 male; 24 control,
49 later resilient and 38 later susceptible. The biological replicate is the
animal. _[METHOD DETAIL UNRESOLVED: housing group size, light cycle and the
social-instability schedule are not specified in the analysis code and must be
taken from the experimental protocol.]_

## Home-cage RFID monitoring and the early window

Spontaneous home-cage activity was recorded continuously by RFID. The
manuscript-relevant early window is the **first Active phase block following the
first cage change (CC1) at P25**, defined on a fixed clock as 18:30 inclusive to
06:30 exclusive — 12 elapsed hours, binned at 10 min, giving 72 slots per animal.
Only the first cage change contributes. Inactive-phase bins are never included;
phase selection uses an explicit normalised value set rather than a permissive
match. Across the analysis set, 7,879 of 7,992 expected animal-slot rows were
observed.

Three features were computed over this window and declared in advance: mean
movement (`Movement_mean`), the root-mean-square of successive differences in
movement (`Movement_rmssd`), and the lag-1 autocorrelation of movement entropy
(`Entropy_acf1`). These are **raw window summaries, not model-derived
quantities**; no GAMM enters the canonical primary analysis. No transformation,
centring or scaling was applied. A 5-min binning sensitivity analysis is reported
separately; note that `Movement_rmssd` and `Entropy_acf1` are lag-1-**bin**
statistics, so they denote a different physical interval at 5 min and the two
resolutions are not the same construct for those two features.

## Later outcome assessment and the composite stress-burden score

Later outcome was summarised as CombZ, the **unweighted mean of six standardised
components**: novel-object recognition discrimination index, combined sucrose
preference, body-weight change, corticosterone rise, adrenal weight ratio and
spleen weight ratio. Each component is a z-score of its raw measure computed
against the **within-sex control animals** (12 male controls for males, 12 female
controls for females) using the **population standard deviation**. Corticosterone
rise, adrenal weight and spleen weight are sign-inverted so that a higher value
of every component denotes a more resilient-like animal; consequently **higher
CombZ means lower later stress burden**. Components are equally weighted at 1/6
and averaged over whatever components are available (two of 117 animals have five
of six). No batch term enters CombZ. All six components are measured after the
RFID recording period.

Canonical CombZ and the later outcome assignments are reproduced exactly from the
historically defined component z-scores (maximum absolute difference 2.2 ×
10⁻¹⁶; no label mismatches across the 93 stress-exposed animals). The upstream
standardisation is preserved as source provenance rather than redefined:
_[METHOD DETAIL UNRESOLVED: the workbook encodes its reference population as
hard-coded absolute row positions rather than a semantic within-sex rule, so the
component z-scores cannot be re-derived under a single uniform algorithm without
changing the endpoint.]_

## Later outcome classification

Stress-exposed animals were classified as susceptible when CombZ fell below the
within-sex control mean minus one within-sex control population standard
deviation, and resilient otherwise; control animals retain their own label and
are never classified as either. Thresholds were −0.437 (male) and −0.222
(female). The classification is a **downstream property of CombZ**, so it did not
exist at the time of RFID recording and must be described as the later outcome
group rather than a baseline group. **The group label is excluded from every
canonical prospective model** and is used only descriptively.

## Association analyses

Each of the three prespecified features was related to CombZ by Spearman rank
correlation with a 5,000-sample bootstrap confidence interval, across all 111
animals. The three tests form one Benjamini–Hochberg family. Partial correlations
controlling for mean movement, and for mean movement and sex, were computed for
the entropy feature.

## Prediction and cross-validation

Prediction used a **fixed a priori model registry declared before fitting**: an
intercept-only baseline, mean movement alone, the three-feature set, and
sex-adjusted variants of the latter two. The outcome is continuous CombZ; no
thresholding is applied. The primary estimate is **leave-one-animal-out**
cross-validation — for each of the 111 animals the model is refitted on the
remaining 110 and used to predict the held-out animal — summarised as the
cross-validated *R*² across held-out predictions. A repeated grouped five-fold
cross-validation (k = 5, 100 repeats, grouped on animal, seed 123) is reported as
a robustness companion with 2.5–97.5% quantiles across splits.

Because the model registry is fixed and no scaling, centring or feature selection
is applied at any point, there is no preprocessing step that could be fitted
outside a training fold. Statistical significance was assessed by **full-refit
outcome permutation**: the outcome was permuted and the entire
leave-one-animal-out procedure repeated 1,000 times (seed 20260811), giving *P* =
1/1001. This is distinct from, and replaces, an earlier post-hoc
prediction-vector permutation.

_[LIMITATION: cage identity is not represented in the Stage 09 design table, so
cage-level dependence is neither modelled nor assessable from the frozen
outputs. Animals share RFID cages, so the held-out estimate may be optimistic
with respect to cage structure. This is internal validation; no independent
cohort was tested.]_

## Sex

Sex was assessed as a formal feature-by-sex interaction on CombZ for each of the
three features, corrected as one family of three. Sex-stratified correlations are
reported as descriptive estimates and are **not** interaction tests.

## Analyses not used in the main figure

Hidden-Markov behavioural-state analyses are not promoted to the main manuscript:
the repository records first-night HMM persistence as unstable across optima, and
occupancy entropy and first-night persistence as excluded. Behaviour–proteomics
integration is underpowered, with no association reaching FDR < 0.05, and
supports no main-text claim.

## Software

_[METHODS TODO: R version, package versions and the session manifest from
`docs/package_versions.csv` and `docs/sessionInfo.txt`.]_
