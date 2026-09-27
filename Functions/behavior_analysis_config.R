# ================================================================
# Frozen canonical behavioural analysis configuration (Exp9 SIS RFID)
# MMMSociability -- Functions/behavior_analysis_config.R
# ================================================================
# Single source of truth for every design choice of the canonical behavioural
# analysis: population, windows, metric definitions, temporal resolution, model
# formulas, random effects, contrasts, multiplicity families, sensitivity rules,
# diagnostics, reporting fields and Figure 1 units.
#
# FREEZE RULE. This list is serialised to canonical JSON and hashed (SHA-256) BEFORE
# any RES/SUS outcome model is fitted (Analysis/_supporting/freeze_behavior_analysis_config.R).
# After the hash is written, no design choice may change in response to RES/SUS results
# (meta$change_rule). Every named element is a list (never a named atomic vector) so
# that the JSON keeps all names; I() keeps length-1 arrays as arrays.
#
# Design provenance: C:/Users/topohl/Documents/e9_behavior_rationalization_audit_2026-09-27/
#   AUDIT_REPORT.md, DESIGN_REVISION_2026-09-27.md, PREFREEZE_CHECKS_A-K_2026-09-27.md,
#   evidence/implementation_2026-09-27/config_review_findings.txt (pre-freeze review).
# Choices were made on design and group-blind measurement grounds AFTER substantial
# inspection of the dataset; tiers are primary / secondary / descriptive / exploratory,
# never "confirmatory", and nothing here is a preregistration.
# ================================================================

MMM_BEHAVIOR_CONFIG <- list(
  meta = list(
    config_id = "E9_SIS_RFID_BEHAVIOR_CANONICAL",
    config_version = "1.0.0",
    frozen_on = "2026-09-27",
    frozen_before_res_sus_outcome_models = TRUE,
    alpha = 0.05, sidedness = "two-sided for every t and z test; F and chi-square tests are omnibus",
    ci_level = 0.95,
    tier_vocabulary = I(c("primary", "secondary", "follow-up", "descriptive", "exploratory", "prediction", "estimation", "robustness")),
    prohibited_tier_words = I(c("confirmatory", "preregistered", "pre-registered")),
    decision_basis_vocabulary = I(c("DESIGN", "GROUP_BLIND_MEASUREMENT", "REGISTRY_CONTINUITY", "POST_HOC_JUDGEMENT", "POST_HOC_CONTEXT")),
    group_results_seen_before_freeze = paste(
      "Group results for related quantities existed and had been seen before this freeze:",
      "Stage 03 (Movement by CC x phase), Stage 09 (Movement_mean/Movement_rmssd/Entropy_acf1 vs CombZ at 10 and 5 min),",
      "Stage 14 (first-night five-domain lm, HMM heatmap incl. CC-averaged Group:Sex), Stages 20-25 (GAMM, incl. light-phase",
      "SIS-CON contrasts), Stage 28 (four-domain composites at 10 and 5 min)."),
    design_documents = I(c("AUDIT_REPORT.md", "DESIGN_REVISION_2026-09-27.md", "PREFREEZE_CHECKS_A-K_2026-09-27.md",
                           "evidence/implementation_2026-09-27/config_review_findings.txt")),
    change_rule = list(new_config_version_required = TRUE, technical_reason_in = "change_log", old_version_preserved = TRUE,
                       required_label = "changed after outcome inspection",
                       allowed_triggers = I(c("model failure (fitting$failure_rule)", "prespecified diagnostic (diagnostics$zone_variance)"))),
    change_log = list()
  ),

  # ---------------------------------------------------------------- population
  population = list(
    rfid_cohort = "111 RFID-tracked animals (of 117 with CombZ); 6 B1 males never tracked (0001, 0002, OQ750-OQ753)",
    primary = list(
      id = "SIS_ONLY", definition = "animals not on Analysis/con_animals.csv (outcome_group RES or SUS)", expected_n = 87,
      expected_by_sex_group = list(Female = list(RES = 28, SUS = 18), Male = list(RES = 25, SUS = 16)),
      rationale = "the RES/SUS question is within SIS; the CON cage structure must not drive the primary random effects",
      decision_basis = "DESIGN"),
    exposure = list(id = "ALL_RFID", definition = "all 111 RFID animals; CON vs SIS", expected_n = 111,
                    role = "secondary estimation (stress-exposure question)", decision_basis = "DESIGN"),
    expected_counts = list(
      shared_zone_use = list(CC1_animals = 85, windows = 345, CC1_cage_clusters = list(pooled = 22, Female = 12, Male = 10)),
      other_constructs = list(CC1_animals = 87, windows = 348, CC1_cage_clusters = list(pooled = 24, Female = 12, Male = 12))),
    group_source = "canonical/later_outcome_combz (outcome_group); equals Analysis/sus_animals.csv + con_animals.csv",
    exclusions_upstream = "raw_data/excluded_animals.csv applied by preprocessing (reasons undocumented); no QC-based exclusion",
    baseline = paste("none: no RFID recording exists before CC1 (P25) in any batch; earlier MMM files are hardware/naming tests or",
                     "duplicate exports; initScan files are post-change scans (verified 2026-09-27, PREFREEZE_CHECKS item D)"),
    open_interpretive_question = "whether the CC1 grouping was procedurally identical for CON and SIS (affects wording only, not design)",
    sex_batch = list(Female = I(c("B3", "B4", "B6")), Male = I(c("B1", "B2", "B5")),
                     rule = "Sex is determined by Batch; no Sex main effect is estimated alongside Batch")
  ),

  # ---------------------------------------------------------------- windows
  windows = list(
    primary = list(
      id = "ACTIVE_PHASE_AFTER_CC",
      definition = "first full active phase after each cage change: [anchor, anchor + 43,200 s) on the RFID logger clock",
      anchor = paste("18:30:00 (logger clock, tz UTC) on the calendar day of each SourceFile's first timestamp t0; stop unless",
                     "0 <= t0 - anchor < 3600 s; must equal the Stage 28 provenance anchor for 24/24 SourceFiles (runtime gate)"),
      cage_changes = I(c("CC1", "CC2", "CC3", "CC4")),
      names = list(CC1 = "first active phase after CC1", trajectory = "active-phase trajectories across CC1-CC4"),
      prohibited_names = I(c("immediate", "acute", "immediate response", "acute post-cage-change response", "acute window")),
      lag_note = "starts an estimated 2.3-8.5 h after the physical cage change (lower bound from RFID recording start)",
      decision_basis = "DESIGN"),
    light_phase = list(
      id = "LIGHT_PHASE_AFTER_ACTIVE",
      definition = "[anchor + 43,200 s, anchor + 86,400 s) following the primary active phase of the same cage change",
      coverage_rule = "each SourceFile's last timestamp must be >= anchor + 86,400 s, otherwise its light-phase windows are NA and counted in G tables",
      role = "secondary readout: exposure (CON vs SIS) and within-SIS estimates only; label 'light-phase activity', never 'sleep'",
      decision_basis = "POST_HOC_CONTEXT",
      disclosure = "added after existing Stage 23/25 SIS-vs-CON light-phase results had been seen"),
    cumulative = list(
      hours = I(c(1, 3, 6)),
      definition = "[anchor, anchor + h) of the first active phase after CC1, observed-hours denominator; CC1 only",
      metric_matrix = list(h1 = I("crossing_rate"), h3 = I(c("crossing_rate", "fragmentation")),
                           h6 = I(c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation"))),
      estimands = I(c("Q1", "RS_sexavg_CC1", "RS_by_sex_CC1")),
      report = "KR estimate + 95% CI, no p; standardised by the 12-h standardizer_sd",
      name = "first h hours of the active phase after CC1 (never 'acute' or 'early response')",
      role = "estimation-only temporal localisation; never replaces the full 12-h window", decision_basis = "DESIGN"),
    not_analysed = "the post-change afternoon (removed by preprocessing) is not part of this analysis"
  ),

  # ---------------------------------------------------------------- event stream
  event_stream = list(
    source = "preprocessed_data/E9_SIS_*_CC*_AnimalPos_preprocessed.csv (change-only RFID reads, floor-snapped PositionID)",
    seed = paste("per animal x file, the last on-grid raw read before the file's first timestamp t0, recovered from raw_data (Stage 01 rule)",
                 "and stamped at t0; the raw-label parser strips both '_sys.' and '-sys.' suffixes"),
    seed_parser_fix = "Stage 01 (Analysis/01_build_multiscale_behavior_metrics.R:605) strips only '_sys.'; the canonical stream fixes this (8 B1 CC2 animals gain a seed)",
    stage01_not_rebuilt = paste("Stage 01 bin tables are NOT rebuilt; they keep the seed bug for 8 B1 CC2 animals (slot 1 only). They are used only",
                                "for the CC1 lag block (unaffected) and by the registered Stage 09 (CC1 only, unaffected)."),
    carry_forward = paste("an animal occupies the PositionID of its last read (or seed) until its next read; after its last read in the file, until",
                          "the window end; observation starts at t0 (seeded animals) or at the first read (unseeded); a seed is never back-filled before t0"),
    event = "a change of PositionID between consecutive reads of the same animal within SourceFile x System",
    validation = list(
      pre_implementation = paste("evidence/implementation_2026-09-27/impl_01: with the Stage 01 parser every metric equals the audit's group-blind caches",
                                 "(max |d| = 0); with the fixed parser only these differ: all metrics for 3, 4, OQ754, OQ755, OQ762, OQ764, OQ772, OQ773 at CC2",
                                 "(re-seeded) and shared_zone_use for OQ770, OQ771 at CC2 (their cage-mates)"),
      runtime_gates = I(c("event counts per active window equal the Stage 01 10-min Movement sums, except the 8 re-seeded B1 CC2 windows",
                          "anchors equal the Stage 28 provenance target_window_start for 24/24 SourceFiles",
                          "444 active windows; frozen population counts")))
  ),

  # ---------------------------------------------------------------- metrics
  metrics = list(
    crossing_rate = list(
      tier = "primary", label = "antenna-crossing rate", interpretation = "overall locomotor activity",
      unit = "crossings/hour", binning = "bin-free",
      definition = "number of events in the window / observed hours (sum of occupancy-run durations within the window)",
      standardizer_sd = 5.1698, standardizer_definition = "CC1 SIS pooled within-batch SD (crossings/hour)",
      prohibited_labels = I(c("distance", "exploration")), decision_basis = "GROUP_BLIND_MEASUREMENT",
      disclosure = "numerically equivalent to 6 x Movement_mean (<= 0.21% at CC1), whose group results had been seen (Stages 03, 09, 14, 28)"),
    shared_zone_use = list(
      tier = "primary", label = "shared antenna-zone use",
      interpretation = "relational/social-spatial overlap in antenna-zone use with current tracked cage-mates",
      unit = "fraction of dyadic observation time", binning = "bin-free",
      definition = paste("sum over tracked cage-mates of seconds both animals are assigned to the same PositionID / sum over tracked",
                         "cage-mates of seconds both are observed, within the window; NA without a tracked cage-mate"),
      standardizer_sd = 0.0607, standardizer_definition = "CC1 SIS pooled within-batch SD",
      prohibited_labels = I(c("sociability", "social coordination", "huddling", "social preference")),
      caveat = "about 95% of between-animal variance is reproduced by chance overlap of hour-by-hour zone occupancy",
      decision_basis = "GROUP_BLIND_MEASUREMENT",
      disclosure = "time-weighted analogue of Proximity_mean, whose group results had been seen (Stages 14, 28)"),
    occupancy_dispersion = list(
      tier = "secondary", label = "occupancy dispersion", unit = "bits", binning = "bin-free",
      definition = "Shannon entropy (log2; 0 log 0 = 0) of the share of window occupancy time assigned to each of the 8 PositionIDs",
      standardizer_sd = 0.2284, standardizer_definition = "CC1 SIS pooled within-batch SD",
      prohibited_labels = I(c("exploration", "nest fidelity")),
      decision_basis = "GROUP_BLIND_MEASUREMENT", disclosure = "defined after related group results had been seen"),
    fragmentation = list(
      tier = "secondary", label = "fragmentation", unit = "proportion of bouts", binning = "bout criterion (no bins)",
      definition = "share of activity bouts that contain exactly one crossing",
      bout_criterion_s = 39.3970988275363,
      bout_criterion_rule = paste("intersection of the weighted components of a 2-component normal mixture on pooled log10 inter-event",
                                  "intervals (all 444 active windows, both sexes; mclust model 'V'); rule declared before fitting;",
                                  "value frozen as this constant (computed on the Stage 01-parser stream) and never re-estimated"),
      bout_rule = "a new bout starts at the first event of the window and at every event whose gap to the previous event is > criterion",
      standardizer_sd = 0.0775, standardizer_definition = "CC1 SIS pooled within-batch SD",
      prohibited_labels = I("impulsivity"),
      decision_basis = "GROUP_BLIND_MEASUREMENT", disclosure = "defined after related group results had been seen"),
    light_phase_crossing_rate = list(
      tier = "secondary", label = "light-phase activity", unit = "crossings/hour", window = "LIGHT_PHASE_AFTER_ACTIVE", binning = "bin-free",
      definition = "as crossing_rate, in the light-phase window", standardizer = "none (raw units only)",
      animal_term = "(1 | AnimalID) in longitudinal models (the cc1 slope evidence is active-phase only)",
      prohibited_labels = I(c("sleep", "rest")), decision_basis = "POST_HOC_CONTEXT",
      legacy = "Stage 23/25 light-phase GAMM/Markov results are legacy support only (no cage term; 3 CON cages per sex); they feed no product"),
    descriptive = list(
      events_per_bout = "mean crossings per bout at the bout criterion",
      bout_decomposition = paste("crossing rate = bouts/hour x events/bout, at criteria 19.6985494137682, 39.3970988275363, 78.7941976550726,",
                                 "149.84699971049 (2-process log-survivorship) and 1341.67100391129 s (3-process long-rest boundary), all from",
                                 "evidence/design_phase_A2/bout-occupancy-study/02_bout_criteria.csv"),
      lag_block = list(
        Movement_rmssd = list(primary = "10min", sensitivity = "5min"),
        Movement_acf1 = list(primary = "10min", sensitivity = "5min"),
        Proximity_rmssd = list(primary = "5min", sensitivity = "10min"),
        Proximity_acf1 = list(primary = "5min", sensitivity = "10min"),
        estimator = "adjacency-aware RMSSD/ACF1 (pairs of consecutive slots only; Functions/first_night_domain_helpers.R) on Stage 01 bins, as in Stage 28",
        scope = "CC1 only (Stage 01 CC1 bins are unaffected by the seed bug)",
        models = "CC1_POOLED and CC1_BY_SEX formulas; estimands Q1, RS_sexavg_CC1, RS_by_sex_CC1; KR estimate + 95% CI; no p",
        role = "descriptive; estimation only",
        decision_basis = list(Movement = "POST_HOC_JUDGEMENT", Proximity = "POST_HOC_JUDGEMENT"),
        disclosure = "resolutions are disclosed judgements; Proximity 5 min is a post-hoc change from Stage 28's single 10-min setting; group results at 5 and 10 min had been seen (Stages 14, 28)"),
      retired = I(c("Entropy_mean", "Entropy_rmssd", "Entropy_acf1 (kept only inside the Stage 09 registry)",
                    "Stage 28 composites (kept as frozen legacy output)"))),
    exploratory = list(hmm = "Stage 08/14 HMM state architecture at 10 min; no first-night HMM; not exported as a result")
  ),

  # ---------------------------------------------------------------- coding
  coding = list(
    g_RS = "+1/2 RES, -1/2 SUS (primary population only)",
    g_SIS = "+1/2 SIS (RES or SUS), -1/2 CON (exposure analysis)",
    sex_c = "+1/2 Female, -1/2 Male",
    Batch = "factor, treatment coding; droplevels() inside every subset (strata, LOBO, S18, D4, complete case)",
    CageChange = "c2, c3, c4 treatment dummies (CC1 reference); categorical in every fixed part",
    cc1 = "CC index - 1 (random-slope covariate for the active-phase crossing rate only; origin CC1)",
    d1_d4 = "indicator of CC k (k = 1..4), used by D3 only",
    conCage = "1 if the cage epoch is CON-only (exposure analysis)", sisCage = "1 - conCage",
    isCON = "1 for CON animals (persistent-CON-group term, exposure longitudinal)",
    Session = "Batch:CC (24 levels), used by S3 only",
    CageEpisodeID = "Batch|System|CC (physical cage in one cage-change epoch)",
    units = "raw units in every model; standardized = raw / standardizer_sd (supplementary)"
  ),

  # ---------------------------------------------------------------- fitting
  fitting = list(
    engine = "lmerTest::lmer, REML = TRUE",
    control = "lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e5), check.rankX = 'stop.deficient')",
    df_method = "Kenward-Roger for every lmer-based test and CI; other rows use the reference declared in their own entry",
    ci_method = "estimate +/- qt(0.975, df_KR) x SE_KR from lmerTest::contest(ddf = 'Kenward-Roger', confint = TRUE); OLS rows: classical t",
    rank_rule = paste("before fitting, X = model.matrix(nobars(formula), droplevels(data)) must have rank == ncol == expected_rank;",
                      "lmer fitted with check.rankX = 'stop.deficient'; stop if getME(fit, 'X') has col.dropped;",
                      "never wrap fits in suppressMessages/suppressWarnings; capture and store every message"),
    singular_rule = paste("the frozen random structure of every model, including (1 + cc1 || AnimalID), conCage/sisCage and (0 + isCON | Batch),",
                          "is kept in every pooled, stratified, LOBO and sensitivity fit even when singular; singularity is logged"),
    optimizer_check = paste("refit from the stored formula and data with lme4's five built-in allFit optimizers (bobyqa, Nelder_Mead, nlminbwrap,",
                            "nloptwrap NLOPT_LN_NELDERMEAD and NLOPT_LN_BOBYQA; mmm_ci_optimizer_check, because lme4::allFit's update() cannot",
                            "re-evaluate these calls); run for the crossing-rate slope models TR_POOLED and TR_BY_SEX (F, M) always, and for",
                            "every lmer fit with a convergence warning; agreement = among optimizers without a convergence warning, logLik",
                            "range < 1e-6 and, for every fixed effect, range across optimizers < 0.01 x its bobyqa SE; the relative",
                            "difference range / |mean| is reported only (ill-conditioned near 0: replaced before the freeze after the",
                            "permuted-label dry run, evidence/implementation_2026-09-27/dryrun)"),
    failure_rule = paste("a fit fails if it errors, or if a convergence warning persists and allFit optimizers disagree (optimizer_check);",
                         "if converging optimizers agree, the bobyqa fit is used and the warning logged; a failed row is reported FAILED",
                         "with no estimate or p, the family keeps its declared m, and any respecification requires a new config_version"),
    no_terms = I(c("Sex main effect", "System", "Batch x anything (primary)", "TimeHours", "Phase", "AR(1)",
                   "random slopes except the active-phase crossing rate", "Animal random effect at CC1"))
  ),

  # ---------------------------------------------------------------- models
  models = list(
    CC1_POOLED = list(population = "SIS_ONLY", window = "CC1",
      formula = "y ~ Batch + g_RS + g_RS:sex_c + (1 | CageEpisodeID)", expected_rank = 8,
      notes = "one row per animal; NO Animal random effect", decision_basis = "DESIGN"),
    CC1_BY_SEX = list(population = "SIS_ONLY", window = "CC1", strata = I(c("Female", "Male")),
      formula = "y ~ Batch + g_RS + (1 | CageEpisodeID)", expected_rank = 4,
      role = "localisation / estimation; never a formal test of sex difference",
      run_rule = "fitted and reported for all four constructs regardless of the pooled result; never conditional on significance",
      decision_basis = "DESIGN"),
    TR_POOLED = list(population = "SIS_ONLY", window = "CC1-CC4",
      formula = paste("y ~ Batch + g_RS + c2 + c3 + c4 + g_RS:(c2 + c3 + c4) + g_RS:sex_c +",
                      "(c2 + c3 + c4):sex_c + g_RS:(c2 + c3 + c4):sex_c + (1 | AnimalID) + (1 | CageEpisodeID)"),
      formula_crossing_rate = paste("y ~ Batch + g_RS + c2 + c3 + c4 + g_RS:(c2 + c3 + c4) + g_RS:sex_c +",
                      "(c2 + c3 + c4):sex_c + g_RS:(c2 + c3 + c4):sex_c + (1 + cc1 || AnimalID) + (1 | CageEpisodeID)"),
      expected_rank = 20,
      random_slope = list(decision_basis = "GROUP_BLIND_MEASUREMENT",
        evidence = paste("group-blind SIS nuisance fits: uncorrelated slope vs intercept-only LR 21.7, dBIC -15.8 (F -7.1, M -3.2);",
                         "covariance not supported (dBIC +5.7, F +5.2, M +3.6); centring at CC 2.5 rejected (implies U-shaped variance)")),
      decision_basis = "DESIGN"),
    TR_BY_SEX = list(population = "SIS_ONLY", window = "CC1-CC4", strata = I(c("Female", "Male")),
      formula = "y ~ Batch + g_RS + c2 + c3 + c4 + g_RS:(c2 + c3 + c4) + (1 | AnimalID) + (1 | CageEpisodeID)",
      formula_crossing_rate = "y ~ Batch + g_RS + c2 + c3 + c4 + g_RS:(c2 + c3 + c4) + (1 + cc1 || AnimalID) + (1 | CageEpisodeID)",
      expected_rank = 10, role = "localisation / estimation",
      run_rule = "fitted and reported for all four constructs regardless of the pooled result; never conditional on significance",
      decision_basis = "DESIGN"),
    EXPOSURE_CC1 = list(population = "ALL_RFID", window = "CC1", role = "secondary estimation only (no p-values)",
      formula = "y ~ Batch + g_SIS + g_SIS:sex_c + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID)", expected_rank = 8,
      formula_by_sex = "y ~ Batch + g_SIS + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID)", expected_rank_by_sex = 4,
      caveat = "CON is cage-segregated: 3 CON cages per sex; SIS-CON is a between-cage contrast", decision_basis = "DESIGN"),
    EXPOSURE_TR = list(population = "ALL_RFID", window = "CC1-CC4", role = "secondary estimation only (no p-values)",
      formula = paste("y ~ Batch + g_SIS + c2 + c3 + c4 + g_SIS:(c2 + c3 + c4) + g_SIS:sex_c + (c2 + c3 + c4):sex_c +",
                      "g_SIS:(c2 + c3 + c4):sex_c + (1 | AnimalID) + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID) + (0 + isCON | Batch)"),
      expected_rank = 20,
      formula_by_sex = paste("y ~ Batch + g_SIS + c2 + c3 + c4 + g_SIS:(c2 + c3 + c4) + (1 | AnimalID) + (0 + conCage | CageEpisodeID) +",
                             "(0 + sisCage | CageEpisodeID) + (0 + isCON | Batch)"),
      expected_rank_by_sex = 10,
      crossing_rate_animal_term = "(1 + cc1 || AnimalID)", decision_basis = "DESIGN"),
    CONTINUOUS = list(role = "secondary estimands (continuous view)", window = "CC1",
      formula = "CombZ ~ Batch + x + x:sex_c", formula_by_sex = "CombZ ~ Batch + x", expected_rank = 8, expected_rank_by_sex = 4,
      predictors = I(c("crossing_rate", "shared_zone_use")), populations = I(c("ALL_RFID", "SIS_ONLY")),
      engine = "OLS (classical t, n - 8 df); robustness: clubSandwich CR2 by CC1 CageEpisodeID with Satterthwaite df",
      report = "estimate + 95% CI; CR2 SE as robustness; no p",
      relation = "not independent of the RES/SUS characterisation or of Stage 09 (same animals, same CombZ)", decision_basis = "DESIGN")
  ),

  # ---------------------------------------------------------------- contrasts
  contrasts = list(
    Q1 = list(model = "CC1_POOLED", L = list(`g_RS:sex_c` = 1), meaning = "(RES - SUS)_female - (RES - SUS)_male", test = "KR t"),
    Q2b = list(model = "TR_POOLED", L_rows = I(c("g_RS:c2:sex_c", "g_RS:c3:sex_c", "g_RS:c4:sex_c")),
               meaning = "does the RES/SUS trajectory across cage changes differ by sex", test = "joint KR F (3 df)"),
    Q2b_components = list(model = "TR_POOLED", meaning = "each g_RS:ck:sex_c (change of DiD from CC1 to CCk)", role = "estimate + 95% CI (interprets Q2b)"),
    Q2c = list(model = "TR_POOLED", L = list(`g_RS:sex_c` = 1, `g_RS:c2:sex_c` = 0.25, `g_RS:c3:sex_c` = 0.25, `g_RS:c4:sex_c` = 0.25),
               meaning = "CC-averaged sex moderation", role = "secondary estimand; estimate + 95% CI; no p",
               decision_basis = "DESIGN", group_results_seen = "Stage 14 SIS heatmap Group:Sex (CC-averaged)"),
    Q2a = list(model = "TR_POOLED", L_rows = I(c("g_RS:c2", "g_RS:c3", "g_RS:c4")),
               meaning = "sex-averaged RES/SUS change from CC1 to CCk", role = "secondary estimand; estimate + 95% CI per component; no joint test, no p"),
    RS_sexavg_CC1 = list(model = "CC1_POOLED", L = list(g_RS = 1), role = "secondary estimand; estimate + 95% CI; no p"),
    RS_sexavg_by_CC = list(model = "TR_POOLED", meaning = "sex-averaged RES-SUS at CC1 = g_RS; at CCk = g_RS + g_RS:ck", role = "estimate + 95% CI; no p"),
    DiD_by_CC = list(model = "TR_POOLED", meaning = "k = 1: g_RS:sex_c; k = 2-4: g_RS:sex_c + g_RS:ck:sex_c", role = "estimate + 95% CI (interprets Q2b); no p"),
    RS_by_sex_CC1 = list(model = "CC1_BY_SEX", L = list(g_RS = 1), role = "follow-up / localisation; p with Holm over the 2 sexes (FU-CC1)"),
    RS_by_sex_by_CC = list(model = "TR_BY_SEX", meaning = "RES-SUS at CC1 = g_RS; at CCk = g_RS + g_RS:ck", role = "estimate + 95% CI; no p"),
    TR_by_sex = list(model = "TR_BY_SEX", L_rows = I(c("g_RS:c2", "g_RS:c3", "g_RS:c4")), test = "joint KR F (3 df)",
                     role = "follow-up / localisation; p with Holm over the 2 sexes (FU-TR)"),
    group_means = list(model = "CC1_BY_SEX and TR_BY_SEX",
                       L = "(Intercept) + equal-weight mean of the within-sex Batch effects (+ ck) +/- 1/2 g_RS (+/- 1/2 g_RS:ck)",
                       role = "estimate + 95% CI for Figure 1c/d; per-sex means and SEs come only from stratified fits"),
    Sex_by_CC = list(model = "S3", L_rows = I(c("c2:sex_c", "c3:sex_c", "c4:sex_c")), role = "estimate + 95% CI only (nuisance batch-set x CC)"),
    SIS_minus_CON = list(models = I(c("EXPOSURE_CC1", "EXPOSURE_TR")),
      constructs = I(c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation", "light_phase_crossing_rate")),
      estimands = list(SC_sexavg_CC1 = "EXPOSURE_CC1 g_SIS", DiD_SC_CC1 = "EXPOSURE_CC1 g_SIS:sex_c",
                       SC_by_sex_CC1 = "g_SIS in EXPOSURE_CC1 formula_by_sex", SC_sexavg_by_CC = "EXPOSURE_TR g_SIS (CC1); g_SIS + g_SIS:ck",
                       DiD_SC_by_CC = "EXPOSURE_TR g_SIS:sex_c (CC1); g_SIS:sex_c + g_SIS:ck:sex_c",
                       SC_by_sex_by_CC = "EXPOSURE_TR formula_by_sex g_SIS (CC1); g_SIS + g_SIS:ck"),
      report = "KR estimate + 95% CI; no p-value"),
    light_phase_within_SIS = list(models = I(c("CC1_POOLED", "CC1_BY_SEX", "TR_POOLED", "TR_BY_SEX")),
      estimands = I(c("RS_sexavg_CC1", "Q1", "RS_by_sex_CC1", "RS_by_sex_by_CC")), report = "KR estimate + 95% CI; no p"),
    CON_contrasts_policy = "RES-CON and SUS-CON are not tested; CON appears descriptively and in the exposure analysis"
  ),

  # ---------------------------------------------------------------- multiplicity
  multiplicity = list(
    applied_once_in = paste("Analysis/29_canonical_behavior_characterization.R (P-*, S-*, FU-* families) and Analysis/09_early_prediction_model_ladder.R",
                            "(PR1, PR3 as registered; PR2 Holm applied by the bundle writer); Analysis/16b recomputes and verifies every p_adjusted"),
    reject_rule = "p_holm (or BH q) <= alpha",
    families = list(
      `P-CC1` = list(tier = "primary", question = "CC1", members = I(c("Q1|crossing_rate", "Q1|shared_zone_use")), method = "holm", m = 2, decision_basis = "DESIGN"),
      `P-TR` = list(tier = "primary", question = "CC1-CC4", members = I(c("Q2b|crossing_rate", "Q2b|shared_zone_use")), method = "holm", m = 2, decision_basis = "DESIGN"),
      `S-CC1-ORG` = list(tier = "secondary", question = "CC1", members = I(c("Q1|occupancy_dispersion", "Q1|fragmentation")), method = "holm", m = 2, decision_basis = "DESIGN"),
      `S-TR-ORG` = list(tier = "secondary", question = "CC1-CC4", members = I(c("Q2b|occupancy_dispersion", "Q2b|fragmentation")), method = "holm", m = 2, decision_basis = "DESIGN"),
      `FU-CC1|crossing_rate` = list(tier = "follow-up", members = I(c("RS_by_sex_CC1|crossing_rate|Female", "RS_by_sex_CC1|crossing_rate|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-CC1|shared_zone_use` = list(tier = "follow-up", members = I(c("RS_by_sex_CC1|shared_zone_use|Female", "RS_by_sex_CC1|shared_zone_use|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-CC1|occupancy_dispersion` = list(tier = "follow-up", members = I(c("RS_by_sex_CC1|occupancy_dispersion|Female", "RS_by_sex_CC1|occupancy_dispersion|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-CC1|fragmentation` = list(tier = "follow-up", members = I(c("RS_by_sex_CC1|fragmentation|Female", "RS_by_sex_CC1|fragmentation|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-TR|crossing_rate` = list(tier = "follow-up", members = I(c("TR_by_sex|crossing_rate|Female", "TR_by_sex|crossing_rate|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-TR|shared_zone_use` = list(tier = "follow-up", members = I(c("TR_by_sex|shared_zone_use|Female", "TR_by_sex|shared_zone_use|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-TR|occupancy_dispersion` = list(tier = "follow-up", members = I(c("TR_by_sex|occupancy_dispersion|Female", "TR_by_sex|occupancy_dispersion|Male")), method = "holm", m = 2, gated = FALSE),
      `FU-TR|fragmentation` = list(tier = "follow-up", members = I(c("TR_by_sex|fragmentation|Female", "TR_by_sex|fragmentation|Male")), method = "holm", m = 2, gated = FALSE),
      PR1 = list(tier = "prediction", note = "Stage 09 internal registry continuity; not a preregistration", members = I("3 Spearman associations"), method = "BH", m = 3),
      PR2 = list(tier = "prediction", members = I("2 behaviour-only full-refit permutation tests"), method = "holm", m = 2),
      PR3 = list(tier = "prediction", note = "Stage 09 internal registry continuity", members = I("3 feature x Sex interactions"), method = "BH", m = 3)),
    never_pooled = I(c("CC1 with CC1-CC4", "pooled with stratified", "resolutions", "primary with secondary",
                       "sensitivity rows with anything", "Stage 09 with behavioural families")),
    estimation_only = I(c("Q2b components", "Q2c", "Q2a components", "RS_sexavg_CC1", "RS_sexavg_by_CC", "DiD_by_CC", "RS_by_sex_by_CC",
                          "group means", "Sex_by_CC", "SIS_minus_CON", "light-phase within-SIS", "continuous CombZ models", "lag block",
                          "bout decomposition", "cumulative windows"))
  ),

  # ---------------------------------------------------------------- sensitivities
  sensitivities = list(
    p_value_policy = paste("HETEROSCEDASTIC A/B and SHARED_ZONE D1 rows report p_raw plus a boolean decision indicator (Holm recomputed as",
                           "defined in their rules; not an exported q-value). Stage 09 sensitivity rows report their own statistic and p_raw without",
                           "adjustment. All other sensitivity rows (LOBO, D2, D3, D4, complete case, S2-S18) report estimate, SE, 95% CI and a",
                           "robustness label only, with no p."),
    LOBO = list(applies = "Q1, Q2b components, RS_by_sex_CC1; all four constructs",
                rule = paste("pooled estimands (Q1, Q2b components): 'robust to single-batch removal' only if the sign is stable in 6/6 refits;",
                             "within-sex RS_by_sex_CC1: 3/3 same-sex refits; otherwise name the leverage batch"),
                report = "estimates + SE only"),
    HETEROSCEDASTIC = list(
      applies = list(contrasts = I(c("Q1", "Q2b")), constructs = I(c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation"))),
      mandatory = TRUE,
      A = paste("difference of the sex-stratified estimates, all variance components sex-specific; SEs and covariances are KR-adjusted",
                "(pbkrtest::vcovAdj): Q1 d = RS_F - RS_M, SE = sqrt(se_F^2 + se_M^2); Q2b 3-df Wald on b_F - b_M of g_RS:ck with V_F + V_M"),
      A_reference = paste("Q1: t with nu = (se_F^2 + se_M^2)^2 / (se_F^4/nu_F + se_M^4/nu_M), nu_s = the CC1_BY_SEX KR df of g_RS;",
                          "Q2b: F = W/3 ~ F(3, nu), nu = min of the two TR_by_sex joint-F KR denominator df"),
      B = paste("glmmTMB(REML = TRUE, dispformula = ~ sex_c) with the primary fixed and random parts (crossing-rate longitudinal animal term",
                "diag(1 + cc1 | AnimalID)); Q1 Wald z; Q2b Wald chi-square(3); Q2b components Wald z"),
      material_dependence = list(
        holm = "Holm (m = 2) recomputed with BOTH family members' p-values from the same comparator gives a different decision for this member",
        ci_Q1 = "whether the 95% CI of Q1 excludes 0 differs",
        p_Q2b = "whether the unadjusted 3-df Q2b p < alpha differs",
        shift = paste("Q1 and each Q2b component: |comparator - primary| >= 0.5 primary SE, or signs differ with both |estimates| >= 0.5 primary SE;",
                      "for Q2b material if any component meets it")),
      if_material = "report both; state that inference is variance-model-sensitive; do not privilege the homoscedastic result",
      shared_zone_use_note = "A and B are run for shared zone use as for all constructs; the primary stays homoscedastic unless diagnostics$zone_variance indicates otherwise"),
    SHARED_ZONE = list(
      primary = "animal-level cage-aware models (CC1_POOLED, TR_POOLED) with the Holm-adjusted primary result",
      D1 = list(role = "mandatory inferential robustness",
                cc1 = "clubSandwich::vcovCR(as(CC1_POOLED lmer fit, 'lmerMod'), cluster = CageEpisodeID, type = 'CR2'); Q1 via coef_test(test = 'Satterthwaite'); 22 clusters (F 12, M 10)",
                longitudinal = paste("lm(fixed part of TR_POOLED) on the 345 shared-zone rows; sandwich::vcovCL(cluster = ~ AnimalID + CageEpisodeID,",
                                     "type = 'HC3', cadjust = TRUE, multi0 = FALSE); negative eigenvalues of V set to 0; W = b'V^-1 b over g_RS:ck:sex_c,",
                                     "F = W/3 ~ F(3, G - 1), G = min(n animals, n cage epochs)"),
                calibration = paste("within-Batch permutation null on permuted labels only, before the freeze (evidence/implementation_2026-09-27/",
                                    "d2_calibration/RESULTS.txt): rejection at nominal 5% was CC1 CR2 0.035; longitudinal HC1 0.093-0.115, HC2 0.067,",
                                    "HC3 0.030; HC1 was therefore replaced by HC3. The primary KR tests rejected 0.035-0.065."),
                support_rule = paste("Q1: replace the zone member's primary p with the D1 p in P-CC1 (partner unchanged); D1 supports the effect if",
                                     "the zone member is rejected at alpha AND the D1 estimate has the primary sign. Q2b: D1 supports if the recomputed",
                                     "P-TR Holm rejects the zone member at alpha."),
                strong_wording_rule = "strong inferential wording requires the primary Holm result AND D1 to support the effect"),
      D2 = list(role = "relational sensitivity (not a significance gate)",
                model_cc1 = "lm(frac ~ 0 + factor(CageEpisodeID) + s_RS + s_RS:sex_c); SIS dyads with both members tracked; unweighted; rank 24 (22 cages)",
                model_tr = "lm(frac ~ 0 + factor(CageEpisodeID) + s_RS + s_RS:(c2 + c3 + c4) + s_RS:sex_c + s_RS:(c2 + c3 + c4):sex_c); rank 101",
                s_RS = "RES-ness(A) + RES-ness(B), RES = +1/2, SUS = -1/2; mixed dyads (s_RS = 0) are used",
                se = paste("clubSandwich CR2 by CageEpisodeID with Satterthwaite df (single coefficients) and the HTZ Wald F for the Q2b analogue.",
                           "Replaced, before the freeze, the dyad-robust (shared-animal) sandwich, which rejected 18-21% (CC1) and 14-17% (CC1-CC4) of",
                           "within-Batch permutation-null draws at nominal 5% and was not always positive definite; CR2 by cage epoch rejected",
                           "5.8-7.5% (CC1) and 4.3-6.7% (CC1-CC4). Null calibration on permuted labels only:",
                           "evidence/implementation_2026-09-27/d2_calibration/RESULTS.txt"),
                reported = "RR_vs_SS = 2 x s_RS and DiD_RR_vs_SS = 2 x (s_RS:sex_c) at CC1; 2 x s_RS:ck:sex_c components longitudinally (additivity)",
                magnitude_scale = "implied animal-level value = beta x mean over analysed animals of (n_i - 2)/(n_i - 1), n_i = tracked animals in the cage",
                assessment = list(same_direction = "implied value has the primary estimate's sign (Q1 at CC1; each Q2b component)",
                                  broadly_compatible = "implied value inside the primary 95% CI, or implied/primary ratio in [0.5, 2]",
                                  clear_contradiction = "the D2 95% CI excludes 0 with the sign opposite to the primary estimate"),
                not_required = "D2 need not have a CI excluding 0"),
      D3 = list(formula_random = "TR_POOLED with (1 | CageEpisodeID) replaced by (0 + d1 | CageEpisodeID) + (0 + d2 | CageEpisodeID) + (0 + d3 | CageEpisodeID) + (0 + d4 | CageEpisodeID)",
                role = "robustness, not a gate"),
      D4 = list(variants = I(c("CC1_POOLED/TR_POOLED on rows in cage epochs with 4 tracked animals (rank 8/20)",
                               "CC1_POOLED/TR_POOLED + factor(n_tracked_mates) in the fixed part (rank 9/22)")),
                role = "robustness, not a gate"),
      complete_case = list(definition = "drop OQ755, OQ770, OQ771 (the 3 animals with a missing window) at every CC", report = "estimates + 95% CI only"),
      downgrade_rule = "a sensitivity labelled SIGN_CHANGE, or a D2 clear_contradiction, is reported explicitly and downgrades the interpretation"),
    OTHER = list(
      S2_batch_x_cc = "y ~ Batch*(c2 + c3 + c4) + g_RS*(c2 + c3 + c4) + g_RS:sex_c + g_RS:(c2 + c3 + c4):sex_c + <frozen random part>; rank 32",
      S3_session_re = "TR_POOLED + (1 | Session); source of the Sex_by_CC estimates",
      S7_crossing_ri_only = "crossing rate TR_POOLED with (1 | AnimalID) only",
      S7b_crossing_correlated_slope = "crossing rate TR_POOLED with (1 + cc1 | AnimalID)",
      S11_bout_criterion = list(half = 19.6985494137682, double = 78.7941976550726, sibly_2process = 149.84699971049),
      S12_reversal_filtered = "A->B->A excursions shorter than 20 s merged into their neighbours (greedy, non-overlapping); all four constructs",
      S13_hardware = paste("exclude exactly the 6 animal-windows in dead/near-dead-antenna cage epochs: 314, 318, OR620, OR630 (B5|sys.1|CC4) and",
                           "OR112, OR141 (B2|sys.2|CC3); occupancy_dispersion and shared_zone_use; animal 692 B6 CC1 (7/8 positions through low",
                           "activity) is retained; cage-mates' values are not recomputed"),
      S18_B1_excluded = "shared zone use without B1 (70% tracked)"),
    robustness_label_rule = list(
      shift_se = "(sensitivity estimate - primary estimate) / primary SE",
      labels_in_order = list(SIGN_CHANGE = "signs differ and both |estimates| >= 0.5 primary SE", LARGE_SHIFT = "|shift_se| >= 1", COMPATIBLE = "otherwise"),
      precedence = "evaluated in order SIGN_CHANGE, LARGE_SHIFT, COMPATIBLE; first match wins",
      scope = "S2, S3, S7, S7b, S11, S12, S13, S18, D3, D4, complete case; for Q2b per component g_RS:ck:sex_c; not D1, D2, LOBO, HETEROSCEDASTIC, cumulative, lag",
      note = "labels never change the primary result")
  ),

  # ---------------------------------------------------------------- diagnostics
  diagnostics = list(
    zone_variance = list(
      definition = paste("group-blind SIS nuisance fits (Group not read): y ~ Batch (CC1) and y ~ Batch + c2 + c3 + c4 + (c2 + c3 + c4):sex_c (CC1-CC4)",
                         "with the frozen random part, glmmTMB ML with vs without dispformula = ~ Sex"),
      trigger = "indicates otherwise iff boundary-free LR p < 0.05 AND dBIC < 0 in either analysis; then comparator B is reported as co-primary for shared zone use",
      pre_freeze_result = "CC1 LR 1.42 (dBIC +3.0), CC1-CC4 LR 3.22 (dBIC +2.6): NOT indicated (evidence/revision_2026-09-27/rev_02)",
      recomputed_in_stage29 = TRUE),
    reported_only = I(c("drop-one-cage influence on Q1 (range of estimates)", "within-sex batch homogeneity of RES-SUS (Batch x g_RS within sex; CC1)",
                        "residual SD by Sex x Group", "pooled_vs_stratified_diff_sd (flag > 0.10 SD)", "singularity and convergence log")),
    role = "reported; never a stop and never a reason to respecify, except zone_variance"
  ),

  # ---------------------------------------------------------------- Stage 09
  stage09 = list(
    role = "prediction view; internal analysis-registry entry (docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv), unchanged; not a preregistration",
    registered = list(resolution = "10min_based", features = I(c("Movement_mean", "Movement_rmssd", "Entropy_acf1")),
                      families = I(c("PR1", "PR2", "PR3")), population = "all 111 animals", decision_basis = "REGISTRY_CONTINUITY"),
    history_disclosure = paste("Stage 09 was created at 5-min bins (commit 946efa6, 2026-05-13; declared primary in ae44a78, 2026-05-17)",
                               "and switched to 10 min on 2026-05-20 (8e29932) without a recorded rationale; 10 min is retained",
                               "for registry continuity and is NOT presented as prospectively selected."),
    sensitivities = list(
      resolution_5min = "Stage 09 rerun with MMM_STAGE09_BIN_LEVEL = 5min_based on canonical CombZ",
      cv = "LOAO R2 (full-sample mean denominator, as Stage 09) with folds = CC1 CageEpisodeID (leave-one-cage-out) and = Batch (leave-one-batch-out)",
      permutation = "CombZ permuted within Batch; B = 1000; full LOAO refit per permutation; p = (#null >= observed + 1)/(B + 1)",
      batch_adjusted_sex = "CombZ ~ Batch + x + x:sex_c for the 3 registered features (OLS)",
      models = I(c("movement_mean", "primary_behavior_family"))),
    rng = list(kind = "Mersenne-Twister", normal_kind = "Inversion", sample_kind = "Rejection", seed = 20260811),
    removed_from_inferential_story = "pooled-sex descriptive Wilcoxon group tests (not exported)",
    resolution_choice_rule = "5 vs 10 min is never chosen by which gives the stronger result"
  ),

  # ---------------------------------------------------------------- reporting
  reporting = list(
    primary_fields = I(c("n_animals", "n_batches", "n_cage_episodes", "estimate", "se", "ci_low", "ci_high", "df", "p_raw", "p_holm",
                         "estimate_standardized", "heteroscedastic_A", "heteroscedastic_B", "variance_model_sensitive", "lobo_sign_stability",
                         "shared_zone D1 (d1_supports, strong_wording_allowed), D2 assessment, D3, D4, complete case", "family_id", "config_version")),
    q2b_rule = "Q2b rows carry n_animals, n_batches, n_cage_episodes + q2b_fields; estimate/se/ci/standardized are given per component and per DiD_by_CC",
    q2b_fields = I(c("F", "df1", "df2", "p_raw", "p_holm", "DiD_by_CC with CI", "RES-SUS by CC by sex with CI")),
    followup_fields = list(RS_by_sex_CC1 = I(c("estimate", "se", "ci_low", "ci_high", "estimate_standardized", "p_raw", "p_holm")),
                           TR_by_sex = I(c("F", "df1", "df2", "p_raw", "p_holm")), RS_by_sex_by_CC = I(c("estimate", "se", "ci_low", "ci_high"))),
    null_wording = "a non-significant sex-moderation result means imprecise data, not equivalence; always show estimate, CI and adjusted p",
    within_sex_wording = "'the estimated RES-SUS difference was larger in X, but the formal sex-moderation estimate was imprecise' when the pooled CI crosses 0",
    causal_wording = I(c("animals later classified RES/SUS differed during the first active phase after CC1",
                         "early behavioural differences associated with later resilience/susceptibility")),
    prohibited_causal_wording = I(c("CC1 induced the RES/SUS phenotype", "female-specific (unless the pooled interaction supports it)")),
    raw_units_first = TRUE
  ),

  # ---------------------------------------------------------------- Figure 1
  figure1 = list(
    panels = list(a = "experimental design and timeline", b = "CombZ and RES/SUS definition",
                  c = "CC1 primary characterisation: crossing rate, shared zone use; per-sex RES/SUS model estimates; sex-difference contrast; P-CC1 Holm p",
                  d = "CC1-CC4 trajectories: crossing rate, shared zone use; sex-stratified; Q2b P-TR Holm p",
                  e = "early crossing rate vs later CombZ (refreshed Stage 09)", f = "held-out prediction and permutation null"),
    panel_sources = list(
      c = list(points = "A1 animal values by group within sex; CON in grey, descriptive (no model)", group_means = "contrasts$group_means (CC1_BY_SEX)",
               rs_by_sex = "RS_by_sex_CC1", did = "Q1 with the P-CC1 Holm p"),
      d = list(estimates = "RS_by_sex_by_CC (TR_BY_SEX) +/- 95% CI, or group means (contrasts$group_means)", con = "CON per-CC means in grey, descriptive",
               test = "Q2b with the P-TR Holm p", legend = "Q2c is reported as a secondary estimate"),
      e = list(x = "6 x Stage 09 Movement_mean (10 min), crossings/h", statistic = "PR1 Spearman rho with bootstrap CI and BH q"),
      f = list(statistic = "Stage 09 LOAO R2, repeated-CV mean and quantiles, permutation null (PR2 Holm p)"),
      rule = "per-sex means and SEs come only from stratified fits"),
    units = list(crossing_rate = "crossings/hour", shared_zone_use = "fraction of dyadic observation time (0-1)", CombZ = "z (control-referenced)",
                 stage09_conversion = "x = 6 x Movement_mean per 10-min bin (agrees with the event-level rate within 0.21% at CC1); statistics from Stage 09 values"),
    legend_dependence = "panels c, d and e use overlapping animals and data (RES/SUS are defined from later CombZ); they are complementary views, not independent validation",
    extended_data = I(c("secondary constructs", "SIS vs CON exposure estimates incl. light-phase activity", "sensitivity analyses", "lag block",
                        "continuous CombZ estimands", "secondary estimands Q2a, Q2c, RS_sexavg", "Stage 09 model ladder and sex interaction"))
  ),

  # ---------------------------------------------------------------- ownership
  ownership = list(
    producer = I(c("Analysis/29_canonical_behavior_characterization.R (characterisation, continuous, exposure, Stage 09 sensitivities)",
                   "Analysis/09_early_prediction_model_ladder.R (registered prediction)")),
    bundle_writer = "Analysis/16b_canonical_behavior_bundle.R (Stage 16 export layer; the only bundle writer)",
    bundle_root = "analysis_ready/canonical/behavior_bundle/<bundle_id>/",
    manuscript = "Exp9_manuscript renders from the imported bundle only; it fits no model, recomputes no p-value or multiplicity, defines no exclusion and selects no resolution or window"
  )
)

# Canonical serialisation and hash (the single definition used by the freeze script, Stage 29 and Stage 16b).
mmm_behavior_config_json <- function(cfg = MMM_BEHAVIOR_CONFIG)
  jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
mmm_behavior_config_sha256 <- function(cfg = MMM_BEHAVIOR_CONFIG)
  digest::digest(as.character(mmm_behavior_config_json(cfg)), algo = "sha256", serialize = FALSE)
