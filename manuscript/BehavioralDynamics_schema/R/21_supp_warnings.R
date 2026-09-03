# ==============================================================================
# 21_supp_warnings.R
# Right-column continuation of the supplementary provenance schema: the
# critical implementation warnings and the recommended manuscript inference.
# Sourced from 20_supplementary_provenance_schema.R; operates on p / yR / XR.
#
# Every claim here is reproducible from the exported tables by
#   R/00_verify_provenance.R, R/01_verify_rest_domain_collapse.R,
#   R/02_verify_zero_fill_mechanism.R
# ==============================================================================

p <- sec(p, XR, yR, CW, "CRITICAL IMPLEMENTATION WARNINGS",
         "verified against exported outputs", accent = P$sus)
yR <- yR - 7.2
p <- p + stext(XR, yR, wrp("These describe the CURRENTLY GENERATED outputs. They were reproduced from the exported tables, not inferred from reading the code alone.", 4.4, CW),
               size = 4.4, colour = P$ink_soft)
yR <- yR - 4.6

# --- W1 -----------------------------------------------------------------------
p <- p +
  sbox(XR, yR - 27.0, CW, 27.0, fill = P$warn_bg, colour = P$sus, linewidth = 0.45) +
  sbox(XR, yR - 27.0, 1.0, 27.0, fill = P$sus, colour = P$sus, linewidth = 0.45) +
  stext(XR + 2.8, yR - 1.4, "W1   Stage 12 phase parser: “Inactive” matches “active”",
        size = 5.0, fontface = "bold", colour = P$sus) +
  scode(XR + 2.8, yR - 4.8,
    "PhaseClass = case_when(\n  str_detect(str_to_lower(Phase), \"active|dark|night\")  ~ \"Active\",   # tested FIRST\n  str_detect(str_to_lower(Phase), \"inactive|light|day\") ~ \"Inactive\",\n  TRUE ~ as.character(Phase))",
    size = 4.2, colour = P$ink) +
  stext(XR + 2.8, yR - 14.2,
        wrp("str_detect is unanchored and “inactive” contains “active”, so every Inactive epoch is relabelled Active. All 444 Stage-12 feature rows carry PhaseClass = “Active”. The identical defect is live in 11_behavioral_adaptation_kinetics.R:86-90. 13_ethological_phase_organization.R:63-67 tests Inactive FIRST and is the correct fix template.", 4.3, CW - 5.4),
        size = 4.3, colour = P$ink) +
  stext(XR + 2.8, yR - 24.8, "12_sleep_like_quiescence_metrics.R:60-64", size = 4.1, colour = P$ink_faint)
yR <- yR - 29.2

# --- W2 -----------------------------------------------------------------------
p <- p +
  sbox(XR, yR - 31.0, CW, 31.0, fill = P$warn_bg, colour = P$sus, linewidth = 0.45) +
  sbox(XR, yR - 31.0, 1.0, 31.0, fill = P$sus, colour = P$sus, linewidth = 0.45) +
  stext(XR + 2.8, yR - 1.4, "W2   The rest-like row currently carries NO quiescence information",
        size = 5.0, fontface = "bold", colour = P$sus) +
  stext(XR + 2.8, yR - 4.8,
        wrp("Stage 14 joins the Stage-12 features on PhaseClass (14:5241). Because W1 labels them all Active, no Inactive epoch receives them: 0 of 444 Inactive rows have inactivity_fraction. safe_scale then converts that total absence into exactly 0 — not NA — for all 444 rows. The domain collapses to:", 4.3, CW - 5.4),
        size = 4.3, colour = P$ink) +
  scode(XR + 2.8, yR - 14.0,
        "rest row  ==  (Movement_acf1_z − Movement_rmssd_z) / 3", size = 4.6, colour = P$sus) +
  stext(XR + 2.8, yR - 17.4,
        wrp("Verified against the exported values: max |exported − collapsed| = 1.1e-16 over 444 rows, and correlation with (Movement_acf1_z − Movement_rmssd_z) = 1.0000000000. The row labelled rest-like organisation is currently movement persistence minus movement volatility, shrunk threefold. It contains no inactivity, bout-length or fragmentation information at all.", 4.3, CW - 5.4),
        size = 4.3, colour = P$ink) +
  stext(XR + 2.8, yR - 28.8, "R/02_verify_zero_fill_mechanism.R", size = 4.1, colour = P$ink_faint)
yR <- yR - 33.2

# --- W3..W9 -------------------------------------------------------------------
wrn <- list(
  c("W3   The volatility row has three different effective scalings",
    "353 Active epochs average all five inputs. 88 Active epochs have the quiescence terms as NA, which score_mean drops, giving a three-term mean. All 441 complete Inactive epochs have them as zero, which score_mean RETAINS, giving (sum of the three real terms)/5 — exactly 0.6× the three-term mean. The same plotted row is therefore on different scales within and between facets."),
  c("W4   Repeated cage-change observations are pooled as if independent",
    "sis_domain_scores holds one row per Animal × CageChange × Phase × Domain, and domain_effect_summary never aggregates over CageChange, so each animal contributes up to four rows to a single Welch test. In the Female × Active × Social spatial organization slice the exported n_ref / n_comp of 48 / 96 / 88 are ROW counts for 12 / 24 / 22 ANIMALS — exactly 4× inflation. Hedges’ g takes its df from those inflated counts and its pooled SD absorbs within-animal variance, so g is not a between-animal standardized difference."),
  c("W5   The FDR family is cross-domain",
    "p.adjust runs after group_by(PhaseClass, Sex) with Domain deliberately dropped, so one correction family spans every domain within a sex × phase facet."),
  c("W6   Colour and symbol assume different variance models",
    "The tile colour is Hedges’ g, computed with a POOLED SD; the symbol comes from a Welch t-test, which explicitly does not assume equal variances. Both are applied to the same two vectors."),
  c("W7   The code label says “circadian”; the manuscript policy forbids it",
    "The domain is named “Inactive-phase rest/circadian regulation” in code, yet nothing in the formula estimates period, amplitude, phase or acrophase. Renaming to “Inactive-phase rest-like organisation” touches eight lines and has zero numerical consequence."),
  c("W8   The 20th-percentile inactivity threshold is inert",
    "56.4% of Movement values in the 10-min input are exactly 0 and the smallest per-animal zero fraction is 0.464, so the 20th percentile is 0 for all 111 animals. inactive_like reduces to Movement == 0, the realised inactivity_fraction is about 0.55 rather than 0.20, and any quantile in (0, 0.564] gives byte-identical output."),
  c("W9   Behavioral state architecture is not always HMM-derived",
    "When hmm_occupancy is empty, 14:5329 silently substitutes mean(flexibility, volatility), making that row a linear combination of two other plotted rows. Where the HMM path IS used, Sex is absent from the summarised frame and any_of() silently drops it, so this row alone is standardized with the sexes pooled.")
)
for (wv in wrn) {
  p <- p + stext(XR, yR, wv[1], size = 4.8, fontface = "bold", colour = P$sus)
  s <- wrp(wv[2], 4.3, CW)
  p <- p + stext(XR, yR - 2.6, s, size = 4.3, colour = P$ink)
  yR <- yR - 2.6 - length(strsplit(s, "\n", fixed = TRUE)[[1]]) * 1.72 - 2.0
}

p <- p + callout(XR, yR - 11.4, CW, 11.4,
  wrp("manuscript/Fig1_behavior_candidates/figure_manifest.csv already marks panel active_inactive_systems as blocked_pending_phase_classifier_fix_and_rerun. Do not present the inactive/rest branch as publication-final until Stage 12 is corrected and Stages 12 and 14 are rerun.", 4.3, CW - 4.4),
  kind = "warn", size = 4.3, title = "STATUS: BLOCKED PENDING FIX AND RERUN")
yR <- yR - 13.6

# --- current vs recommended ---------------------------------------------------
p <- sec(p, XR, yR, CW, "CURRENT IMPLEMENTATION   vs   RECOMMENDED INFERENCE", NULL, accent = P$con)
yR <- yR - 7.0
hw <- (CW - 4) / 2
p <- p +
  sbox(XR, yR - 27.0, hw, 27.0, fill = P$panel, colour = P$rule, linewidth = 0.3) +
  stext(XR + 1.8, yR - 1.6, "CURRENT heatmap", size = 4.8, fontface = "bold", colour = P$ink_faint) +
  stext(XR + 1.8, yR - 4.8, wrp("Welch two-sample t-test on Animal × CageChange rows pooled within Group. BH FDR within Sex × PhaseClass, across domains. Hedges’ g from the same pooled rows supplies the colour; the symbol comes from that Welch q.", 4.3, hw - 3.4), size = 4.3) +
  stext(XR + 1.8, yR - 19.0, wrp("Repeated measures are treated as independent, so the symbols are anti-conservative.", 4.3, hw - 3.4), size = 4.3, colour = P$sus) +
  sbox(XR + hw + 4, yR - 27.0, hw, 27.0, fill = P$ok_bg, colour = P$ok_edge, linewidth = 0.3) +
  stext(XR + hw + 5.8, yR - 1.6, "RECOMMENDED for the manuscript", size = 4.8, fontface = "bold", colour = P$ok_edge) +
  scode(XR + hw + 5.8, yR - 4.8, "DomainScore ~ Group * Sex + CageChange\n              + (1 | Animal)", size = 4.2, colour = P$ink) +
  stext(XR + hw + 5.8, yR - 10.4, wrp("fitted separately within Domain × Phase, followed by model-based pairwise group contrasts. Keep a standardized effect size as the tile colour, but take the significance symbol from the repeated-measures model instead of treating cage-change observations as independent.", 4.3, hw - 3.4), size = 4.3)
yR <- yR - 29.2
p <- p + stext(XR, yR, wrp("Stage 14 already fits a repeated-measures model — extract_lmm_stats (14:5064-5126), called at 14:5367-5376 — as domain_score ~ Group * Sex * PhaseClass + CageChangeIndex + (1 | AnimalNum) via lmerTest::lmer, exported to stats_tables/systems_sis_domain_mixed_model_stats.csv. It is NOT the source of the heatmap symbols. Its terms agree with the descriptive pattern: for Social spatial organization the significant effects are the Inactive interactions (GroupRES:PhaseClassInactive estimate −1.564, q = 1.3e-6).", 4.3, CW),
               size = 4.3, colour = P$ink_soft)
yR <- yR - 12.0
