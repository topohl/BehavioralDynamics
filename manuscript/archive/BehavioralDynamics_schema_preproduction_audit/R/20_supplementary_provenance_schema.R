# ==============================================================================
# 20_supplementary_provenance_schema.R
# BehavioralDynamics - SUPPLEMENTARY provenance schema.
# 297 x 420 mm (A3 portrait), two columns.
#
# Full equations, definitions, input tables, source scripts, caveats and one
# worked numerical example using REAL exported values.
#
# Everything here was traced to source in
#   C:/Users/topohl/Documents/GitHub/MMMSociability/Analysis
# and adversarially re-verified. Numerical claims about the rest-like row and
# the volatility row were reproduced independently by
#   R/01_verify_rest_domain_collapse.R and R/02_verify_zero_fill_mechanism.R
# ==============================================================================

ROOT <- "C:/Users/topohl/Documents/GitHub/MMMSociability/manuscript/archive/BehavioralDynamics_schema_preproduction_audit"
source(file.path(ROOT, "R", "schema_toolkit.R"))

W <- 297; H <- 420
P <- SCHEMA_PAL
M  <- 8
CW <- (W - 2 * M - 13) / 2      # column width
XL <- M                          # left column x
XR <- M + CW + 13                # right column x

p <- schema_canvas(c(0, W), c(0, H))

# --- generic building blocks --------------------------------------------------

# section header inside a column
sec <- function(pp, x, y, w, title, sub = NULL, accent = P$con, size = 6.6) {
  pp <- pp +
    sbox(x, y - 5.6, w, 5.6, fill = accent, colour = accent, linewidth = 0.3) +
    stext(x + 1.6, y - 2.8, title, size = size, hjust = 0, vjust = 0.5,
          colour = "white", fontface = "bold")
  if (!is.null(sub))
    pp <- pp + stext(x + w - 1.6, y - 2.8, sub, size = size - 1.6, hjust = 1,
                     vjust = 0.5, colour = "white")
  pp
}

# a labelled provenance row: KEY | wrapped value. Returns the new cursor y.
prow <- function(pp, x, y, w, key, val, size = 4.5, kw = 20, colour = P$ink) {
  s  <- wrp(val, size, w - kw)
  nl <- length(strsplit(s, "\n", fixed = TRUE)[[1]])
  pp <- pp +
    stext(x, y, key, size = size, hjust = 0, vjust = 1, colour = P$ink_faint, fontface = "bold") +
    stext(x + kw, y, s, size = size, hjust = 0, vjust = 1, colour = colour)
  list(p = pp, y = y - nl * size * 0.395 - 0.42)
}

# a full provenance block for one quantity
prov <- function(pp, x, y, w, title, accent, eq, rows, src) {
  h_est <- 0
  pp <- pp +
    stext(x, y, title, size = 5.6, fontface = "bold", colour = accent) +
    stext(x + w, y, src, size = 4.1, hjust = 1, vjust = 1, colour = P$ink_faint)
  y <- y - 3.2
  eqs <- wrp(eq, 4.6, w - 2.0, family = "Consolas")
  nl  <- length(strsplit(eqs, "\n", fixed = TRUE)[[1]])
  pp <- pp +
    sbox(x, y - nl * 1.95 - 1.2, w, nl * 1.95 + 1.2, fill = P$panel, colour = NA, linewidth = 0) +
    scode(x + 1.2, y - 0.6, eqs, size = 4.6, colour = accent)
  y <- y - nl * 1.95 - 2.6
  for (r in rows) {
    o <- prow(pp, x, y, w, r[1], r[2]); pp <- o$p; y <- o$y
  }
  list(p = pp, y = y - 1.0)
}

# ------------------------------------------------------------------ title ----
p <- p +
  stext(M, H - 2.0, "Supplementary  |  Full provenance of the continuous home-cage behavioural phenotype",
        size = 10.5, fontface = "bold") +
  stext(M, H - 8.4, "Every quantity in Fig_sis_active_inactive_domain_heatmap, traced from the RFID read to the tile colour. Repository: MMMSociability/Analysis. References are file:line.",
        size = 5.4, colour = P$ink_soft) +
  stext(M, H - 13.0, "Numbers in the worked example are real exported values. Re-applying the documented transformations to the exported table reproduces the exported domain score exactly (max |difference| = 0 over 882 rows; R/00_verify_provenance.R).",
        size = 5.0, colour = P$ink_faint) +
  sline(M, H - 15.6, W - M, H - 15.6, colour = P$ink, linewidth = 0.6)

# ==============================================================================
# LEFT COLUMN
# ==============================================================================
yL <- H - 19.0

p <- sec(p, XL, yL, CW, "LEVEL 1   Raw RFID observation", "01_build_multiscale_behavior_metrics.R")
yL <- yL - 7.4
p <- p + stext(XL, yL, wrp("The apparatus emits one row per detector read: (DateTime, AnimalID, PositionID, System). There is no continuous x/y trajectory. Occupancy intervals are reconstructed on a shared per-system event grid by last-observation-carried-forward, then split at bin and phase boundaries (01:407-453, 01:684-714).", 4.6, CW), size = 4.6, colour = P$ink_soft)
yL <- yL - 8.6

o <- prov(p, XL, yL, CW, "A.  Movement", P$movement,
  "PositionChanged = is.finite(PrevPositionID) & PositionID != PrevPositionID\nMovement        = sum(MovementEvent)   # MovementEvent == 1, per bin",
  list(
    c("ENTERS", "Which discrete reader/compartment the animal was detected at, and the order of those detections."),
    c("MATH", "Count of rows where PositionID differs from the animal's previous read, within (SourceFile, Batch, CageChange, System, AnimalID) ordered by time."),
    c("WHY", "Locomotor magnitude is the most direct behavioural output, and the baseline against which more complex temporal or spatial organisation must be distinguished."),
    c("HIGH", "Many compartment transitions: the animal repeatedly relocates."),
    c("LOW", "Few transitions: repeatedly detected at the same reader, or rarely detected."),
    c("LOST", "Dwell time, path taken, distance, and every repeated read at the same reader. The first read of each session can never be an event (is.finite guard)."),
    c("UNIT", "One RFID read that differs in PositionID from the preceding read."),
    c("NOTE", "The historical long-gap invalidation rule is retired (exclude_long_gaps_from_metrics = FALSE, 01:107-122): a long silence before a move is the expected signature of an animal that stayed put, not evidence the move is invalid. MovementDistance (summed Manhattan grid distance) exists but is NOT the backbone of this figure.")),
  "01:30, 01:455-490, 01:784")
p <- o$p; yL <- o$y

o <- prov(p, XL, yL, CW, "B.  Spatial entropy", P$entropy,
  "calc_entropy <- function(seconds_by_position) {\n  p <- seconds_by_position / sum(seconds_by_position)\n  -sum(p * log2(p))\n}",
  list(
    c("ENTERS", "How many seconds of reconstructed occupancy the animal accumulated at each discrete reader position within the bin."),
    c("MATH", "Shannon entropy in bits over the occupancy-second proportions p_i of the positions visited in that bin."),
    c("WHY", "Two animals can make the same number of moves yet organise them very differently in space. Entropy separates spatial spread from locomotor amount."),
    c("HIGH", "Time distributed across many readers: even, diffuse spatial use."),
    c("LOW", "Time concentrated at one reader; a single occupied position gives exactly 0."),
    c("LOST", "Which reader, where it is, the order of visits, and any distance. Entropy is permutation-invariant within the bin."),
    c("UNIT", "One animal-bin of reconstructed occupancy seconds."),
    c("NOTE", "No minimum-occupancy, minimum-duration or small-sample bias correction is applied in Stage 01, so entropy from a sparsely observed bin is not down-weighted.")),
  "01:32, 01:182-187, 01:768")
p <- o$p; yL <- o$y

o <- prov(p, XL, yL, CW, "C.  Proximity", P$proximity,
  "ProximityFraction = safe_divide(ProximitySeconds, dyadic_observation_seconds)\nProximity         = ProximityFraction        # backward-compatible alias",
  list(
    c("ENTERS", "For every pair of animals in a system, the reconstructed seconds during which both occupied the SAME reader position."),
    c("MATH", "Same-position dyadic seconds summed across all of the focal animal's partners, divided by the dyadic observation seconds actually available in that bin."),
    c("WHY", "Raw contact seconds grow with observation time; the fraction makes unequal windows comparable. safe_divide returns NA when the denominator is not finite or <= 0."),
    c("HIGH", "A large share of observable social time spent co-located with cage mates."),
    c("LOW", "Little co-location, or little observable dyadic time."),
    c("LOST", "Partner identity, directionality, who approached whom, and any distinction between affiliation and mere co-use of a resource."),
    c("UNIT", "One animal-bin, aggregated over all available dyads."),
    c("NOTE", "This is a social-spatial CO-LOCATION proxy, not sociability or social preference. AdjacentProximityFraction (adjacent rather than same position) is computed but not used here.")),
  "01:33-37, 01:492-546, 01:790-827")
p <- o$p; yL <- o$y

# ---- LEVEL 2 -----------------------------------------------------------------
p <- sec(p, XL, yL, CW, "LEVEL 2   Multiscale temporal binning", "01 emits all; 14 selects")
yL <- yL - 7.4
p <- p + stext(XL, yL, wrp("Stage 01 writes a canonical metric table at 10 s, 1 min, 5 min, 10 min, 30 min and whole-phase resolution. Stage 14 declares primary_bin_level <- \"5min_based\" (14:72) and a per-domain preference list (domain_bin_preference, 14:79-95).", 4.6, CW), size = 4.6, colour = P$ink_soft)
yL <- yL - 7.0
tb <- list(c("SIS epoch backbone (this figure)", "5 min", "Movement / Entropy / Proximity mean, RMSSD, ACF1"),
           c("Quiescence import (Stage 12)", "10 min", "inactivity_fraction, bout length, fragmentation, transition rate"),
           c("HMM import (Stage 08)", "10 min", "state occupancy, state entropy, state fractions"),
           c("Temporal-flexibility domains", "10 s preferred", "not used by the seven plotted rows"))
p <- p +
  sbox(XL, yL - 4.0 - 3.6 * length(tb), CW, 4.0 + 3.6 * length(tb), fill = P$panel, colour = P$rule, linewidth = 0.25) +
  stext(XL + 1.4, yL - 1.0, "component", size = 4.3, fontface = "bold", colour = P$ink_faint) +
  stext(XL + 52, yL - 1.0, "bin", size = 4.3, fontface = "bold", colour = P$ink_faint) +
  stext(XL + 66, yL - 1.0, "what it contributes", size = 4.3, fontface = "bold", colour = P$ink_faint)
for (i in seq_along(tb)) {
  yy <- yL - 4.2 - (i - 1) * 3.6
  p <- p +
    stext(XL + 1.4, yy, tb[[i]][1], size = 4.4) +
    stext(XL + 52, yy, tb[[i]][2], size = 4.4, fontface = "bold", colour = P$con) +
    stext(XL + 66, yy, tb[[i]][3], size = 4.3, colour = P$ink_soft)
}
yL <- yL - 4.0 - 3.6 * length(tb) - 2.4
p <- p + stext(XL, yL, wrp("Why bin at all? RFID events are irregular in time. Fixed bins convert an event stream into an evenly indexed series, which is what makes mean, RMSSD and ACF1 definable. \"Multiscale\" describes the pipeline's capability, not an averaging step inside any single tile.", 4.5, CW), size = 4.5, colour = P$ink_faint)
yL <- yL - 8.0

# ---- LEVEL 3 -----------------------------------------------------------------
p <- sec(p, XL, yL, CW, "LEVEL 3   Repeated perturbations \u00d7 phase", "14:5224-5241")
yL <- yL - 7.4
o <- prow(p, XL, yL, CW, "GROUPING", "group_by(AnimalNum, Group, Sex, CageChange, CageChangeIndex, PhaseClass) -> one row per animal-epoch.", kw = 22); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "DESIGN", "111 animals: CON 24 (12F/12M), RES 49 (24F/25M), SUS 38 (22F/16M). Four cage changes CC1-CC4. Two phase classes.", kw = 22); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "EPOCH SIZE", "An epoch pools every phase block of that cage change: Active up to 576 five-minute bins (4 x 12 h), Inactive up to 432 (3 x 12 h). PhaseNumber is NOT a grouping key, so repeated dark phases within a cage change are averaged together.", kw = 22); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "WHY SPLIT", "Mice are nocturnal. The same locomotor value carries different meaning in the dark and light phase, so the phases are treated as distinct biological regimes rather than pooled.", kw = 22); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "UNIT", "Animal x CageChange x PhaseClass. This is the row that becomes one observation in the final contrast.", kw = 22, colour = P$sus); p <- o$p; yL <- o$y
yL <- yL - 1.0
p <- p + callout(XL, yL - 12.4, CW, 12.4,
  wrp("Manuscript language (docs/analysis_strategy_for_manuscript.md:215-238): RFID home-cage metrics alone do not validate circadian or sleep disruption. Use \"active/inactive organisation\", \"day/night behavioural structure\", \"light-phase rest-like organisation\". Avoid \"circadian disruption\", \"sleep disruption\", \"insomnia-like\".", 4.4, CW - 4.4),
  kind = "note", size = 4.4, title = "PERMITTED LANGUAGE")
yL <- yL - 14.6

# ---- LEVEL 4 -----------------------------------------------------------------
p <- sec(p, XL, yL, CW, "LEVEL 4   Three temporal descriptions", "14:5229-5237")
yL <- yL - 7.4
d4 <- list(
  list("Mean \u2014 magnitude", "mean(x, na.rm = TRUE)",
       "Asks \u201chow much?\u201d. Permutation-invariant: it discards all ordering. No minimum-n guard, so a single surviving bin still yields a mean."),
  list("RMSSD \u2014 local volatility", "if (sum(is.finite(x)) >= 3) sqrt(mean(diff(x[is.finite(x)])^2))",
       "Asks \u201chow much does behaviour change between adjacent bins?\u201d. Root mean square of successive differences: high-pass, sign-blind, blind to slow drift. It is NOT normalised by the level, so it remains correlated with the mean. This is what separates temporal instability from simple hyper- or hypo-activity."),
  list("ACF1 \u2014 persistence / inertia", "if (sum(is.finite(x)) >= 4) safe_cor(x[-n], x[-1], \"pearson\")",
       "Asks \u201chow strongly does the current bin depend on the one before?\u201d. Location- and scale-invariant, so amplitude is discarded entirely, as is everything beyond lag 1. High positive = state carries over; low or negative = rapid switching."))
for (dd in d4) {
  p <- p + stext(XL, yL, dd[[1]], size = 5.0, fontface = "bold", colour = P$con)
  yL <- yL - 2.4
  p <- p + scode(XL, yL, wrp(dd[[2]], 4.4, CW, family = "Consolas"), size = 4.4, colour = P$ink)
  yL <- yL - 2.2 * length(strsplit(wrp(dd[[2]], 4.4, CW, family = "Consolas"), "\n", fixed = TRUE)[[1]]) - 0.4
  s <- wrp(dd[[3]], 4.4, CW)
  p <- p + stext(XL, yL, s, size = 4.4, colour = P$ink_soft)
  yL <- yL - length(strsplit(s, "\n", fixed = TRUE)[[1]]) * 1.75 - 2.0
}
p <- p + callout(XL, yL - 15.0, CW, 15.0,
  wrp("RMSSD and ACF1 are computed on the NA-COMPACTED series x[is.finite(x)]. Missing bins are removed before diff() and before the lag-1 pairing, so a pair that is adjacent in the compacted index may be separated by a real time gap. There is also no gap-filling upstream: absent bins are missing ROWS, not NA rows. arrange() guarantees ORDER but not SPACING, and no regularity check exists. ACF1's \"n >= 4\" guard is effectively n >= 5, because safe_cor additionally returns NA when the shifted vectors have fewer than 4 finite pairs. A perfectly constant series returns NA (sd == 0) \u2014 so an animal with Proximity identically zero is dropped rather than scored.", 4.3, CW - 4.4),
  kind = "warn", size = 4.3, title = "WHAT THE GUARDS ACTUALLY DO")
yL <- yL - 17.0

# ==============================================================================
# RIGHT COLUMN
# ==============================================================================
yR <- H - 19.0

p <- sec(p, XR, yR, CW, "LEVEL 5   Contextual standardization", "14:5039-5044, 14:349-354", accent = P$sus)
yR <- yR - 7.4
p <- p +
  scode(XR, yR, "standardize_within_context(dat, value_col,\n    group_cols = c(\"Sex\", \"PhaseClass\", \"CageChangeIndex\"))\n\nsafe_scale <- function(x) {\n  s <- sd(x, na.rm = TRUE); m <- mean(x, na.rm = TRUE)\n  if (!is.finite(s) || s == 0) return(rep(0, length(x)))\n  (x - m) / s\n}", size = 4.5, colour = P$con)
yR <- yR - 19.0
o <- prow(p, XR, yR, CW, "MEANING", "z = +1.0 means one SD above animals of the SAME sex, in the SAME phase, at the SAME cage change \u2014 NOT above the whole experiment.", kw = 20, colour = P$ink); p <- o$p; yR <- o$y
o <- prow(p, XR, yR, CW, "WHY", "Counts, bits, fractions, RMSSD and correlations share no unit; averaging them raw would weight each by its numerical scale. Standardizing also prevents large sex, phase and cage-change baselines from dominating a domain score.", kw = 20); p <- o$p; yR <- o$y
o <- prow(p, XR, yR, CW, "GROUP KEPT", "Group (CON/RES/SUS) is deliberately NOT a grouping key, so the between-group contrast survives standardization.", kw = 20); p <- o$p; yR <- o$y
o <- prow(p, XR, yR, CW, "CONTEXT n", "16 cells; n = 58 female / 53 male epochs per cell in the main SIS table.", kw = 20); p <- o$p; yR <- o$y
o <- prow(p, XR, yR, CW, "CAVEAT", "These are RELATIVE descriptions within a context, not absolute biological units, and they are not comparable across facets.", kw = 20, colour = P$sus); p <- o$p; yR <- o$y
yR <- yR - 1.0
p <- p + callout(XR, yR - 14.0, CW, 14.0,
  wrp("safe_scale returns rep(0, length(x)) \u2014 the NUMBER zero, not NA \u2014 whenever the SD is zero or non-finite. That fires for an all-NA cell, a constant cell, a single-observation cell, or any cell containing Inf. A feature that could not be measured at all therefore becomes indistinguishable from a perfectly average animal: score_mean's na.rm cannot drop it and coalesce(., 0) cannot detect it. No warning and no audit column records this.", 4.3, CW - 4.4),
  kind = "warn", size = 4.3, title = "ZERO-FILL, NOT MISSINGNESS")
yR <- yR - 16.2

# ---- LEVEL 6 -----------------------------------------------------------------
p <- sec(p, XR, yR, CW, "LEVEL 6   Domain construction", "14:5259-5290, 14:5318, 14:5329")
yR <- yR - 7.0
p <- p + stext(XR, yR, wrp("score_mean(dat, cols) = rowMeans(as.matrix(dat[, cols]), na.rm = TRUE), with NaN mapped to NA. The denominator is row-dependent and never recorded. Subtracted terms use coalesce(z, 0), so the two halves of one formula treat missingness in OPPOSITE directions.", 4.4, CW), size = 4.4, colour = P$ink_soft)
yR <- yR - 7.2

dom <- list(
  list("1  Psychomotor activation", "Movement_mean_z",
       "Locomotor magnitude, kept as its own dimension so movement cannot silently dominate every composite. HIGH = more movement than the context average; LOW = less."),
  list("2  Behavioral flexibility / predictability", "mean(Entropy_mean_z, Entropy_rmssd_z) \u2212 Entropy_acf1_z",
       "spatial diversity + spatial change \u2212 spatial persistence. HIGH = diverse, changing, weakly persistent spatial use; LOW = restricted and/or persistent. High flexibility is NOT automatically good: it can be adaptive exploration or disorganised scanning."),
  list("3  Social spatial organization", "mean(Proximity_mean_z, Proximity_acf1_z) \u2212 Proximity_rmssd_z",
       "amount of co-location + persistence of co-location \u2212 rapid fluctuation. Mean proximity alone cannot distinguish sustained co-location from repeated approach and separation. HIGH = strong and stable co-organisation. Not a measure of affiliative preference."),
  list("4  Behavioral volatility / fragmentation", "mean(Movement_rmssd_z, Entropy_rmssd_z, Proximity_rmssd_z,\n     inactivity_fragmentation_z, active_inactive_transition_rate_z)",
       "locomotor + spatial + social-spatial volatility, plus quiescence fragmentation and state switching. HIGH = frequent, abrupt change on short timescales. Volatility is not intrinsically maladaptive: acute novelty exploration also raises it."),
  list("5  Inactive-phase rest/circadian regulation   [Inactive only]", "mean(inactivity_fraction_z, mean_inactivity_bout_min_z, Movement_acf1_z)\n  \u2212 mean(Movement_rmssd_z, inactivity_fragmentation_z,\n         active_inactive_transition_rate_z)",
       "stable rest-like behaviour (more low-activity time, longer bouts, greater persistence) minus fragmentation (movement volatility, bout interruption, state switching). HIGH = more sustained rest-like inactivity. NOT EEG sleep. See the critical warning below \u2014 as currently generated this row carries no quiescence information at all."),
  list("6  Active-phase adaptation/exploration   [Active only]", "mean(Movement_mean_z, Entropy_mean_z, Proximity_mean_z)\n  \u2212 mean(Movement_acf1_z, Entropy_acf1_z)",
       "engagement and exploration minus behavioural inertia. HIGH = active, spatially diverse, socially co-located, with comparatively low temporal inertia. This is an OPERATIONAL composite, not a physiological measure of adaptive coping."),
  list("7  Behavioral state architecture   [HMM]", "mean(state_occupancy_entropy_z, social_state_fraction_z)\n  \u2212 inactive_state_fraction_z",
       "How evenly time is spread across inferred HMM states, plus occupancy of the data-derived social state, minus the low-movement/inactive state. HIGH = diverse state use weighted toward the social state."))
for (dd in dom) {
  p <- p + stext(XR, yR, dd[[1]], size = 4.9, fontface = "bold")
  yR <- yR - 2.5
  eqs <- dd[[2]]
  nl <- length(strsplit(eqs, "\n", fixed = TRUE)[[1]])
  p <- p +
    sbox(XR, yR - nl * 1.95 - 1.0, CW, nl * 1.95 + 1.0, fill = P$panel, colour = NA, linewidth = 0) +
    scode(XR + 1.2, yR - 0.5, eqs, size = 4.3, colour = P$con)
  yR <- yR - nl * 1.95 - 2.0
  s <- wrp(dd[[3]], 4.3, CW)
  p <- p + stext(XR, yR, s, size = 4.3, colour = P$ink_soft)
  yR <- yR - length(strsplit(s, "\n", fixed = TRUE)[[1]]) * 1.72 - 2.6
}

# upstream definitions for the quiescence and HMM inputs
p <- p +
  sbox(XR, yR - 25.0, CW, 25.0, fill = P$panel_alt, colour = P$note_edge, linewidth = 0.3) +
  stext(XR + 1.6, yR - 1.4, "Where the upstream inputs come from", size = 4.8, fontface = "bold", colour = P$con)
yy <- yR - 4.6
up <- list(
  c("Stage 12  (10-min bins)", "Per-animal low-activity threshold = 20th percentile of that animal's Movement. A bin is inactive_like when Movement <= threshold. Consecutive inactive-like bins form bouts. inactivity_fraction = share of bins classified inactive-like; mean_inactivity_bout_min = average bout duration; inactivity_fragmentation and active_inactive_transition_rate index how often the binary state changes."),
  c("Stage 08  (10-min bins)", "One pooled 4-state Gaussian HMM (depmixS4, n_states hard-coded to 4) over globally z-scored Movement, Entropy and Proximity. Hard Viterbi labels only. State semantics are assigned post hoc in Stage 14 from the state profiles."))
for (u in up) {
  p <- p + stext(XR + 1.6, yy, u[1], size = 4.4, fontface = "bold", colour = P$ink)
  s <- wrp(u[2], 4.2, CW - 3.2)
  p <- p + stext(XR + 1.6, yy - 2.2, s, size = 4.2, colour = P$ink_soft)
  yy <- yy - 2.2 - length(strsplit(s, "\n", fixed = TRUE)[[1]]) * 1.68 - 1.6
}
yR <- yR - 27.0

p <- p + callout(XR, yR - 9.0, CW, 9.0,
  wrp("HMM states are model-derived behavioural regimes, not independently validated ethological states. The \"social\" and \"inactive\" labels are assigned by a first-match-wins rule over four state-profile rows, so a run can end with no state labelled social, with no warning.", 4.3, CW - 4.4),
  kind = "warn", size = 4.3, title = "HMM STATES ARE NOT VALIDATED ETHOLOGY")
yR <- yR - 11.2

save_schema(p, file.path(ROOT, "rendered", "BehavioralDynamics_schema_supplementary"), W, H)
cat("left column ended at y =", yL, "  right column ended at y =", yR, "\n")

# ==============================================================================
# LEFT COLUMN (continued): LEVEL 7, worked example, sources
# ==============================================================================
p <- sec(p, XL, yL, CW, "LEVEL 7   Domain score → heatmap tile", "14:5822-5843, 14:5951-5971", accent = P$sus)
yL <- yL - 7.4
p <- p + scode(XL, yL,
  "group_by(Domain, PhaseClass, Sex)                  # CageChange NOT a key\n  ref <- pair[1]; comp <- pair[2]\n  x <- scores[Group == ref]; y <- scores[Group == comp]\n  contrast        = paste0(comp, \"-\", ref)\n  hedges_g        = hedges_g(x, y)\n  mean_difference = mean(y) - mean(x)\n  p.value         = t.test(y, x)$p.value             # Welch, var.equal = FALSE\ngroup_by(PhaseClass, Sex) %>% mutate(p_fdr = p.adjust(p.value, \"BH\"))",
  size = 4.4, colour = P$con)
yL <- yL - 19.0
o <- prow(p, XL, yL, CW, "CONTRASTS", "contrast_pairs = list(c(\"CON\",\"RES\"), c(\"CON\",\"SUS\"), c(\"RES\",\"SUS\")) (14:2132) gives the three fixed columns RES-CON, SUS-CON, SUS-RES. g > 0 means the first-named group scores higher.", kw = 21); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "COLOUR", "scale_fill_gradient2(low = \"#3d3b6e\", mid = \"white\", high = \"#e63947\", midpoint = 0, na.value = \"grey90\"). Red = positive g, purple = negative, white = negligible.", kw = 21); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "SYMBOL", "sig_label(p_fdr) drawn on the tile: the BH-adjusted p from the Welch test.", kw = 21); p <- o$p; yL <- o$y
o <- prow(p, XL, yL, CW, "FACETS", "facet_grid(Sex ~ PhaseClass): Female Active | Female Inactive over Male Active | Male Inactive. Phase-specific domains are blank in the facet where they are undefined.", kw = 21); p <- o$p; yL <- o$y
yL <- yL - 0.6

p <- p + callout(XL, yL - 11.0, CW, 11.0,
  wrp("EFFECT is the colour; EVIDENCE is the symbol. Red is not “worse” and purple is not “better” — direction is domain-specific. Red = more movement (psychomotor activation), greater volatility, STRONGER and more stable proximity organisation, or MORE sustained rest-like inactivity.", 4.3, CW - 4.4),
  kind = "warn", size = 4.3, title = "HOW TO READ A TILE")
yL <- yL - 13.2

# ---- worked example ----------------------------------------------------------
p <- sec(p, XL, yL, CW, "WORKED EXAMPLE   full numerical provenance of one tile", "real exported values", accent = P$con)
yL <- yL - 7.0
we <- list(
  c("RFID position sequence", "Animal 1545 · SUS · Female · CC1 · Active phase"),
  c("↓  dyadic reconstruction", "same-position seconds / dyadic observation seconds, per 5-min bin"),
  c("ProximityFraction per bin", "575 bins in this epoch"),
  c("↓  three temporal descriptors", "Proximity_mean  = 0.17099879710257854\nProximity_rmssd = 0.16760332601561975\nProximity_acf1  = 0.60223043530389275"),
  c("↓  z within Sex × Phase × CC", "context = Female × Active × CC1, n = 58 epochs\ncontext mean / SD:  0.229849 / 0.032455   (mean)\n                    0.175161 / 0.014830   (rmssd)\n                    0.652883 / 0.067977   (acf1)"),
  c("standardized values", "Proximity_mean_z  = −1.8132797062905825\nProximity_rmssd_z = −0.5096111877131153\nProximity_acf1_z  = −0.7451450355507243"),
  c("↓  domain equation", "mean(−1.81328, −0.74515) − (−0.50961)  =  −0.76960118320753823\nexported DomainScore                    =  −0.76960118320753823"),
  c("↓  one score per mouse × phase × regrouping", "Female Active: CON 48 rows/12 animals, RES 96/24, SUS 88/22"),
  c("↓  SUS versus RES", "Welch p = 0.4892415507700073   BH q = 0.5870898609240087"),
  c("↓  Hedges’ g", "g = −0.10127263580838378   mean difference = −0.12155670673587333"),
  c("coloured SUS−RES tile", "near-white, no symbol — no standardized difference in this cell")
)
for (i in seq_along(we)) {
  p <- p + stext(XL, yL, we[[i]][1], size = 4.4, fontface = "bold",
                 colour = if (i %% 2 == 1) P$con else P$ink_faint)
  s <- we[[i]][2]
  nl <- length(strsplit(s, "\n", fixed = TRUE)[[1]])
  p <- p + scode(XL + 46, yL, s, size = 4.2, colour = P$ink)
  yL <- yL - max(nl * 1.9, 2.6) - 0.2
}
p <- p + gtile(XL + CW - 12, yL - 0.4, 12, 7.6, -0.101, "", limit = 0.8, size = 4.8)
p <- p + stext(XL, yL - 2.4, wrp("The signal in this domain is elsewhere: in the Inactive facet the same row gives Female RES−CON g = −0.8197 (q = 9.9e-4) and SUS−CON g = −0.7633 (q = 1.9e-3). There is no SUS-vs-RES separation anywhere in this domain.", 4.3, CW - 14), size = 4.3, colour = P$ink_soft)
yL <- yL - 12.0

# ---- sources -----------------------------------------------------------------
p <- sec(p, XL, yL, CW, "SOURCE SCRIPTS AND EXPORTED TABLES", NULL, accent = P$ink_soft)
yL <- yL - 6.6
srcs <- list(
  c("01_build_multiscale_behavior_metrics.R", "Movement, Entropy, Proximity per bin, all six resolutions"),
  c("08_hmm_behavioral_states_optional.R", "pooled 4-state HMM; state occupancy at 10-min"),
  c("12_sleep_like_quiescence_metrics.R", "rest-like inactivity, bouts, fragmentation at 10-min"),
  c("14_systems_neuroscience_summary_dashboard.R", "epoch features, z-scoring, domains, contrasts, plot"),
  c("tables/systems_sis_raw_phase_epoch_features.csv", "888 epoch rows: mean / rmssd / acf1 + quiescence joins"),
  c("tables/systems_sis_domain_scores.csv", "5695 rows: Animal × CC × Phase × Domain scores"),
  c("stats_tables/systems_sis_domain_effect_summary.csv", "84 rows: the g, p and q behind every tile"),
  c("stats_tables/systems_sis_domain_mixed_model_stats.csv", "the repeated-measures model — not the source of the stars")
)
for (s in srcs) {
  p <- p + scode(XL, yL, s[1], size = 4.1, colour = P$con) +
    stext(XL + 62, yL, wrp(s[2], 4.1, CW - 62), size = 4.1, colour = P$ink_soft)
  yL <- yL - 2.4
}

source(file.path(ROOT, "R", "21_supp_warnings.R"))

save_schema(p, file.path(ROOT, "rendered", "BehavioralDynamics_schema_supplementary"), W, H)
cat("FINAL  left y =", yL, "   right y =", yR, "
")
