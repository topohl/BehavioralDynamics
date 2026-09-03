# ==============================================================================
# 10_main_compact_schema.R
# BehavioralDynamics - MAIN PAPER compact provenance schema.
# 125 x 262 mm  (1.5-column width, full-page height).
#
# Canvas units are millimetres (1 unit = 1 mm), so type sizes in pt and box
# geometry can be reasoned about directly at final print size. Prose is wrapped
# with wrp() against a measured column width, so nothing overflows silently.
#
# Formulas and line references were traced to source in
#   C:/Users/topohl/Documents/GitHub/MMMSociability/Analysis
# The worked example uses REAL exported values; R/00_verify_provenance.R
# reproduces the exported DomainScore exactly (max |exported - recomputed| = 0).
# ==============================================================================

ROOT <- "C:/Users/topohl/Documents/GitHub/MMMSociability/manuscript/BehavioralDynamics_schema"
source(file.path(ROOT, "R", "schema_toolkit.R"))

W <- 125; H <- 262
P <- SCHEMA_PAL
L <- 3.5; R <- W - 3.5
CW <- R - L

p <- schema_canvas(c(0, W), c(0, H))

body <- function(pp, x, y, txt, size = 4.7, colour = P$ink_soft, w = CW, ...) {
  s <- wrp(txt, size, w); check_fit(s, size, w, where = paste0("y=", y))
  pp + stext(x, y, s, size = size, colour = colour, ...)
}

# ------------------------------------------------------------------ title ----
p <- p +
  stext(L, H - 1.6, "Continuous home-cage behavioural phenotype", size = 8.6, fontface = "bold") +
  stext(L, H - 6.6, "Multiscale behavioural dimensions, split by active/inactive phase and sex, expressed as pairwise group contrasts",
        size = 5.4, colour = P$ink_soft) +
  stext(L, H - 11.8, "Provenance of Fig_sis_active_inactive_domain_heatmap   |   Stage 01 \u2192 12 / 08 \u2192 14   |   5-min backbone",
        size = 4.7, colour = P$ink_faint) +
  sline(L, H - 14.6, R, H - 14.6, colour = P$ink, linewidth = 0.5)

# =========================== 1 - RAW RFID OBSERVATION ========================
b1 <- 237.6
p <- p + level_banner(L, b1, CW, 6.4, "1", "Raw RFID observation",
                      subtitle = "what the apparatus actually records")
p <- body(p, L, b1 - 1.6,
  "A timestamped sequence of discrete position identities \u2014 not continuous x/y tracking. One read = (time, animal, PositionID).", 4.9)

sq <- c("P3", "P3", "P4", "P6", "P6", "P2")
bx <- 12.0; bw <- 11.6; gp <- 3.6; sy <- 226.0
for (i in seq_along(sq)) {
  xi <- bx + (i - 1) * (bw + gp); mv <- i > 1 && sq[i] != sq[i - 1]
  p <- p +
    sbox(xi, sy, bw, 5.2, fill = if (mv) "#e8eff5" else "white",
         colour = if (mv) P$movement else P$rule, linewidth = if (mv) 0.45 else 0.26) +
    stext(xi + bw / 2, sy + 2.6, sq[i], size = 5.8, hjust = 0.5, vjust = 0.5,
          fontface = if (mv) "bold" else "plain", colour = if (mv) P$movement else P$ink)
  if (i < length(sq))
    p <- p + sarrow(xi + bw + 0.4, sy + 2.6, xi + bw + gp - 0.4, sy + 2.6,
                    colour = P$ink_faint, linewidth = 0.22, len = 0.8)
}
p <- p +
  stext(L, sy + 2.6, "time \u2192", size = 4.9, hjust = 0, vjust = 0.5, colour = P$ink_faint) +
  stext(bx, sy - 1.2, "blue = PositionID differs from the previous read   \u2192   3 movement events in this window",
        size = 4.6, colour = P$movement)

by <- 201.0; bh <- 21.0; bwid <- (CW - 2 * 2.4) / 3
brs <- list(
  list(t = "A  Movement", col = P$movement,
       eq = "Movement = # PositionID\n  transitions in the bin",
       mean = "how much the animal changes location", src = "01:30, 01:470"),
  list(t = "B  Spatial entropy", col = P$entropy,
       eq = "H = \u2212 \u03a3 p\u1d62 log\u2082 p\u1d62\n  p\u1d62 = sec at position i\n       / total sec",
       mean = "diversity / evenness of spatial occupancy \u2014 not distance", src = "01:182"),
  list(t = "C  Proximity", col = P$proximity,
       eq = "ProximityFraction =\n  ProximitySeconds /\n  dyadic_observation_sec",
       mean = "share of observable social time spent co-located", src = "01:492, 01:823")
)
for (i in seq_along(brs)) {
  b <- brs[[i]]; xi <- L + (i - 1) * (bwid + 2.4)
  p <- p +
    sbox(xi, by, bwid, bh, fill = "white", colour = b$col, linewidth = 0.42) +
    sbox(xi, by + bh - 4.4, bwid, 4.4, fill = b$col, colour = b$col, linewidth = 0.42) +
    stext(xi + 1.5, by + bh - 2.2, b$t, size = 5.6, hjust = 0, vjust = 0.5,
          colour = "white", fontface = "bold") +
    scode(xi + 1.5, by + bh - 5.6, b$eq, size = 4.5, colour = b$col) +
    stext(xi + 1.5, by + 7.0, wrp(b$mean, 4.7, bwid - 3.2), size = 4.7, colour = P$ink) +
    stext(xi + bwid - 1.5, by + 0.7, b$src, size = 4.2, hjust = 1, vjust = 0, colour = P$ink_faint) +
    sarrow(xi + bwid / 2, sy - 3.4, xi + bwid / 2, by + bh + 0.7,
           colour = b$col, linewidth = 0.3, len = 1.1)
}

# =========================== 2 - MULTISCALE BINNING ==========================
b2 <- 194.0
p <- p + level_banner(L, b2, CW, 6.4, "2", "Multiscale temporal binning",
                      subtitle = "irregular events \u2192 ordered time series")
bins <- c("10 s", "1 min", "5 min", "10 min", "30 min", "phase")
bwv <- 14.2; gpv <- 2.3; x0 <- L + 18.5
for (i in seq_along(bins)) {
  xi <- x0 + (i - 1) * (bwv + gpv); pr <- bins[i] == "5 min"; s12 <- bins[i] == "10 min"
  p <- p +
    sbox(xi, b2 - 7.0, bwv, 4.8, fill = if (pr) P$con else if (s12) P$panel_alt else "white",
         colour = if (pr) P$con else if (s12) P$note_edge else P$rule,
         linewidth = if (pr || s12) 0.42 else 0.24) +
    stext(xi + bwv / 2, b2 - 4.6, bins[i], size = 5.0, hjust = 0.5, vjust = 0.5,
          colour = if (pr) "white" else P$ink, fontface = if (pr || s12) "bold" else "plain")
}
p <- p + stext(L, b2 - 4.6, "Stage 01\nemits all:", size = 4.7, hjust = 0, vjust = 0.5, colour = P$ink_faint)
p <- body(p, L, b2 - 8.8,
  "Backbone for this figure = 5-min bins (14:72). \u201cMultiscale\u201d means the pipeline generates and can integrate several resolutions \u2014 not that each tile averages over all of them. Stage 12 (quiescence) and Stage 08 (HMM) import at 10-min, so one composite can mix resolutions.", 4.6)

# =========================== 3 - BIOLOGICAL CONTEXT ==========================
b3 <- 174.0
p <- p + level_banner(L, b3, CW, 6.4, "3", "Repeated perturbations \u00d7 active/inactive phase",
                      subtitle = "the epoch that becomes one observation")
lv <- list(c("Animal", "111 animals \u2014 CON 24 / RES 49 / SUS 38"),
           c("CC1 \u2044 CC2 \u2044 CC3 \u2044 CC4", "four regroupings (a repeated measure)"),
           c("Active \u2044 Inactive", "dark, high-activity  |  light, rest-like"),
           c("ordered 5-min bins", "Active \u2264 576 bins; Inactive \u2264 432"))
for (i in seq_along(lv)) {
  yy <- b3 - 1.8 - (i - 1) * 3.7
  p <- p +
    stext(L + (i - 1) * 2.8, yy, paste0(if (i > 1) "\u2514 " else "", lv[[i]][1]),
          size = 5.0, fontface = "bold", colour = P$con) +
    stext(L + 38, yy, lv[[i]][2], size = 4.7, colour = P$ink_soft)
}
p <- p +
  sbox(R - 34, b3 - 14.4, 34, 12.6, fill = P$panel, colour = P$rule, linewidth = 0.3) +
  stext(R - 32.6, b3 - 3.4, "one epoch =", size = 4.6, colour = P$ink_faint) +
  stext(R - 32.6, b3 - 6.4, "Animal 1545 \u00d7 CC1 \u00d7 Active", size = 5.0, fontface = "bold") +
  stext(R - 32.6, b3 - 9.6, "SUS, Female, 575 bins", size = 4.6, colour = P$ink_soft) +
  stext(R - 32.6, b3 - 12.4, "pools every phase of that CC", size = 4.3, colour = P$ink_faint)
p <- body(p, L, b3 - 16.6,
  "Phases are kept separate because the same locomotor value means different things in the dark and the light phase.  14:5226", 4.5, colour = P$ink_faint)

# =========================== 4 - TEMPORAL DESCRIPTORS ========================
b4 <- 148.0
p <- p + level_banner(L, b4, CW, 6.4, "4", "Three temporal descriptions of each signal",
                      subtitle = "the same mean can hide very different organisation")
c4 <- list(
  list(t = "Mean", lab = "MAGNITUDE", eq = "mean(x\u209c)", q = "\u201chow much?\u201d"),
  list(t = "RMSSD", lab = "LOCAL VOLATILITY", eq = "\u221amean((x\u209c \u2212 x\u209c\u208b\u2081)\u00b2)",
       q = "\u201chow much does it change\nbetween adjacent bins?\u201d"),
  list(t = "ACF1", lab = "PERSISTENCE / INERTIA", eq = "cor(x\u209c , x\u209c\u208b\u2081)",
       q = "\u201chow strongly does now\ndepend on just before?\u201d")
)
cw4 <- (CW - 2 * 2.4) / 3
for (i in seq_along(c4)) {
  cc <- c4[[i]]; xi <- L + (i - 1) * (cw4 + 2.4)
  p <- p +
    sbox(xi, b4 - 15.4, cw4, 13.6, fill = "white", colour = P$rule, linewidth = 0.3) +
    stext(xi + 1.5, b4 - 3.2, cc$t, size = 6.0, fontface = "bold", colour = P$con) +
    stext(xi + 1.5, b4 - 6.6, cc$lab, size = 4.4, fontface = "bold", colour = P$ink_faint) +
    scode(xi + 1.5, b4 - 9.2, cc$eq, size = 4.6, colour = P$ink) +
    stext(xi + 1.5, b4 - 12.0, cc$q, size = 4.6, colour = P$ink_soft)
}
p <- p +
  scode(L + 1.5, b4 - 16.8, "3 3 4 3 4  \u2192  mean 3.4   RMSSD 0.87   (smooth)", size = 4.5, colour = P$ink) +
  scode(L + 1.5, b4 - 19.4, "0 8 1 9 0  \u2192  mean 3.6   RMSSD 7.68   (fragmented)", size = 4.5, colour = P$sus)
p <- body(p, L, b4 - 21.8,
  "Same level, opposite organisation. Computed per epoch for Movement, Entropy and Proximity (14:5229\u20135237); RMSSD needs \u2265 3 finite bins, ACF1 \u2265 4.", 4.5, colour = P$ink_faint)

# =========================== 5 - CONTEXTUAL Z ================================
b5 <- 118.0
p <- p + level_banner(L, b5, CW, 6.4, "5", "Contextual standardization before integration",
                      subtitle = "what the composite scores are relative to", accent = P$sus)
p <- p +
  sbox(L, b5 - 17.6, CW, 15.8, fill = P$warn_bg, colour = P$sus, linewidth = 0.4) +
  scode(L + 2.2, b5 - 3.2, "z = (animal value \u2212 context mean) / context SD", size = 5.2, colour = P$ink) +
  stext(L + 2.2, b5 - 6.4, "context  =  Sex  \u00d7  Active/Inactive phase  \u00d7  CageChange",
        size = 5.0, fontface = "bold", colour = P$sus)
p <- body(p, L + 2.2, b5 - 9.4,
  "Movement_mean_z = +1.0 means one SD above animals of the SAME sex, in the SAME phase, at the SAME regrouping \u2014 not above the whole experiment.",
  4.6, colour = P$ink, w = CW - 4.4)
p <- body(p, L + 2.2, b5 - 13.4,
  "Counts, bits, fractions, RMSSD and correlations share no unit; averaging them raw would weight by numerical scale. Standardization also stops large sex / phase / cage-change baselines from dominating.   14:5039",
  4.4, colour = P$ink_soft, w = CW - 4.4)
p <- p + callout(L, b5 - 25.0, CW, 5.6,
  wrp("z-scores describe RELATIVE organisation within a context. They are not absolute biological units and cannot be read across facets as if they were.", 4.6, CW - 4.4),
  kind = "note", size = 4.6)

# =========================== 6 - DOMAIN CONSTRUCTION =========================
b6 <- 87.0
p <- p + level_banner(L, b6, CW, 6.4, "6", "Domain construction \u2014 the seven heatmap rows",
                      subtitle = "inputs \u2192 equation \u2192 meaning", accent = P$con)
dm <- list(
  c("Psychomotor activation", "Movement_mean_z", "more locomotor output"),
  c("Behavioral flexibility / predictability", "mean(Ent_mean_z, Ent_rmssd_z) \u2212 Ent_acf1_z", "diverse + changing, weakly persistent"),
  c("Social spatial organization", "mean(Prox_mean_z, Prox_acf1_z) \u2212 Prox_rmssd_z", "co-location strong AND stable"),
  c("Behavioral volatility / fragmentation", "mean(Mov_rmssd_z, Ent_rmssd_z, Prox_rmssd_z, inact_frag_z, transition_z)", "abrupt change on short timescales"),
  c("Inactive-phase rest/circadian regulation", "mean(inact_frac_z, bout_z, Mov_acf1_z) \u2212 mean(Mov_rmssd_z, inact_frag_z, transition_z)", "\u25b3 as generated, carries no quiescence input \u2014 Suppl. W1\u2013W2"),
  c("Active-phase adaptation/exploration", "mean(Mov_mean_z, Ent_mean_z, Prox_mean_z) \u2212 mean(Mov_acf1_z, Ent_acf1_z)", "engaged + exploratory, low inertia   [Active]"),
  c("Behavioral state architecture", "mean(state_entropy_z, social_state_z) \u2212 inactive_state_z", "diverse state use, social > inactive   [HMM]")
)
for (i in seq_along(dm)) {
  d <- dm[[i]]; yy <- b6 - 1.8 - (i - 1) * 4.75
  p <- p +
    sbox(L, yy - 4.35, CW, 4.35, fill = if (i %% 2 == 0) P$panel else "white", colour = NA, linewidth = 0) +
    stext(L + 1.2, yy - 0.5, d[1], size = 4.8, fontface = "bold") +
    scode(L + 1.2, yy - 2.5, d[2], size = 4.1, colour = P$con) +
    stext(R - 1.2, yy - 2.0, d[3], size = 4.3, hjust = 1, vjust = 0.5,
          colour = if (startsWith(d[3], "△")) P$sus else P$ink_faint,
          fontface = if (startsWith(d[3], "△")) "bold" else "plain")
}
p <- body(p, L, b6 - 35.6,
  "score_mean() = rowMeans(na.rm=TRUE): a score may use a SUBSET of its inputs; a subtracted term uses coalesce(z,0), so a missing penalty counts as zero.",
  4.4, colour = P$sus)

# =========================== 7 - CONTRAST -> TILE ============================
b7 <- 43.0
p <- p + level_banner(L, b7, CW, 6.4, "7", "From animal domain score to one heatmap tile",
                      subtitle = "effect = colour, evidence = symbol", accent = P$sus)
st <- c("one score per\nanimal \u00d7 phase\n\u00d7 regrouping", "pool the group\u2019s\nscores (CC1\u2013CC4\nnot averaged)",
        "Welch two-sample\nt-test on the two\ndistributions", "Hedges\u2019 g\n(pooled SD,\nsmall-n corrected)",
        "BH FDR within\nSex \u00d7 PhaseClass\n(across domains)")
sw <- 19.0
for (i in seq_along(st)) {
  xi <- L + (i - 1) * (sw + 2.4)
  p <- p +
    sbox(xi, b7 - 11.4, sw, 9.2, fill = "white", colour = P$rule, linewidth = 0.3) +
    stext(xi + sw / 2, b7 - 6.8, st[i], size = 4.3, hjust = 0.5, vjust = 0.5)
  if (i < length(st))
    p <- p + sarrow(xi + sw + 0.3, b7 - 6.8, xi + sw + 2.1, b7 - 6.8,
                    colour = P$ink_faint, linewidth = 0.24, len = 0.9)
}
tx <- L + 5 * (sw + 2.4)
p <- p +
  gtile(tx, b7 - 10.4, 10.4, 7.2, -0.10, "", limit = 0.8, size = 4.8) +
  stext(tx + 5.2, b7 - 11.6, "one tile", size = 4.3, hjust = 0.5, vjust = 1, colour = P$ink_faint)
p <- body(p, L, b7 - 13.0,
  "Columns fixed: RES\u2212CON | SUS\u2212CON | SUS\u2212RES;  g > 0 = first-named group higher. Facets: Sex (rows) \u00d7 Active/Inactive (columns).   14:5822",
  4.5, colour = P$ink_soft)
p <- p + callout(L, b7 - 24.0, CW, 8.4,
  wrp("Red is not \u201cworse\u201d and purple is not \u201cbetter\u201d \u2014 direction is domain-specific. Red = more movement (psychomotor), more volatility, STRONGER and more stable proximity organisation, or MORE sustained rest-like inactivity.", 4.5, CW - 4.4),
  kind = "warn", size = 4.5)

# ===================== WORKED EXAMPLE (real exported values) =================
p <- p +
  sline(L, 17.2, R, 17.2, colour = P$ink, linewidth = 0.4) +
  stext(L, 16.2, "Worked example \u2014 real values, one genuine tile", size = 5.4, fontface = "bold") +
  stext(R, 16.2, "source: systems_sis_raw_phase_epoch_features.csv \u00b7 systems_sis_domain_effect_summary.csv",
        size = 4.0, hjust = 1, vjust = 1, colour = P$ink_faint)

wx <- c(L, 24.0, 47.5, 71.0, 94.5)
wn <- list(
  c("Animal 1545", "SUS \u00b7 Female", "CC1 \u00b7 Active", "575 \u00d7 5-min bins"),
  c("Proximity per bin", "mean  0.1710", "RMSSD 0.1676", "ACF1  0.6022"),
  c("z in Female\u00d7Active\u00d7CC1", "mean  \u22121.813", "RMSSD \u22120.510", "ACF1  \u22120.745"),
  c("Social spatial org.", "mean(\u22121.813, \u22120.745)", "\u2212 (\u22120.510)", "=  \u22120.770"),
  c("Contrast: SUS\u2212RES", "n 88 vs 96 rows", "g = \u22120.101", "q = 0.587  (n.s.)")
)
ax <- c(19.4, 43.0, 66.5, 90.0)          # arrow gutters between the five nodes
for (i in seq_along(wn)) {
  p <- p +
    stext(wx[i], 12.9, wn[[i]][1], size = 4.4, fontface = "bold", colour = P$con) +
    scode(wx[i], 10.2, paste(wn[[i]][-1], collapse = "\n"), size = 4.2, colour = P$ink)
  if (i < length(wn))
    p <- p + sarrow(ax[i], 8.6, ax[i] + 2.6, 8.6, colour = P$ink_faint,
                    linewidth = 0.24, len = 0.9)
}
p <- p +
  gtile(R - 10.4, 5.7, 10.4, 6.8, -0.101, "", limit = 0.8, size = 4.8) +
  stext(R - 5.2, 4.4, "the tile", size = 4.0, hjust = 0.5, vjust = 0, colour = P$ink_faint) +
  stext(L, 0.7, "Recomputing this chain from the exported table reproduces the exported domain score exactly (max |difference| = 0 across all 882 rows).",
        size = 4.2, vjust = 0, colour = P$ink_faint)

save_schema(p, file.path(ROOT, "rendered", "BehavioralDynamics_schema_main"), W, H)
