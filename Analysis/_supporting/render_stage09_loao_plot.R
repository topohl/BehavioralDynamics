# Re-render only the Stage 09 headline LOAO figure from persisted tables.
source(file.path("Functions", "stage09_loao_figure.R"))
root <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT",
                   unset = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID")
resolution <- Sys.getenv("MMM_STAGE09_BIN_LEVEL", unset = "10min_based")
bin_label <- sub("_based$", "", resolution)
bin_minutes <- sub("min$", "", bin_label)
if (!resolution %in% c("10min_based", "5min_based"))
  stop("Unsupported Stage 09 resolution: ", resolution, call. = FALSE)
stage <- file.path(root, "analysis_ready", "pipeline", "09_early_prediction",
                   bin_label, "tables")
read_required <- function(name) {
  path <- file.path(stage, name)
  if (!file.exists(path)) stop("Missing persisted Stage 09 table: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE)
}
p <- stage09_loao_figure(
  read_required("primary_prediction_predictions.csv"),
  read_required("primary_prediction_performance.csv"),
  read_required("early_prediction_permutation_draws.csv"))
if (resolution != "10min_based")
  p <- p + ggplot2::labs(caption = paste0("Resolution sensitivity: ", bin_minutes,
                                          "-min bins; primary analysis uses 10-min bins."))
out <- file.path(dirname(stage), "figures", "best_model_observed_vs_predicted")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
for (ext in c("svg", "pdf", "png")) {
  ggplot2::ggsave(paste0(out, ".", ext), p, width = 89, height = 78,
                  units = "mm", dpi = 600)
}
cat("Rendered Stage 09 LOAO figure from persisted tables: ", out, ".{svg,pdf,png}\n", sep = "")
