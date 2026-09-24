# Render the fixed Stage 09 headline model from persisted animal predictions and
# full-refit outcome-permutation draws. Group is descriptive metadata only.
stage09_loao_figure <- function(predictions, performance, permutations,
                                model_id = "movement_mean", outcome = "CombZ") {
  needed_pred <- c("Model", "observed", "predicted", "Group", "Sex")
  needed_perf <- c("model_id", "cv_r2", "permutation_p")
  needed_perm <- c("model_id", "performance_value", "is_observed", "n_permutations")
  for (entry in list(list("predictions", predictions, needed_pred),
                     list("performance", performance, needed_perf),
                     list("permutations", permutations, needed_perm))) {
    missing <- setdiff(entry[[3]], names(entry[[2]]))
    if (length(missing)) stop(entry[[1]], " lacks: ",
                              paste(missing, collapse = ", "), call. = FALSE)
  }
  pred <- predictions[predictions$Model == model_id, , drop = FALSE]
  perf <- performance[performance$model_id == model_id, , drop = FALSE]
  perm <- permutations[permutations$model_id == model_id, , drop = FALSE]
  if (nrow(pred) < 2L || nrow(perf) != 1L || anyNA(pred[, needed_pred]) ||
      !all(is.finite(pred$observed) & is.finite(pred$predicted)) ||
      !setequal(unique(as.character(pred$Group)), c("CON", "RES", "SUS")) ||
      !all(as.character(pred$Sex) %in% c("Female", "Male"))) {
    stop("Invalid headline prediction rows or descriptive Group/Sex metadata.", call. = FALSE)
  }
  observed <- perm$performance_value[perm$is_observed]
  null <- perm$performance_value[!perm$is_observed]
  if (length(observed) != 1L || length(unique(perm$n_permutations)) != 1L ||
      length(null) != unique(perm$n_permutations) ||
      !all(is.finite(c(observed, null))) ||
      abs(observed - perf$cv_r2) > 1e-8 ||
      abs((sum(null >= observed) + 1) / (length(null) + 1) - perf$permutation_p) > 1e-8) {
    stop("Persisted permutation draws disagree with headline performance.", call. = FALSE)
  }
  pred$Group <- factor(as.character(pred$Group), levels = c("CON", "RES", "SUS"))
  pred$Sex <- factor(as.character(pred$Sex), levels = c("Female", "Male"))
  bounds <- range(c(pred$observed, pred$predicted))
  span <- diff(bounds)
  bounds <- bounds + c(-0.03, 0.03) * span
  label <- sprintf("LOAO R2 = %.3f\nperm p = %d/%d",
                   perf$cv_r2, sum(null >= observed) + 1L, length(null) + 1L)
  inset <- ggplot2::ggplot(data.frame(value = null), ggplot2::aes(.data$value)) +
    ggplot2::geom_histogram(bins = 24, fill = "grey75", colour = "white", linewidth = 0.15) +
    ggplot2::scale_x_continuous(breaks = c(-0.05, 0, 0.05),
                                labels = c("-0.05", "0", "0.05")) +
    ggplot2::labs(title = "null R2", x = NULL, y = NULL) +
    ggplot2::theme_classic(base_size = 7) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 7))
  ggplot2::ggplot(pred, ggplot2::aes(.data$observed, .data$predicted,
                                    colour = .data$Group, shape = .data$Sex)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         linewidth = 0.3, colour = "grey60") +
    ggplot2::geom_point(size = 1.8, alpha = 0.78) +
    ggplot2::scale_colour_manual(values = c(CON = "#3d3b6e", RES = "#C6C3BB",
                                            SUS = "#d45b58"), drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(Female = 16, Male = 17), drop = FALSE) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, nrow = 1),
                    shape = ggplot2::guide_legend(order = 2, nrow = 1)) +
    ggplot2::coord_equal(xlim = bounds, ylim = bounds) +
    ggplot2::annotate("text", x = bounds[1], y = bounds[2], label = label,
                      hjust = 0, vjust = 1, size = 2.5) +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(inset),
                               xmin = bounds[1] + 0.57 * diff(bounds),
                               xmax = bounds[1] + 0.99 * diff(bounds),
                               ymin = bounds[1] + 0.65 * diff(bounds),
                               ymax = bounds[1] + 0.99 * diff(bounds)) +
    ggplot2::labs(x = paste("Observed", outcome),
                  y = paste("Held-out predicted", outcome),
                  colour = "Group", shape = "Sex") +
    ggplot2::theme_classic(base_size = 9) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
}
