source(file.path("Functions", "stage09_loao_figure.R"))
pred <- data.frame(
  Model = rep("movement_mean", 6),
  observed = c(-1, -0.5, 0, 0.2, 0.6, 1),
  predicted = c(-0.3, -0.2, 0, 0.1, 0.3, 0.4),
  Group = rep(c("CON", "RES", "SUS"), 2),
  Sex = rep(c("Female", "Male"), each = 3))
perf <- data.frame(model_id = "movement_mean", cv_r2 = 0.2,
                   permutation_p = 1 / 5)
perm <- data.frame(model_id = rep("movement_mean", 5),
                   performance_value = c(0.2, -0.04, -0.03, -0.02, 0.01),
                   is_observed = c(TRUE, rep(FALSE, 4)), n_permutations = 4)
p <- stage09_loao_figure(pred, perf, perm)
stopifnot(inherits(p, "ggplot"),
          identical(unname(p$scales$get_scales("shape")$palette(2)), c(16, 17)),
          identical(unname(p$scales$get_scales("colour")$palette(3)),
                    c("#3d3b6e", "#C6C3BB", "#d45b58")),
          any(vapply(p$layers, function(layer)
            inherits(layer$geom, "GeomCustomAnn"), logical(1))))
bad <- perm[-2, ]
stopifnot(inherits(try(stage09_loao_figure(pred, perf, bad), silent = TRUE),
                   "try-error"))
bad_pred <- pred
bad_pred$Sex[1] <- NA_character_
stopifnot(inherits(try(stage09_loao_figure(bad_pred, perf, perm), silent = TRUE),
                   "try-error"))
cat("Stage 09 LOAO figure contract passed.\n")
