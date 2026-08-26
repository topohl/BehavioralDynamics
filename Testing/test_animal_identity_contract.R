# Focused regression checks for canonical behavioral animal identity.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("Analysis/_pipeline_setup.R")

examples <- c("3", "0003", "4", "0004", "303", "00303", "OR004", "OR111", "OQ754")
expected <- c("3", "3", "4", "4", "303", "303", "OR004", "OR111", "OQ754")
stopifnot(identical(canonical_animal_id(examples), expected))

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
metrics_path <- file.path(base_dir, "analysis_ready/03_derived_metrics/10min_based/all_behavior_metrics.csv")
metrics <- read_csv(
  metrics_path,
  show_col_types = FALSE,
  col_types = cols_only(
    AnimalID = col_character(), AnimalNum = col_character(), Sex = col_character(),
    Group = col_character(), CageChange = col_character()
  )
) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum))

identity_qc <- metrics %>%
  distinct(AnimalNum, Sex, Group) %>%
  group_by(AnimalNum) %>%
  summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop")
stopifnot(all(identity_qc$n_groups == 1L), all(identity_qc$n_sexes == 1L))

animal_3 <- metrics %>% filter(AnimalNum == "3") %>% distinct(Sex, Group)
animal_4 <- metrics %>% filter(AnimalNum == "4") %>% distinct(Sex, Group)
stopifnot(
  nrow(animal_3) == 1L, identical(animal_3$Sex, "Male"), identical(animal_3$Group, "RES"),
  nrow(animal_4) == 1L, identical(animal_4$Sex, "Male"), identical(animal_4$Group, "SUS")
)
stopifnot(!any(metrics$AnimalNum == "4" & metrics$Group != "SUS"))

stage03_dir <- behavior_stage_dir(base_dir, "03", "movement_phase_stats", "10min")
endpoints <- read_csv(
  file.path(stage03_dir, "tables/raw_movement_animal_level_endpoints.csv"),
  show_col_types = FALSE
)
endpoint_qc <- endpoints %>%
  distinct(AnimalNum, Sex, Group) %>%
  group_by(AnimalNum) %>%
  summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop")
stopifnot(all(endpoint_qc$n_groups == 1L), all(endpoint_qc$n_sexes == 1L))
stopifnot(!anyDuplicated(endpoints %>% filter(ScopeType == "overall_by_phase") %>% select(AnimalNum, Group, Sex, Endpoint)))

cat("Animal identity contract checks: PASS\n")
