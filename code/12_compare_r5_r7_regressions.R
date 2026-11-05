# Compare legacy R5 employment IVs with pooled R5/R7 cross sections.
# Run from the project root. Exports a new table to the revision project.
# YC-inclusive fits use frozen exposure and are explicitly exploratory.

library(tidyverse)
source("code/helpers/cluster_inference.R")
source("code/helpers/export_paths.R")

audit_dir <- "temp/r5_r7_regression_comparison"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
input_file <- "data/r7/r5_r7_appended.rds"
input_hash <- tools::md5sum(input_file)

source("code/helpers/fixed_employment_sample.R")

data <- fixed_employment_sample() |> filter(region != "Addis Ababa")
base_controls <- paste(lean_terms, collapse = " + ")
plans <- tribble(
  ~panel, ~description, ~additional,
  "A", "R5 older cohort: replication", "",
  "B", "R5 + R7 both cohorts: exploratory frozen YC exposure",
  " + round7 + cohort_young"
)

# Fix the complete-case sample before both stages and inference checks.
results <- list()
models <- list()
cluster_audits <- list()
sample_keys <- list()
for (panel_id in plans$panel) {
  plan <- plans |> filter(panel == panel_id)
  controls <- paste0(base_controls, plan$additional)
  for (column in 1:4) {
    outcome <- c("salary_employ", "wage_employ")[(column - 1) %% 2 + 1]
    sample <- data
    if (panel_id == "A") {
      sample <- sample |> filter(round == 5, yc == 0)
    }
    if (column > 2) sample <- sample |> filter(region != "Amhara")
    model <- lfe::felm(as.formula(paste(
      outcome, "~", controls, "| 0 | (IMTI ~ E_is)"
    )), data = sample)
    first_stage <- lm(as.formula(paste(
      "IMTI ~ E_is +", controls
    )), data = sample)
    restricted <- lm(as.formula(paste("IMTI ~", controls)), data = sample)
    coefficient <- tail(summary(model)$coefficients, 1)[1, ]
    fs <- summary(first_stage)$coefficients["E_is", ]
    stopifnot(model$N == nobs(first_stage))
    cluster <- table2_cluster_checks(
      sample, outcome, controls, unname(coefficient[1])
    )
    key <- paste(panel_id, column, sep = "_")
    models[[key]] <- model
    sample_keys[[key]] <- sample |> select(child_id, round, yc)
    cluster_audits[[key]] <- cluster$audit |>
      mutate(panel = panel_id, column = column)
    results[[key]] <- bind_cols(
      tibble(
        panel = panel_id, column = column, outcome = outcome,
        excluding_Amhara = column > 2,
        iv_beta = coefficient[1], iv_se = coefficient[2],
        iv_p = coefficient[4], first_stage_beta = fs[1],
        first_stage_F = unname(fs[3]^2),
        partial_R2 = 1 - sum(resid(first_stage)^2) /
          sum(resid(restricted)^2),
        n = nrow(sample), participants = n_distinct(sample$child_id),
        n_r5 = sum(sample$round == 5), n_r7 = sum(sample$round == 7),
        n_young = sum(sample$yc == 1)
      ), cluster$stats
    )
  }
}
stats <- bind_rows(results)

# R5 is re-estimated under current exclusions and combined wage codes.
stopifnot(identical(input_hash, tools::md5sum(input_file)))
write_csv(stats, file.path(audit_dir, "diagnostics.csv"))
write_csv(bind_rows(cluster_audits), file.path(audit_dir, "cluster_audit.csv"))
saveRDS(models, file.path(audit_dir, "models.rds"))
saveRDS(sample_keys, file.path(audit_dir, "estimation_keys.rds"))

# A separate writer keeps table formatting out of the estimation code.
source("code/auxiliary/write_r5_r7_comparison.R", local = TRUE)
print(stats |> select(panel, column, iv_beta, n, iv_p_CR2))
