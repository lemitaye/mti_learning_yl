# Compare pooled employment IV specifications on a common sample.
# Run from the project root.
library(tidyverse)
source("code/helpers/wild_iv_few_regions.R")
source("code/helpers/cluster_inference.R")
source("code/helpers/export_paths.R")

lean <- c(
  "chsex", "stunting", "caredu_r1", "hhsize", "wi_new",
  "round7", "cohort_young"
)
baseline_reduced <- c(
  lean, "hq_new", "cd_new", "elecq_new", "factor(foodsec_r3)"
)
full <- c(
  baseline_reduced, "zbfa", "careage_r1", "factor(caresex_r1)",
  "ownlandhse_r1"
)
specifications <- list(
  "Full" = full,
  "Baseline-reduced" = baseline_reduced,
  "Lean" = lean
)
outcomes <- c("salary_employ", "wage_employ")
required <- all.vars(reformulate(c(full, outcomes, "IMTI", "E_is")))

# Use the same participant-round rows for both outcomes and all models.
sample <- readRDS("data/r7/r5_r7_appended.rds") |>
  mutate(across(where(is.numeric), ~ as.numeric(haven::zap_labels(.x)))) |>
  filter(
    region != "Addis Ababa",
    yc == 1 | (!is.na(raw_lang_r4) & raw_lang_r4 != 0)
  ) |>
  drop_na(all_of(required))

fit_model <- function(outcome, terms) {
  controls <- paste(terms, collapse = " + ")
  model <- lfe::felm(as.formula(paste(
    outcome, "~", controls, "| 0 | (IMTI ~ E_is)"
  )), data = sample)
  stopifnot(model$N == nrow(sample))
  estimate <- tail(summary(model)$coefficients, 1)[1, ]
  first_stage <- lm(
    reformulate(c("E_is", terms), "IMTI"), data = sample
  )
  wild <- wild_iv_few_regions(sample, outcome, controls, unname(estimate[1]))
  cluster <- table2_cluster_checks(sample, outcome, controls, unname(estimate[1]))

  bind_cols(
    tibble(
      beta = unname(estimate[1]),
      se = unname(estimate[2]),
      p = unname(estimate[4]),
      first_stage_F = summary(first_stage)$coefficients["E_is", 3]^2,
      n = nrow(sample)
    ),
    wild$stats,
    select(cluster$stats, AR_p_sign_beta0)
  )
}

stats <- map_dfr(outcomes, function(outcome) {
  imap_dfr(specifications, function(terms, specification) {
    fit_model(outcome, terms) |>
      mutate(outcome = outcome, specification = specification)
  })
})

audit_dir <- "temp/pooled_control_comparison"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(stats, file.path(audit_dir, "diagnostics.csv"))
source("code/auxiliary/write_pooled_control_comparison.R", local = TRUE)
print(stats |> select(outcome, specification, n, beta, se,
                      wild_boot_se, wild_boot_p, AR_p_sign_beta0))
