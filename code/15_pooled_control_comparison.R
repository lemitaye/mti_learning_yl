# Compare pooled employment IV specifications on a common sample.
# Run from the project root.
library(tidyverse)
source("code/helpers/wild_iv_few_regions.R")
source("code/helpers/cluster_inference.R")
source("code/helpers/export_paths.R")

source("code/helpers/fixed_employment_sample.R")
baseline_reduced <- c(baseline_reduced_terms, "round7", "cohort_young")
lean <- setdiff(baseline_reduced, c(
  "hq_new", "cd_new", "elecq_new", "factor(foodsec_r3)"
))
extra_numeric <- c("zbfa", "careage_r1", "ownlandhse_r1")
full <- c(
  baseline_reduced, extra_numeric, "factor(caresex_r1)",
  paste0(extra_numeric, "_missing")
)
specifications <- list(
  "Full (missingness adjusted)" = full,
  "Baseline-reduced" = baseline_reduced,
  "Lean" = lean
)
outcomes <- c("salary_employ", "wage_employ")

# Keep the fixed sample when adding controls with missing values.
# Zero fills are paired with indicators, and caregiver sex has a missing level.
sample <- fixed_employment_sample() |>
  filter(region != "Addis Ababa") |>
  mutate(
    across(all_of(extra_numeric), is.na, .names = "{.col}_missing"),
    across(all_of(extra_numeric), ~ replace_na(.x, 0)),
    caresex_r1 = factor(if_else(is.na(caresex_r1), "Missing",
                               as.character(caresex_r1)))
  )

# Skip constant missingness indicators in the fitted sample.
constant_indicators <- paste0(extra_numeric, "_missing")
constant_indicators <- constant_indicators[
  !vapply(sample[constant_indicators], any, logical(1))
]
specifications[[1]] <- setdiff(specifications[[1]], constant_indicators)

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
