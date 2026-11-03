# Pooled R5/R7 both-cohort IV comparisons and few-region bootstrap checks.
# No cohort is dropped. YC frozen exposure remains exploratory.
# Run from the project root; auxiliary writer exports the new Overleaf table.
library(tidyverse)
source("code/helpers/cluster_inference.R")
source("code/helpers/wild_iv_few_regions.R")
source("code/helpers/export_paths.R")
audit_dir <- "temp/pooled_robustness"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
reference_file <- file.path(audit_dir, "diagnostics.csv")
reference <- if (file.exists(reference_file)) {
  read.csv(reference_file) |> filter(column == 2)
} else NULL
input_file <- "data/r7/r5_r7_appended.rds"
input_hash <- tools::md5sum(input_file)
data <- readRDS(input_file) |>
  mutate(across(where(is.numeric), ~ as.numeric(haven::zap_labels(.x)))) |>
  mutate(across(where(is.character), as.factor)) |>
  filter(region != "Addis Ababa")
base_controls <- paste(
  "chsex + zbfa + stunting + caredu_r1 + careage_r1",
  "+ factor(caresex_r1) + hhsize + wi_new + hq_new + cd_new",
  "+ elecq_new + ownlandhse_r1 + factor(foodsec_r3) + cohort_young"
)
plans <- tribble(
  ~specification, ~round_dummy, ~exclude_amhara, ~relax_score_gate,
  "No round dummy", FALSE, FALSE, FALSE,
  "Round dummy", TRUE, FALSE, FALSE,
  "Exclude Amhara", TRUE, TRUE, FALSE,
  "Relax score gate", TRUE, FALSE, TRUE
)
results <- list()
models <- list()
bootstrap_draws <- list()
keys <- list()
cluster_audit <- list()
for (outcome in c("wage_employII", "wage_employ")) {
  for (column in seq_len(nrow(plans))) {
    plan <- plans[column, ]
    sample <- data
    if (!plan$relax_score_gate) {
      sample <- sample |>
        filter(yc == 1 | (!is.na(raw_lang_r4) & raw_lang_r4 != 0))
    }
    if (plan$exclude_amhara) {
      sample <- sample |> filter(region != "Amhara")
    }
    controls <- paste0(base_controls,
      if (plan$round_dummy) " + round7" else "")
    variables <- all.vars(as.formula(paste(
      outcome, "~ IMTI + E_is +", controls
    )))
    sample <- sample[complete.cases(sample[, variables]), ]
    model <- lfe::felm(as.formula(paste(
      outcome, "~", controls, "| 0 | (IMTI ~ E_is)"
    )), data = sample)
    coefficient <- tail(summary(model)$coefficients, 1)[1, ]
    first_stage <- lm(as.formula(paste(
      "IMTI ~ E_is +", controls
    )), data = sample)
    cluster <- table2_cluster_checks(
      sample, outcome, controls, unname(coefficient[1])
    )
    wild <- wild_iv_few_regions(
      sample, outcome, controls, unname(coefficient[1])
    )
    key <- paste(outcome, column, sep = "_")
    models[[key]] <- model
    keys[[key]] <- sample |> select(child_id, round, yc, region)
    bootstrap_draws[[key]] <- wild
    cluster_audit[[key]] <- cluster$audit |>
      mutate(outcome = outcome, column = column)
    results[[key]] <- bind_cols(
      tibble(
        outcome = outcome, column = column,
        specification = plan$specification,
        cohort_dummy = TRUE, round_dummy = plan$round_dummy,
        exclude_amhara = plan$exclude_amhara,
        relax_score_gate = plan$relax_score_gate,
        iv_beta = coefficient[1], iv_se = coefficient[2],
        iv_p = coefficient[4], n = nrow(sample),
        n_r5 = sum(sample$round == 5), n_r7 = sum(sample$round == 7),
        n_young = sum(sample$yc == 1),
        participants = n_distinct(sample$child_id),
        first_stage_F = summary(first_stage)$coefficients["E_is", 3]^2
      ), cluster$stats, wild$stats
    )
  }
}
stats <- bind_rows(results)
# Only the round dummy changes in columns 1 and 2; samples must match.
for (outcome in unique(stats$outcome)) {
  stopifnot(identical(keys[[paste0(outcome, "_1")]],
                      keys[[paste0(outcome, "_2")]]))
}
# Column 2 retains the previous cohort-plus-round model exactly.
if (!is.null(reference)) {
  check <- stats |> filter(column == 2)
  stopifnot(isTRUE(all.equal(check$iv_beta, reference$iv_beta,
    tolerance = 1e-9, check.attributes = FALSE)),
    identical(as.integer(check$n), as.integer(reference$n)))
}
stopifnot(identical(input_hash, tools::md5sum(input_file)))
write_csv(stats, file.path(audit_dir, "diagnostics.csv"))
write_csv(bind_rows(cluster_audit), file.path(audit_dir, "cluster_audit.csv"))
saveRDS(models, file.path(audit_dir, "models.rds"))
saveRDS(keys, file.path(audit_dir, "estimation_keys.rds"))
saveRDS(bootstrap_draws, file.path(audit_dir, "bootstrap_draws.rds"))
source("code/auxiliary/write_pooled_robustness.R", local = TRUE)
print(stats |> select(outcome, specification, n, iv_beta,
                     wild_boot_se, wild_boot_p, AR_p_sign_beta0))
