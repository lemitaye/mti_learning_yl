# Prepare compatible employment cross sections for R5 and R7.
# Run after code/10_build_r7_cross_section.R from the project root.
# R7 uses current household resources and frozen historical MTI assignment.
# YC exposure remains flagged for review; no model is fitted here.

library(tidyverse)
library(haven)

# 1. Inputs and shared employment schema -----------------------------------
output_dir <- "data/r7"
audit_dir <- "temp/r7_append_preparation"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

legacy_files <- c(
  "data/aa_samp.csv", "data/non_aa_samp.csv", "data/ylsample_yc.csv"
)
legacy_hashes <- tools::md5sum(legacy_files)

r5_older <- bind_rows(
  read.csv(legacy_files[1]), read.csv(legacy_files[2])
) |> mutate(yc = 0L)
r5_younger <- read.csv(legacy_files[3]) |> mutate(yc = 1L)
r5 <- bind_rows(r5_older, r5_younger)
r7_source <- readRDS("data/r7/r7_inperson_analysis.rds")

# Keep established names for controls with genuinely historical timing.
fixed_variables <- c(
  "chsex", "chethnic", "chlang", "chldrel", "preprim", "agegr1",
  "lang_primary", "region", "zbfa", "stunting", "caredu_r1",
  "careage_r1", "caresex_r1", "ownlandhse_r1", "foodsec_r3",
  "entype_r4", "typesite_r3", "rural_3", "IMTI", "E_is"
)
current_variables <- c(
  "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new", "sv_new"
)
outcomes <- c(
  "active", "type_activ", "wage_employ", "wage_employII",
  "self_employ", "self_employII"
)
shared_variables <- c(fixed_variables, current_variables, outcomes)

# 2. R5: retain existing values, including structural YC outcome missingness -
r5_analysis <- r5 |>
  select(child_id, yc, all_of(shared_variables))

# R5 did not supply the YC employment outcomes in the existing analysis file.
# They remain NA rather than being interpreted as non-employment.
r7_analysis <- r7_source |>
  select(child_id, yc, all_of(paste0(fixed_variables, "_hist"))) |>
  rename_with(~ str_remove(.x, "_hist$"), ends_with("_hist")) |>
  mutate(
    hhsize = as.numeric(r7_source$hhsize_r7),
    wi_new = as.numeric(r7_source$wi_r7_legacy_definition),
    hq_new = as.numeric(r7_source$hq_r7_legacy_definition),
    cd_new = as.numeric(r7_source$durables_r7),
    elecq_new = as.numeric(r7_source$electricity_r7),
    sv_new = as.numeric(r7_source$services_r7),
    active = as.numeric(r7_source$worked_annual_r7),
    type_activ = as.numeric(r7_source$activity_annual_r7),
    wage_employ = as.numeric(r7_source$wage_annual_r7),
    wage_employII = as.numeric(r7_source$salary_annual_r7),
    self_employ = as.numeric(r7_source$self_employment_annual_r7),
    self_employII = as.numeric(r7_source$nonfarm_self_employment_annual_r7)
  )

# 3. Common indicators and explicit timing/provenance -----------------------
prepare_round <- function(data, round_number) {
  data |>
    select(child_id, yc, all_of(shared_variables)) |>
    mutate(
      round = round_number,
      round7 = as.integer(round == 7L),
      cohort_young = as.integer(yc == 1L),
      cohort = if_else(yc == 1L, "young", "old"),
      mti_exposure_review_required = yc == 1L,
      household_measurement_round = if_else(round == 7L, 7L, 3L),
      .after = yc
    )
}
r5_analysis <- prepare_round(r5_analysis, 5L)
r7_analysis <- prepare_round(r7_analysis, 7L)

# Retain childhood test scores under names that state their measurement round.
# They must not be read as new R7 learning outcomes.
r5_analysis <- r5_analysis |>
  mutate(raw_lang_r4 = r5$raw_lang, raw_maths_r4 = r5$raw_maths)
r7_analysis <- r7_analysis |>
  mutate(
    raw_lang_r4 = r7_source$raw_lang_hist,
    raw_maths_r4 = r7_source$raw_maths_hist
  )

# 4. Labels shared across both rounds --------------------------------------
apply_labels <- function(data) {
  for (variable in names(data)) {
    source_name <- paste0(variable, "_hist")
    description <- attr(r7_source[[source_name]], "label")
    if (is.null(description)) description <- gsub("_", " ", variable)
    attr(data[[variable]], "label") <- description
  }
  binary_variables <- c(
    "yc", "round7", "cohort_young", "active", "wage_employ",
    "wage_employII", "self_employ", "self_employII", "elecq_new"
  )
  for (variable in binary_variables) {
    codes <- switch(variable,
      yc = c("Old" = 0, "Young" = 1),
      cohort_young = c("Old" = 0, "Young" = 1),
      round7 = c("Round 5" = 0, "Round 7" = 1),
      c("No" = 0, "Yes" = 1)
    )
    data[[variable]] <- labelled(
      data[[variable]], labels = codes,
      label = attr(data[[variable]], "label")
    )
  }
  data$round <- labelled(
    data$round, c("Round 5" = 5L, "Round 7" = 7L), label = "Survey round"
  )
  activity_labels <- read.csv(
    "temp/r7_cross_section_build/source_value_labels.csv"
  ) |>
    filter(cohort == "oc", variable == "typwrk12r7", code < 21) |>
    distinct(code, label)
  data$type_activ <- labelled(
    data$type_activ,
    setNames(activity_labels$code, activity_labels$label),
    label = "Annual main activity; original categories and R7 aliases"
  )
  descriptions <- c(
    wage_employ = "Annual main activity: wage employment (5, 12, 13)",
    wage_employII = "Annual main activity: salary employment (13)",
    self_employ = "Annual main activity: farm/nonfarm self-employment",
    self_employII = "Annual main activity: nonfarm self-employment",
    wi_new = "Original-definition wealth; R3 in R5 rows, R7 in R7 rows",
    hq_new = "Original-definition housing; R3 in R5 rows, R7 in R7 rows",
    cd_new = "Original ten durables; R3 in R5 rows, R7 in R7 rows",
    elecq_new = "Electricity; R3 in R5 rows, R7 in R7 rows",
    hhsize = "Resident household size; R3 in R5 rows, R7 in R7 rows",
    sv_new = "Basic services; R3 in R5 rows, R7 in R7 rows",
    IMTI = "Frozen original schooling-capped MTI through R5; YC review",
    raw_lang_r4 = "Historical R4 raw language score",
    raw_maths_r4 = "Historical R4 raw mathematics score"
  )
  for (variable in names(descriptions)) {
    attr(data[[variable]], "label") <- descriptions[[variable]]
  }
  as_tibble(data)
}
r5_analysis <- apply_labels(r5_analysis)
r7_analysis <- apply_labels(r7_analysis)
combined <- bind_rows(r5_analysis, r7_analysis)

# 5. Validate and export: retain all rows; model deletion remains explicit ---
source("code/auxiliary/audit_r5_r7_append.R", local = TRUE)

for (name in c("r5_analysis", "r7_analysis", "combined")) {
  filename <- switch(name,
    r5_analysis = "r5_append_ready",
    r7_analysis = "r7_append_ready",
    combined = "r5_r7_appended"
  )
  saveRDS(get(name), file.path(output_dir, paste0(filename, ".rds")))
  write_csv(get(name), file.path(output_dir, paste0(filename, ".csv")))
}
