# Prepare compatible employment cross sections for R5 and R7.
# Run after code/10_build_r7_cross_section.R from the project root.
# R7 uses current household resources and frozen historical MTI assignment.
# YC exposure remains flagged for review; no model is fitted here.

library(tidyverse)
library(haven)
source("code/helpers/analysis_helpers.R")
source("code/helpers/raw_data_catalog.R")

# 1. Inputs and shared employment schema -----------------------------------
output_dir <- "data/r7"
audit_dir <- "temp/r7_append_preparation"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

legacy_files <- c(
  "data/aa_samp.rds", "data/non_aa_samp.rds", "data/ylsample_yc.csv"
)
legacy_hashes <- tools::md5sum(legacy_files)

r5_older <- bind_rows(
  readRDS(legacy_files[1]), readRDS(legacy_files[2])
) |>
  prepare_employment_outcomes() |>
  mutate(across(where(is.factor), as.character), yc = 0L)
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
  "active", "self_agri", "wage_agri", "self_nonag",
  "wage_employ", "salary_employ", "self_employ",
  paste0("type_activ_", 1:4)
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
    type_activ_1 = as.numeric(r7_source$activity_annual_r7),
    type_activ_2 = NA_real_, type_activ_3 = NA_real_,
    type_activ_4 = NA_real_,
    self_agri = as.numeric(r7_source$self_agri_r7),
    wage_agri = as.numeric(r7_source$wage_agri_r7),
    self_nonag = as.numeric(r7_source$self_nonag_r7),
    wage_employ = as.numeric(r7_source$wage_employ_r7),
    salary_employ = as.numeric(r7_source$salary_employ_r7),
    self_employ = as.numeric(r7_source$self_employ_r7)
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
      outcome_coverage = case_when(
        round == 5L & yc == 1L ~ "not_available",
        round == 5L ~ "any_of_four_reported_activities",
        TRUE ~ "annual_main_activity_only"
      ),
      secondary_activity_types_observed = round == 5L & yc == 0L,
      outcome_definition_version = "revised_r5_codes_partial_coverage_v2",
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

# Participation and secondary-job coverage diagnostics, not model controls.
participation <- read_dta(
  raw_data_files[["constructed_legacy"]],
  col_select = c(childid, round, inround)
) |>
  filter(round == 5) |>
  transmute(
    child_id = as.numeric(str_remove(childid, "ET")),
    r5_interview_observed = as.integer(inround)
  )
stopifnot(!anyDuplicated(participation$child_id))
r5_analysis <- r5_analysis |>
  left_join(participation, by = "child_id", relationship = "one-to-one") |>
  mutate(other_annual_work_count_r7 = NA_real_)
other_jobs <- map_dfr(c("oc", "yc"), function(cohort) {
  read_dta(
    file.path(raw_data_files[["round7_ethiopia"]],
      paste0("et_r7_", cohort, "ch_arch.dta")),
    col_select = c(childcode, amntothwrk12r7)
  ) |>
    transmute(
      child_id = as.numeric(childcode),
      other_annual_work_count_r7 = if_else(
        amntothwrk12r7 %in% 0:50, as.numeric(amntothwrk12r7), NA_real_
      )
    )
})
r7_analysis <- r7_analysis |>
  left_join(other_jobs, by = "child_id", relationship = "one-to-one") |>
  mutate(
    r5_interview_observed = NA_integer_,
    other_annual_work_count_r7 = if_else(
      active == 0, 0, other_annual_work_count_r7
    )
  )
# Ensure identical order after adding diagnostics from different sources.
r7_analysis <- r7_analysis |> select(all_of(names(r5_analysis)))

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
    "salary_employ", "self_employ", "self_agri", "wage_agri",
    "self_nonag", "elecq_new"
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
  for (variable in paste0("type_activ_", 1:4)) {
    data[[variable]] <- labelled(
      data[[variable]],
      setNames(activity_labels$code, activity_labels$label),
      label = paste(variable, "R5 slot; R7 slot 1 is main, others unobserved")
    )
  }
  descriptions <- c(
    wage_employ = "Wage (12/19): any activity R5; main only R7",
    salary_employ = "Salary (13): any activity R5; main only R7",
    self_agri = "Agricultural self-work (1-4): any R5; main R7",
    wage_agri = "Agricultural wage (5-7): any R5; main R7",
    self_nonag = "Nonfarm self-work (8-11): any R5; main R7",
    self_employ = "Combined self-work: any R5; main R7",
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
