# Build the R7 in-person cross section (older and younger cohorts).
# Run from the project root: Rscript code/10_build_r7_cross_section.R
# Inputs: archived R7 core/roster, constructed R7, and legacy history lookups.
# Outputs: data/r7/ plus aggregate checks in temp/r7_cross_section_build/.
# No models or external exports run; the production R1-R5 sample is preserved.
# Style: https://r4ds.hadley.nz/workflow-style.html
#        https://worldbank.github.io/dime-data-handbook/coding.html
# 1. Setup and paths -------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(haven))
source("code/helpers/r7_build_helpers.R", local = TRUE)
source("code/helpers/raw_data_catalog.R", local = TRUE)
root <- normalizePath(".")
destination <- "data/r7"
audit_dir <- "temp/r7_cross_section_build"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
protected_files <- c(
  raw_data_files, file.path("data", c(
    "aa_samp.csv", "non_aa_samp.csv",
    "ylsample_yc.csv", "schsur_yl_joined.csv"
  )),
  list.files("output/tables", recursive = TRUE, full.names = TRUE),
  list.files("output/figures", recursive = TRUE, full.names = TRUE)
)
protected_files <- protected_files[file.exists(protected_files) & !dir.exists(protected_files)]
protected_hashes <- tools::md5sum(protected_files)
write_audit <- function(cohort_data, filename) write.csv(cohort_data, file.path(audit_dir, filename), row.names = FALSE, na = "")
# Numeric codes remain in separate *_raw variables; analytic fields use only
# declared valid domains. Missing/unknown/refused are never converted to no.
as_numeric_code <- function(x) as.double(x)
valid_codes <- function(x, values) {
  x <- as_numeric_code(x)
  x[!x %in% values] <- NA_real_
  x
}
r7_root <- raw_data_files[["round7_ethiopia"]]
constructed <- haven::read_dta(raw_data_files[["constructed_r1_r7"]])
constructed <- constructed |> filter(round == 7)
stopifnot(!anyDuplicated(constructed$childid))
# 2. Historical lookup: existing values always take precedence --------------
older_history <- bind_rows(
  read.csv("data/aa_samp.csv"),
  read.csv("data/non_aa_samp.csv")
) |> mutate(yc = 0L)
younger_history <- read.csv("data/ylsample_yc.csv") |> mutate(yc = 1L)
existing <- bind_rows(older_history, younger_history)
stopifnot(!anyDuplicated(existing$child_id))
# Recover a historical lookup for R7 respondents excluded by the OLD R5
# deceased filter. Existing lookup values always take precedence. The legacy
# scripts themselves and their production CSVs are not changed.
recovered <- recover_r7_history(root, audit_dir)
extra <- recovered |> anti_join(existing, by = "child_id")
history <- bind_rows(existing, extra)
result <- list()
checks <- list()
sources <- list()
definitions <- list()
labels <- list()
define_variable <- function(variable, origin, period, description) {
  definitions[[variable]] <<- data.frame(
    variable = variable, origin = origin,
    period = period, definition = description
  )
}
# 3. Construct the same R7 variables for each cohort ------------------------
for (cohort in c("oc", "yc")) {
  path <- file.path(r7_root, paste0("et_r7_", cohort, "ch_arch.dta"))
  raw <- haven::read_dta(path)
  phone_path <- file.path(r7_root, paste0("et_r7_", cohort, "ch_phone_arch.dta"))
  phone <- haven::read_dta(phone_path)
  roster_path <- file.path(r7_root, paste0("et_r7_", cohort, "hh_arch.dta"))
  roster <- haven::read_dta(roster_path)
  sources[[cohort]] <- data.frame(
    path = c(path, roster_path, phone_path),
    md5 = unname(tools::md5sum(c(path, roster_path, phone_path)))
  )
  labelled_fields <- c(
    "typwrk12r7", "typwrk7r7", "lstqualr7", "fdsec00r7", "electr7",
    "employer12r7", "employer7r7", "econsec12r7", "econsec7r7", "cntrctwk7r7"
  )
  labels[[cohort]] <- map_dfr(labelled_fields, function(variable) {
    lab <- attr(raw[[variable]], "labels")
    data.frame(cohort = cohort, variable = variable, code = as_numeric_code(lab), label = names(lab))
  })
  ids <- as_numeric_code(raw$childcode)
  stopifnot(
    !anyNA(ids), !anyDuplicated(ids), !any(ids %in% as_numeric_code(phone$childcode)),
    !anyDuplicated(paste(roster$childcode, roster$memidr7))
  )
  # Explicit one-to-one joins preserve the in-person core's ID order.
  participant_ids <- tibble(child_id = ids)
  constructed_cohort <- participant_ids |>
    left_join(
      constructed |> mutate(child_id = as.numeric(str_remove(childid, "ET"))),
      by = "child_id", relationship = "one-to-one"
    )
  stopifnot(
    !anyNA(constructed_cohort$childid), all(constructed_cohort$in_phone_r7 == 0), all(constructed_cohort$inround == 1),
    all(constructed_cohort$yc == as.integer(cohort == "yc"))
  )
  historical_data <- participant_ids |> left_join(history, by = "child_id", relationship = "one-to-one")
  stopifnot(all(historical_data$yc == as.integer(cohort == "yc")))
  cohort_data <- data.frame(
    child_id = ids, childid = paste0("ET", sprintf("%06d", ids)),
    yc = as.integer(cohort == "yc"), cohort = if (cohort == "yc") "younger" else "older",
    round = 7L, interview_mode = "in_person", interview_date_r7 = as.Date(raw$dintr7),
    legacy_r5_sample_member = ids %in% existing$child_id,
    historical_lookup_available = ids %in% history$child_id,
    historical_lookup_source = ifelse(ids %in% existing$child_id, "existing_legacy_analysis_CSV",
      "legacy_extraction_without_deceased_filter_fallback"
    ), stringsAsFactors = FALSE
  )
  for (variable in setdiff(names(historical_data), c("child_id", "yc"))) {
    target <- paste0(variable, "_hist")
    define_variable(
      target, "Original legacy analysis lookup; isolated legacy-source fallback if absent",
      "R1-R5; original variable timing retained", paste("Original", variable, "unchanged; suffix prevents treating it as a current R7 measurement")
    )
  }
  cohort_data <- bind_cols(cohort_data, historical_data |> select(-child_id, -yc) |> rename_with(~ paste0(.x, "_hist")))
  # Explicitly keep the inherited gate as a flag; do NOT apply it to the master.
  cohort_data$original_language_gate <- if (cohort == "oc") !is.na(historical_data$raw_lang) & historical_data$raw_lang != 0 else TRUE
  cohort_data$legacy_comparison_eligible <- cohort_data$legacy_r5_sample_member & cohort_data$original_language_gate
  cohort_data$yc_exposure_review_required <- cohort == "yc"
  cohort_data$yc_r5_schooling_below8_or_missing <- cohort == "yc" & (is.na(historical_data$hghgrade_final_num) | historical_data$hghgrade_final_num < 8)
  cohort_data$school_grade_r7 <- valid_codes(constructed_cohort$hghschoolgrade, 0:12)
  cohort_data$qualification_code_raw_r7 <- as_numeric_code(raw$lstqualr7)
  qualification_codes <- setdiff(as_numeric_code(attr(raw$lstqualr7, "labels")), c(-99, -88, -79, -77, 77, 79, 88, 99))
  stopifnot(length(qualification_codes) > 0)
  cohort_data$qualification_code_r7 <- valid_codes(raw$lstqualr7, qualification_codes)
  # Reuse the original MTI rules verbatim as a clearly provisional sensitivity:
  # only the schooling input is updated; historical language/region remain fixed.
  cohort_data$IMTI_r7_schooling_candidate <- calculate_mti_candidate(historical_data, cohort_data$school_grade_r7)
  define_variable(
    "IMTI_r7_schooling_candidate", "Original IMTI rules + R7 constructed school grade",
    "R7 school attainment, historical language/policy cells", "Provisional sensitivity only; not validated full schooling-language exposure, particularly for YC"
  )
  define_variable("school_grade_r7", "UKDA-9543 hghschoolgrade", "R7", "School-only highest completed grade; not the broader legacy grade scale")
  define_variable("qualification_code_r7", paste(basename(path), "lstqualr7"), "R7", "Label-validated qualification category; special codes are NA; not years of schooling")
  # 3a. Work screens and outcome-specific classifications -------------------
  screens <- c("lst12frmr7", "lst12nhsr7", "lst12bsnr7")
  annual_screens <- raw |>
    select(all_of(screens)) |>
    mutate(across(everything(), ~ valid_codes(.x, 0:1))) |>
    as.matrix()
  cohort_data$worked_annual_r7 <- ifelse(rowSums(annual_screens == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(annual_screens == 0, na.rm = TRUE) == 3, 0, NA_real_)
  )
  wk_names <- c("lstwkfrmr7", "lstwknhsr7", "lstwkbsnr7")
  weekly_screens <- sapply(seq_along(wk_names), function(j) {
    variable <- valid_codes(raw[[wk_names[j]]], 0:1)
    variable[is.na(variable) & !is.na(annual_screens[, j]) & annual_screens[, j] == 0] <- 0
    variable
  })
  cohort_data$worked_weekly_r7 <- ifelse(rowSums(weekly_screens == 1, na.rm = TRUE) > 0, 1,
    ifelse(rowSums(weekly_screens == 0, na.rm = TRUE) == 3, 0, NA_real_)
  )
  current_job <- valid_codes(raw$curjbr7, 0:1)
  for (window in c("annual", "weekly_job")) {
    variable <- if (window == "annual") "typwrk12r7" else "typwrk7r7"
    activity <- valid_codes(raw[[variable]], c(1:13, 15, 19:22))
    cohort_data[[paste0("activity_", window, "_raw_r7")]] <- as_numeric_code(raw[[variable]])
    mapped <- activity
    mapped[mapped %in% 21] <- 1
    mapped[mapped %in% 22] <- 4
    cohort_data[[paste0("activity_", window, "_r7")]] <- mapped
    known_no <- if (window == "annual") {
      !is.na(cohort_data$worked_annual_r7) & cohort_data$worked_annual_r7 == 0
    } else {
      !is.na(cohort_data$worked_weekly_r7) & cohort_data$worked_weekly_r7 == 0 &
        ((!is.na(cohort_data$worked_annual_r7) & cohort_data$worked_annual_r7 == 0) | (!is.na(current_job) & current_job == 0))
    }
    for (outcome in c("salary", "wage", "self_employment", "nonfarm_self_employment")) {
      positive <- switch(outcome,
        salary = 13,
        wage = c(5, 12, 13),
        self_employment = c(1:4, 8:11),
        nonfarm_self_employment = 8:11
      )
      exclude <- switch(outcome,
        salary = c(8:11, 19),
        wage = c(1:4, 8:11, 19),
        self_employment = c(5, 12, 13, 19),
        nonfarm_self_employment = c(13, 19)
      )
      # Valid activity takes precedence; exclusions stay missing.
      y <- case_when(
        mapped %in% positive ~ 1,
        mapped %in% exclude ~ NA_real_,
        !is.na(mapped) ~ 0,
        known_no ~ 0,
        TRUE ~ NA_real_
      )
      target <- paste0(outcome, "_", window, "_r7")
      cohort_data[[target]] <- y
      define_variable(
        target, paste(basename(path), variable, "and work screens"), window,
        paste("Legacy", outcome, "positive/exclusion rules; unknown stays NA, valid nonwork=0; activity 21->1 and 22->4")
      )
    }
    # All-respondent alternatives use a different denominator, explicitly named.
    for (outcome in c("salary", "wage")) {
      positive <- if (outcome == "salary") 13 else c(5, 12, 13)
      y <- case_when(
        !is.na(mapped) ~ as.numeric(mapped %in% positive),
        known_no ~ 0,
        TRUE ~ NA_real_
      )
      cohort_data[[paste0(outcome, "_", window, "_allrespondents_r7")]] <- y
    }
  }
  for (variable in c("employer12r7", "employer7r7", "econsec12r7", "econsec7r7", "cntrctwk7r7")) {
    values <- setdiff(as_numeric_code(attr(raw[[variable]], "labels")), c(-99, -88, -79, -77, 77, 79, 88, 99))
    cohort_data[[paste0(variable, "_raw")]] <- as_numeric_code(raw[[variable]])
    cohort_data[[paste0(variable, "_clean")]] <- valid_codes(raw[[variable]], values)
  }
  cohort_data$public_sector_annual_r7 <- ifelse(is.na(cohort_data$employer12r7_clean), NA, as.integer(cohort_data$employer12r7_clean == 4))
  # 3b. Contemporaneous household controls ----------------------------------
  # Current roster: keep only actual resident members, require complete roster.
  household_sizes <- roster |>
    group_by(childcode) |>
    summarise(hhsize = sum(livhser7 == 1), .groups = "drop")
  hh <- setNames(household_sizes$hhsize, household_sizes$childcode)
  cohort_data$roster_complete_r7 <- valid_codes(raw$cmplthhr7, 0:1)
  cohort_data$hhsize_r7 <- as.numeric(hh[as.character(ids)])
  cohort_data$hhsize_r7[is.na(cohort_data$roster_complete_r7) | cohort_data$roster_complete_r7 != 1] <- NA
  define_variable("hhsize_r7", paste(basename(roster_path), "livhser7=1; core cmplthhr7=1"), "R7", "Current resident count including index child; incomplete rosters remain missing")
  for (variable in c("wi", "hq", "wi_new", "hq_new", "sv", "cd")) {
    target <- switch(variable,
      wi = "wi_r7_legacy_definition",
      hq = "hq_r7_legacy_definition",
      wi_new = "wi_r7_revised_definition",
      hq_new = "hq_r7_revised_definition",
      sv = "services_r7",
      cd = "durables_r7"
    )
    cohort_data[[target]] <- as_numeric_code(constructed_cohort[[variable]])
    cohort_data[[target]][cohort_data[[target]] < 0 | cohort_data[[target]] > 1] <- NA
    define_variable(target, paste("UKDA-9543 R7", variable), "R7", "Imported constructed index, range checked; see raw component checks and definition bridge")
  }
  cohort_data <- cohort_data |>
    mutate(
      electricity_raw_r7 = as_numeric_code(raw$electr7),
      electricity_r7 = valid_codes(raw$electr7, 0:1),
      foodsec_raw_r7 = as_numeric_code(raw$fdsec00r7),
      foodsec_r7 = valid_codes(raw$fdsec00r7, 1:4),
      ownhouse_r7 = valid_codes(raw$ownhser7, 0:1),
      region_current_code_r7 = as_numeric_code(raw$regionr7),
      region_current_r7 = as.character(haven::as_factor(raw$regionr7)),
      age_years_r7 = as_numeric_code(constructed_cohort$ageyea)
    )
  for (variable in c("wallr7", "roofr7", "floorr7", "rmshser7", "srcwtrr7", "toiletr7", "flcookr7", paste0("hhitemsr7_", 1:9), "hhitemsr7_11")) {
    cohort_data[[paste0(variable, "_raw")]] <- as_numeric_code(raw[[variable]])
  }
  # Consumer durables: original ten-item set (fan is not in the original set).
  asset_names <- c(paste0("hhitemsr7_", 1:9), "hhitemsr7_11")
  assets <- raw |>
    select(all_of(asset_names)) |>
    mutate(across(everything(), ~ valid_codes(.x, 0:1)))
  cohort_data$durables_raw_reconstruction_r7 <- rowMeans(assets)
  cohort_data$durables_component_discrepancy_r7 <- !is.na(cohort_data$durables_raw_reconstruction_r7) & !is.na(cohort_data$durables_r7) &
    abs(cohort_data$durables_raw_reconstruction_r7 - cohort_data$durables_r7) > 1e-6
  cohort_data$electricity_component_discrepancy_r7 <- !is.na(cohort_data$electricity_r7) & !is.na(constructed_cohort$elecq) & cohort_data$electricity_r7 != constructed_cohort$elecq
  # TN43 Ethiopia definitions; un-suffixed constructed HQ/WI retain the
  # original wall-material rule. R7 _new versions additionally accept code 25.
  rooms <- valid_codes(raw$rmshser7, 1:50)
  wall <- valid_codes(raw$wallr7, 1:25)
  roof <- valid_codes(raw$roofr7, 1:25)
  floor <- valid_codes(raw$floorr7, 1:21)
  indicator <- function(x, good) ifelse(is.na(x), NA_real_, as.numeric(x %in% good))
  crowding <- pmin(rooms / cohort_data$hhsize_r7 / 1.5, 1)
  cohort_data$hq_raw_reconstruction_r7 <- (crowding + indicator(wall, c(3, 8, 9, 14)) +
    indicator(roof, c(4, 6, 15)) + indicator(floor, c(1, 3, 6, 7))) / 4
  water <- indicator(valid_codes(raw$srcwtrr7, 1:14), c(3, 4, 8, 10))
  sanitation <- indicator(valid_codes(raw$toiletr7, 1:10), c(1, 5, 6))
  cooking <- indicator(valid_codes(raw$flcookr7, 1:16), c(8, 9))
  cohort_data$services_raw_reconstruction_r7 <- (cohort_data$electricity_r7 + water + sanitation + cooking) / 4
  cohort_data$wi_raw_reconstruction_r7 <- (cohort_data$hq_raw_reconstruction_r7 + cohort_data$services_raw_reconstruction_r7 + cohort_data$durables_raw_reconstruction_r7) / 3
  for (variable in c("hq", "services", "wi")) {
    reconstructed <- cohort_data[[paste0(variable, "_raw_reconstruction_r7")]]
    imported <- cohort_data[[switch(variable,
      hq = "hq_r7_legacy_definition",
      services = "services_r7",
      wi = "wi_r7_legacy_definition"
    )]]
    cohort_data[[paste0(variable, "_component_discrepancy_r7")]] <- !is.na(reconstructed) & !is.na(imported) & abs(reconstructed - imported) > 1e-6
    stopifnot(!any(cohort_data[[paste0(variable, "_component_discrepancy_r7")]]))
  }
  # 3c. Reading responses: retain items without inventing a score ------------
  # YC reading responses are available; do not invent an answer key or IRT scale.
  for (variable in c(paste0("raq", 1:12, "r7"), "cnsntreadr7", "readtesttime")) {
    cohort_data[[paste0(variable, "_raw_r7")]] <- if (variable %in% names(raw)) as_numeric_code(raw[[variable]]) else NA_real_
  }
  cohort_data$reading_items_observed_r7 <- if (cohort == "yc") rowSums(!is.na(cohort_data[paste0("raq", 1:12, "r7_raw_r7")])) else NA_integer_
  cohort_data$reading_score_r7 <- NA_real_
  cohort_data$reading_score_requires_validated_key <- cohort == "yc"
  # 3d. Completeness flags: keep every respondent in the master --------------
  historical_controls <- c(
    "IMTI", "E_is", "chsex", "zbfa", "stunting", "caredu_r1", "careage_r1", "caresex_r1",
    "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new", "ownlandhse_r1", "foodsec_r3"
  )
  # Include caregiver sex for both cohorts, flag completeness rather than silently drop it.
  historical_control_columns <- paste0(historical_controls, "_hist")
  cohort_data$historical_controls_complete <- complete.cases(cohort_data[historical_control_columns])
  current_control_columns <- c(
    paste0(setdiff(historical_controls, c("hhsize", "wi_new", "hq_new", "cd_new", "elecq_new")), "_hist"),
    "hhsize_r7", "wi_r7_legacy_definition", "hq_r7_legacy_definition", "durables_r7", "electricity_r7"
  )
  cohort_data$r7_household_controls_complete <- complete.cases(cohort_data[current_control_columns])
  current_food_control_columns <- c(setdiff(current_control_columns, "foodsec_r3_hist"), "foodsec_r7")
  cohort_data$r7_household_plus_food_controls_complete <- complete.cases(cohort_data[current_food_control_columns])
  for (y in c("salary", "wage")) cohort_data[[paste0("complete_case_", y, "_annual_r7")]] <- cohort_data$r7_household_controls_complete & !is.na(cohort_data[[paste0(y, "_annual_r7")]])
  stopifnot(
    all(!is.na(cohort_data$worked_annual_r7)),
    !any(cohort_data$worked_annual_r7 == 1 & is.na(cohort_data$activity_annual_r7)),
    !any(cohort_data$worked_annual_r7 == 0 & !is.na(cohort_data$activity_annual_r7)),
    all(cohort_data$salary_annual_allrespondents_r7 <= cohort_data$wage_annual_allrespondents_r7, na.rm = TRUE)
  )
  checks[[cohort]] <- data.frame(
    cohort = cohort, rows = nrow(cohort_data), phone_ids_retained = sum(ids %in% as_numeric_code(phone$childcode)),
    existing_history = sum(cohort_data$legacy_r5_sample_member), fallback_history = sum(!cohort_data$legacy_r5_sample_member & cohort_data$historical_lookup_available),
    annual_workers = sum(cohort_data$worked_annual_r7 == 1), roster_complete = sum(cohort_data$roster_complete_r7 == 1, na.rm = TRUE),
    durables_discrepancies = sum(cohort_data$durables_component_discrepancy_r7), electricity_discrepancies = sum(cohort_data$electricity_component_discrepancy_r7),
    hq_raw_pairs = sum(!is.na(cohort_data$hq_raw_reconstruction_r7) & !is.na(cohort_data$hq_r7_legacy_definition)),
    services_raw_pairs = sum(!is.na(cohort_data$services_raw_reconstruction_r7) & !is.na(cohort_data$services_r7)),
    wi_raw_pairs = sum(!is.na(cohort_data$wi_raw_reconstruction_r7) & !is.na(cohort_data$wi_r7_legacy_definition)),
    hq_discrepancies = sum(cohort_data$hq_component_discrepancy_r7), services_discrepancies = sum(cohort_data$services_component_discrepancy_r7), wi_discrepancies = sum(cohort_data$wi_component_discrepancy_r7)
  )
  result[[cohort]] <- cohort_data
}
# 4. Combine cohorts, validate keys, and protect historical values ----------
master <- bind_rows(result) |>
  arrange(child_id) |>
  as.data.frame()
stopifnot(
  nrow(master) == 2038L, sum(master$yc == 0) == 632L, sum(master$yc == 1) == 1406L,
  !anyNA(master$child_id), !anyDuplicated(master$child_id), all(master$interview_mode == "in_person")
)
# Match source ID strings and ensure historical values in the existing sample are unchanged.
original_id <- match(master$child_id, existing$child_id)
for (variable in setdiff(names(existing), c("child_id", "yc"))) {
  ix <- !is.na(original_id)
  stopifnot(isTRUE(all.equal(master[[paste0(variable, "_hist")]][ix], existing[[variable]][original_id[ix]], check.attributes = FALSE)))
}
# 5. Export the separate dataset and aggregate write_audit evidence ---------------
write.csv(master, file.path(destination, "r7_inperson_analysis.csv"), row.names = FALSE, na = "")
saveRDS(master, file.path(destination, "r7_inperson_analysis.rds"))
write_audit(bind_rows(checks), "build_checks.csv")
write_audit(bind_rows(sources), "r7_source_manifest.csv")
write_audit(bind_rows(labels), "source_value_labels.csv")
cohort_summary <- master |>
  group_by(cohort) |>
  summarise(
    rows = n(),
    age_min = min(age_years_r7, na.rm = TRUE),
    age_max = max(age_years_r7, na.rm = TRUE),
    r5_schooling_below8_or_missing = sum(yc_r5_schooling_below8_or_missing),
    imti_candidate_changed = sum(abs(IMTI_r7_schooling_candidate - IMTI_hist) > 1e-8, na.rm = TRUE),
    reading_any_items = sum(reading_items_observed_r7 > 0, na.rm = TRUE),
    reading_all12_items = sum(reading_items_observed_r7 == 12, na.rm = TRUE),
    .groups = "drop"
  )
write_audit(cohort_summary, "cohort_summary.csv")

outcome_names <- c(
  "worked_annual_r7", "worked_weekly_r7", "salary_annual_r7", "wage_annual_r7",
  "self_employment_annual_r7", "nonfarm_self_employment_annual_r7",
  "salary_weekly_job_r7", "wage_weekly_job_r7"
)
outcome_counts <- master |>
  select(cohort, all_of(outcome_names)) |>
  pivot_longer(-cohort, names_to = "variable", values_to = "value") |>
  group_by(cohort, variable) |>
  summarise(
    zero = sum(value == 0, na.rm = TRUE),
    one = sum(value == 1, na.rm = TRUE),
    missing = sum(is.na(value)),
    .groups = "drop"
  )
write_audit(outcome_counts, "outcome_counts.csv")

# Count missingness separately by cohort, including structural missing fields.
availability <- map_dfr(c("older", "younger"), function(cohort_name) {
  cohort_data <- master |> filter(cohort == cohort_name)
  map_dfr(names(cohort_data), function(variable_name) {
    observed <- sum(!is.na(cohort_data[[variable_name]]))
    tibble(
      cohort = cohort_name,
      variable = variable_name,
      rows = nrow(cohort_data),
      observed = observed,
      missing = nrow(cohort_data) - observed
    )
  })
})
write_audit(availability, "variable_availability.csv")
define_variable("child_id", "Raw R7 childcode", "All rounds", "Unique numeric participant join key; childid retains the ET-prefixed string")
define_variable("yc", "Raw cohort file + constructed cohort check", "Time invariant", "0=older cohort; 1=younger cohort")
define_variable("IMTI_hist", "Original legacy analysis lookup", "Schooling through R5", "Frozen original schooling-capped MTI measure; YC may complete primary schooling after R5")
define_variable("E_is_hist", "Original legacy policy/ethnicity/language rules", "Historical assignment", "Frozen policy instrument; current residence must not overwrite policy geography")
define_variable("legacy_comparison_eligible", "Existing legacy CSV membership and original OC language gate", "Sample flag", "OC: original history member and raw_lang nonzero/nonmissing; YC: original history member. Not applied to master")
define_variable("yc_exposure_review_required", "Cohort flag", "R7", "TRUE for all YC: validate completed-primary MTI exposure before estimation")
define_variable("yc_r5_schooling_below8_or_missing", "Legacy completed schooling", "R5", "Conservative review flag for YC schooling below eight or missing; not a finding of miscoding")
define_variable("reading_score_r7", "Not scored", "R7", "Intentionally missing: archived raw reading responses require a validated answer key and a comparability decision")
define_variable("reading_items_observed_r7", "Raw raq1r7 to raq12r7", "R7", "Count of nonmissing raw response cells, not correct answers or a validated test-completion flag")
define_variable("foodsec_r7", "Raw fdsec00r7", "R7", "Food situation category, valid codes 1:4; same four-category construct as historical control, different date")
define_variable("electricity_r7", "Raw electr7", "R7", "0=no, 1=yes; missing and special codes remain NA")
define_variable("r7_household_controls_complete", "Outcome-independent completeness", "Mixed timing", "Frozen IMTI/E_is, sex, childhood nutrition, R1 caregiver/land and R3 food; R7 size, WI/HQ/CD/electricity")
define_variable("r7_household_plus_food_controls_complete", "Outcome-independent completeness", "Mixed timing", "Same as household completeness but replaces R3 food situation with R7 food situation")
for (w in c("annual", "weekly_job")) {
  for (y in c("salary", "wage")) {
    define_variable(
      paste0(y, "_", w, "_allrespondents_r7"), "Raw activity and valid nonwork screens", w,
      "Alternative denominator: any valid other activity=0, including categories excluded in legacy outcome; known nonwork=0"
    )
  }
}
dictionary <- map_dfr(names(master), function(variable_name) {
  if (variable_name %in% names(definitions)) {
    definitions[[variable_name]]
  } else {
    tibble(
      variable = variable_name,
      origin = "R7 source or explicitly named build flag",
      period = "R7 unless marked hist",
      definition = variable_name
    )
  }
})
write_audit(dictionary, "variable_dictionary.csv")

# These are conditional candidate counts, not fitted-model samples.
sample_groups <- c("all", "non_Addis", "excluding_Amhara", "Addis")
sample_counts <- map_dfr(c("older", "younger"), function(cohort_name) {
  cohort_data <- master |> filter(cohort == cohort_name)
  map_dfr(sample_groups, function(sample_group) {
    in_group <- with(cohort_data, case_when(
      sample_group == "all" ~ TRUE,
      is.na(region_hist) ~ FALSE,
      sample_group == "Addis" ~ region_hist == "Addis Ababa",
      sample_group == "non_Addis" ~ region_hist != "Addis Ababa",
      TRUE ~ !region_hist %in% c("Addis Ababa", "Amhara")
    ))
    eligible <- in_group & cohort_data$legacy_comparison_eligible
    if (sample_group == "Addis") {
      eligible <- eligible & !is.na(cohort_data$entype_r4_hist)
    }
    map_dfr(c("salary", "wage"), function(outcome_name) {
      outcome_observed <- !is.na(cohort_data[[paste0(outcome_name, "_annual_r7")]])
      tibble(
        cohort = cohort_name,
        group = sample_group,
        outcome = outcome_name,
        master_rows = sum(in_group),
        legacy_comparison_eligible = sum(eligible),
        complete_current_controls = sum(eligible & cohort_data$r7_household_controls_complete & outcome_observed),
        complete_current_controls_and_food = sum(eligible & cohort_data$r7_household_plus_food_controls_complete & outcome_observed)
      )
    })
  })
})
write_audit(sample_counts, "candidate_sample_counts.csv")
stopifnot(identical(protected_hashes, tools::md5sum(protected_files)))
write_audit(data.frame(path = names(protected_hashes), md5 = unname(protected_hashes), unchanged = TRUE), "legacy_integrity.csv")
writeLines(capture.output(sessionInfo()), file.path(audit_dir, "session.txt"))
message("Created R7 master: ", nrow(master), " in-person participants; CSV and RDS in ", destination)
