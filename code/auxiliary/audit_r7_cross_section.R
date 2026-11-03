# Export aggregate R7 build diagnostics and the variable dictionary.
# Called by code/10_build_r7_cross_section.R after saving the master dataset.
# Requires master, checks, sources, labels, definitions, define_variable,
# write_audit, protected_files, protected_hashes, and audit_dir from the
# build.
# Run the main build script to generate these inputs; do not run this alone.

# 1. Source and reconstruction checks --------------------------------------

write_audit(bind_rows(checks), "build_checks.csv")

write_audit(bind_rows(sources), "r7_source_manifest.csv")

write_audit(bind_rows(labels), "source_value_labels.csv")

# 2. Cohort and outcome coverage -------------------------------------------

cohort_summary <- master |>
  group_by(cohort) |>
  summarise(
    rows = n(),
    age_min = min(age_years_r7, na.rm = TRUE),
    age_max = max(age_years_r7, na.rm = TRUE),
    r5_schooling_below8_or_missing = sum(yc_r5_schooling_below8_or_missing),
    imti_candidate_changed = sum(abs(
      IMTI_r7_schooling_candidate - IMTI_hist) > 1e-8, na.rm = TRUE),
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

# 3. Variable definitions --------------------------------------------------
define_variable("child_id", "Raw R7 childcode", "All rounds", paste0(
  "Unique numeric participant join key; childid ",
  "retains the ET-prefixed string"))
define_variable("yc", "Raw cohort file + constructed cohort check",
  "Time invariant", "0=older cohort; 1=younger cohort")
define_variable("IMTI_hist", "Original legacy analysis lookup",
  "Schooling through R5", paste0(
  "Frozen original schooling-capped MTI measure; ",
  "YC may complete primary schooling after R5"))
define_variable("E_is_hist",
  "Original legacy policy/ethnicity/language rules", "Historical assignment",
  paste0("Frozen policy instrument; current residence ",
  "must not overwrite policy geography"))
define_variable("legacy_comparison_eligible", paste0(
  "Existing legacy CSV membership and original OC ", "language gate"),
  "Sample flag", paste0("OC: original history member and raw_lang ",
  "nonzero/nonmissing; YC: original history ", "member. Not applied to master"))
define_variable("yc_exposure_review_required", "Cohort flag", "R7", paste0(
  "TRUE for all YC: validate completed-primary MTI ",
  "exposure before estimation"))
define_variable("yc_r5_schooling_below8_or_missing",
  "Legacy completed schooling", "R5", paste0(
  "Conservative review flag for YC schooling below ",
  "eight or missing; not a finding of miscoding"))
define_variable("reading_score_r7", "Not scored", "R7", paste0(
  "Intentionally missing: archived raw reading ",
  "responses require a validated answer key and a ", "comparability decision"))
define_variable("reading_items_observed_r7", "Raw raq1r7 to raq12r7", "R7",
  paste0("Count of nonmissing raw response cells, not ",
  "correct answers or a validated test-completion ", "flag"))
define_variable("foodsec_r7", "Raw fdsec00r7", "R7", paste0(
  "Food situation category, valid codes 1:4; same ",
  "four-category construct as historical control, ", "different date"))
define_variable("electricity_r7", "Raw electr7", "R7",
  "0=no, 1=yes; missing and special codes remain NA")
define_variable("r7_household_controls_complete",
  "Outcome-independent completeness", "Mixed timing", paste0(
  "Frozen IMTI/E_is, sex, childhood nutrition, R1 ",
  "caregiver/land and R3 food; R7 size, ", "WI/HQ/CD/electricity"))
define_variable("r7_household_plus_food_controls_complete",
  "Outcome-independent completeness", "Mixed timing", paste0(
  "Same as household completeness but replaces R3 ",
  "food situation with R7 food situation"))

for (w in c("annual", "weekly_job")) {
  for (y in c("salary", "wage")) {
    define_variable(
      paste0(y, "_", w, "_allrespondents_r7"),
        "Raw activity and valid nonwork screens", w,
      paste0("Alternative denominator: any valid other ",
        "activity=0, including categories excluded in ",
        "legacy outcome; known nonwork=0")
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

# 4. Candidate sample sizes -----------------------------------------------
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
      outcome_observed <- !is.na(cohort_data[[paste0(outcome_name,
        "_annual_r7")]])
      tibble(
        cohort = cohort_name,
        group = sample_group,
        outcome = outcome_name,
        master_rows = sum(in_group),
        legacy_comparison_eligible = sum(eligible),
        complete_current_controls = sum(eligible &
          cohort_data$r7_household_controls_complete & outcome_observed),
        complete_current_controls_and_food = sum(eligible &
          cohort_data$r7_household_plus_food_controls_complete &
          outcome_observed)
      )
    })
  })
})

write_audit(sample_counts, "candidate_sample_counts.csv")
# 5. Confirm protected inputs and outputs are unchanged --------------------

stopifnot(identical(protected_hashes, tools::md5sum(protected_files)))

write_audit(data.frame(path = names(protected_hashes), md5 = unname(
  protected_hashes), unchanged = TRUE), "legacy_integrity.csv")
writeLines(capture.output(sessionInfo()), file.path(audit_dir, "session.txt"))
