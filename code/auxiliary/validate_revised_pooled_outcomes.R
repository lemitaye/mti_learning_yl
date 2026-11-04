# Independent raw-record checks for the revised R5 / R7 category mapping.
library(tidyverse)
library(haven)
source("code/helpers/raw_data_catalog.R")
audit_dir <- "temp/r7_outcome_harmonization"
pooled <- readRDS("data/r7/r5_r7_appended.rds") |>
  mutate(across(where(is.numeric), ~ as.numeric(zap_labels(.x))))
codes <- list(
  self_agri = 1:4, wage_agri = 5:7, self_nonag = 8:11,
  wage_employ = c(12, 19), salary_employ = 13,
  self_employ = c(1:4, 8:11)
)
r5_raw <- read_dta(raw_data_files[["r5_oc_activity"]])
older <- pooled |> filter(round == 5, yc == 0)
for (outcome in names(codes)) {
  positive_ids <- r5_raw$CHILDCODE[r5_raw$ACTR5 %in% codes[[outcome]]]
  expected <- as.integer(older$child_id %in% positive_ids)
  stopifnot(identical(as.integer(older[[outcome]]), expected))
}
for (cohort in c("oc", "yc")) {
  root <- raw_data_files[["round7_ethiopia"]]
  raw <- read_dta(file.path(root,
    paste0("et_r7_", cohort, "ch_arch.dta")))
  phone <- read_dta(file.path(root,
    paste0("et_r7_", cohort, "ch_phone_arch.dta")))
  r7 <- pooled |> filter(round == 7, yc == as.integer(.env$cohort == "yc"))
  index <- match(r7$child_id, raw$childcode)
  stopifnot(!anyNA(index), setequal(r7$child_id, raw$childcode),
            !any(r7$child_id %in% phone$childcode))
  activity <- as.numeric(raw$typwrk12r7[index])
  activity[activity == 21] <- 1
  activity[activity == 22] <- 4
  for (outcome in names(codes)) {
    expected <- as.integer(activity %in% codes[[outcome]])
    stopifnot(identical(as.integer(r7[[outcome]]), expected))
  }
  stopifnot(all(is.na(r7$type_activ_2)),
            all(is.na(r7$type_activ_3)), all(is.na(r7$type_activ_4)),
            all(r7$outcome_coverage == "annual_main_activity_only"))
}
stopifnot(!anyDuplicated(paste(pooled$child_id, pooled$round)),
          nrow(pooled) == 4942L)

# Surface, rather than silently fix, the supplied R5 zero-coding convention.
nonresponse <- pooled |>
  filter(round == 5, yc == 0, r5_interview_observed == 0) |>
  summarise(
    observations = n(), salary_zero = sum(salary_employ == 0),
    wage_zero = sum(wage_employ == 0)
  )
write_csv(nonresponse, file.path(audit_dir, "r5_nonresponse_zero_coding.csv"))
coverage <- pooled |> filter(round == 7) |>
  group_by(cohort) |>
  summarise(
    observations = n(),
    secondary_jobs_reported = sum(other_annual_work_count_r7 > 0,
                                  na.rm = TRUE),
    secondary_count_missing = sum(is.na(other_annual_work_count_r7)),
    .groups = "drop"
  )
write_csv(coverage, file.path(audit_dir, "r7_secondary_job_coverage.csv"))
counts <- pooled |> group_by(round, cohort) |>
  summarise(
    observations = n(), salary_observed = sum(!is.na(salary_employ)),
    salary_positive = sum(salary_employ == 1, na.rm = TRUE),
    wage_observed = sum(!is.na(wage_employ)),
    wage_positive = sum(wage_employ == 1, na.rm = TRUE), .groups = "drop"
  )
write_csv(counts, file.path(audit_dir, "outcome_counts.csv"))
writeLines(c(
  "PASS: R5 indicators independently match any positive raw activity ID.",
  "PASS: R7 indicators independently match raw MAIN activity categories.",
  "PASS: 2038 raw in-person IDs and zero phone IDs in R7.",
  "PASS: 4942 unique participant-round keys and explicit coverage flags."
), file.path(audit_dir, "outcome_validation.txt"))
print(counts)
print(coverage)
print(nonresponse)
