# Explain outcome missingness without changing data or fitted models.
# Reproduce the exact Table 1 selection before attributing missing values.
library(tidyverse)

out <- "temp/employment_missingness"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
input <- "data/r7/r5_r7_appended.rds"
input_hash <- tools::md5sum(input)
data <- readRDS(input) |>
  mutate(across(where(is.numeric), ~ as.numeric(haven::zap_labels(.x))))
full_counts <- data |>
  group_by(round, cohort) |>
  summarise(
    rows = n(), wage_missing = sum(is.na(wage_employ)),
    salary_missing = sum(is.na(wage_employII)), .groups = "drop"
  )
write_csv(full_counts, file.path(out, "full_file_counts.csv"))
sample <- data |>
  filter(round == 7 | yc == 0) |>
  filter(yc == 1 | (!is.na(raw_lang_r4) & raw_lang_r4 != 0)) |>
  mutate(group = if_else(region == "Addis Ababa", "Addis", "Non-Addis"))

# Preserve the legacy definitions: outcome-specific exclusions are NA.
long <- sample |>
  pivot_longer(c(wage_employ, wage_employII),
    names_to = "outcome", values_to = "value") |>
  mutate(
    reason = case_when(
      !is.na(value) ~ "Observed outcome",
      outcome == "wage_employ" & type_activ %in% 1:4 ~
        "Excluded: agricultural self-employment",
      type_activ %in% 8:11 ~ "Excluded: nonfarm self-employment",
      type_activ == 19 ~ "Excluded: housemaid",
      is.na(type_activ) & is.na(active) ~
        "Unknown: activity and work participation missing",
      TRUE ~ "Other unexplained missing"
    )
  )
stopifnot(!any(long$reason == "Other unexplained missing"))
counts <- long |>
  group_by(group, outcome) |>
  summarise(
    rows = n(), observed = sum(!is.na(value)),
    missing = sum(is.na(value)),
    missing_percent = 100 * mean(is.na(value)), .groups = "drop"
  )
reasons <- long |>
  count(group, outcome, reason, name = "observations")
by_round <- long |>
  count(group, round, cohort, outcome, reason, name = "observations")
activity_counts <- long |>
  filter(is.na(value)) |>
  count(group, round, cohort, outcome, type_activ, reason,
        name = "observations")
write_csv(counts, file.path(out, "table1_counts.csv"))
write_csv(reasons, file.path(out, "missingness_reasons.csv"))
write_csv(by_round, file.path(out, "reasons_by_round_cohort.csv"))
write_csv(activity_counts, file.path(out, "excluded_activity_counts.csv"))

# Check displayed Table 1 Ns against its saved summary-statistics audit.
summary <- read.csv("temp/pooled_summary_statistics/summary_statistics.csv")
for (i in seq_len(nrow(counts))) {
  row <- counts[i, ]
  reference <- summary |>
    filter(group == row$group, variable == row$outcome)
  stopifnot(nrow(reference) == 1, reference$n == row$observed)
}
# Valid annual nonworkers are zero in R7, not treated as missing outcomes.
nonworkers <- sample |>
  filter(round == 7, active == 0)
stopifnot(all(nonworkers$wage_employ == 0),
          all(nonworkers$wage_employII == 0),
          identical(input_hash, tools::md5sum(input)))
print(counts)
print(reasons, n = Inf)

# Trace unknowns to the original R5 constructed participation and raw module.
source("code/helpers/raw_data_catalog.R")
r5 <- haven::read_dta(raw_data_files[["constructed_legacy"]]) |>
  filter(round == 5, yc == 0) |>
  mutate(child_id = as.numeric(str_remove(childid, "ET")))
activity <- haven::read_dta(raw_data_files[["r5_oc_activity"]])
unknown <- sample |>
  filter(round == 5, is.na(type_activ), is.na(active)) |>
  select(child_id, group) |>
  left_join(r5 |> select(child_id, inround, hwork), by = "child_id") |>
  mutate(
    raw_activity_record = child_id %in% activity$CHILDCODE,
    raw_main_record = child_id %in%
      activity$CHILDCODE[activity$ACTIDR5 == 1]
  )
unknown_audit <- unknown |>
  count(group, inround, raw_activity_record, raw_main_record,
        name = "observations")
write_csv(unknown_audit, file.path(out, "r5_unknown_source_audit.csv"))
print(unknown_audit, n = Inf)
