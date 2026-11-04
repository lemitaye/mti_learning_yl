# Check the approved exclusions and first-ranked/main employment outcomes.
library(tidyverse)
library(haven)
source("code/helpers/pooled_employment_sample.R")
out <- "temp/pooled_sample_revision"
full <- readRDS("data/r7/r5_r7_appended_full.rds") |>
  mutate(across(where(is.numeric), ~ as.numeric(zap_labels(.x))))
data <- readRDS("data/r7/r5_r7_appended.rds") |>
  mutate(across(where(is.numeric), ~ as.numeric(zap_labels(.x))))
expected <- classify_pooled_employment(full) |>
  filter(pooled_sample_eligible)
stopifnot(
  nrow(full) == 4942, nrow(data) == 2805,
  !anyDuplicated(paste(data$child_id, data$round)),
  setequal(paste(data$child_id, data$round),
           paste(expected$child_id, expected$round)),
  all(data$employment_group %in%
        c("self_employed", "wage_employed", "salary_employed", "no_activity")),
  all(data$regression_activity_basis == "ranked"),
  all(data$wage_employ == as.integer(data$type_activ_1 %in%
                                    c(5:7, 12, 19))),
  all(data$salary_employ == as.integer(data$type_activ_1 %in% 13)),
  all(data$self_employ == as.integer(data$type_activ_1 %in% c(1:4, 8:11))),
  all(data$wage_employ + data$salary_employ + data$self_employ == data$active)
)
# Match first-ranked R5 types directly to the archived activity roster.
raw <- read_dta(paste0(
  "rawdata/yl_r5/UKDA-8357-stata/stata/stata13/ethiopia_r5/",
  "etoc_ch_anon/et_r5_occh_activity.dta"
)) |> filter(ACTIDR5 == 1)
r5 <- data |> filter(round == 5)
i <- match(r5$child_id, raw$CHILDCODE)
for (pair in list(list("wage_employ", c(5:7, 12, 19)),
                  list("salary_employ", 13),
                  list("self_employ", c(1:4, 8:11)))) {
  positive_ids <- raw$CHILDCODE[raw$ACTR5 %in% pair[[2]]]
  stopifnot(all(r5[[pair[[1]]]] == as.integer(r5$child_id %in% positive_ids)))
}
counts <- full |> count(round, cohort, employment_group,
                       pooled_sample_eligible)
write_csv(counts, file.path(out, "selection_counts.csv"))
writeLines(c(
  "PASS: 2805 selected rows exactly match the approved exclusion rule.",
  "PASS: all three rejected categories absent; R5 YC unavailable excluded.",
  "PASS: combined wage, salary and self-work match first-ranked raw R5.",
  "PASS: regression employment indicators are mutually exclusive.",
  "PASS: active=sum(wage,salary,self); inactive completes the partition."
), file.path(out, "validation.txt"))
print(count(data, round, cohort))
