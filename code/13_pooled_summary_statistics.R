# Table 1 for the pooled employment design: R5 OC and R7 both cohorts.
# Available cases by variable; do not impose regression complete cases.
# R5 YC employment is structurally missing and is excluded from this design.

library(tidyverse)
source("code/helpers/export_paths.R")
audit_dir <- "temp/pooled_summary_statistics"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

sample <- readRDS("data/r7/r5_r7_appended.rds") |>
  mutate(across(where(is.numeric), ~ as.numeric(haven::zap_labels(.x)))) |>
  filter(round == 7 | yc == 0) |>
  filter(yc == 1 | (!is.na(raw_lang_r4) & raw_lang_r4 != 0))

# Highest grade is historical R5 attainment, not an invented R7 equivalent.
lookup <- readRDS("data/r7/r7_inperson_analysis.rds") |>
  transmute(child_id, grade_r5 = hghgrade_final_num_hist)
legacy <- bind_rows(
  read.csv("data/aa_samp.csv"), read.csv("data/non_aa_samp.csv"),
  read.csv("data/ylsample_yc.csv")
) |> transmute(child_id, grade_r5 = hghgrade_final_num)
lookup <- bind_rows(legacy, anti_join(lookup, legacy, by = "child_id"))
sample <- sample |>
  left_join(lookup, by = "child_id", relationship = "many-to-one") |>
  mutate(
    male = chsex == "Male",
    older = yc == 0, younger = yc == 1, in_r7 = round == 7,
    stunted = stunting == 1, caregiver_female = caresex_r1 == 2,
    rural = rural_3 == 1,
    amhara = region == "Amhara", oromia = region == "Oromia",
    snnp = region == "SNNP", tigray = region == "Tigray"
  )
# Any-activity indicators can overlap across different R5 jobs.
employment_variables <- c(
  "self_agri", "wage_agri", "self_nonag", "wage_employ", "salary_employ"
)
sample <- sample |>
  mutate(
    employment_categories = rowSums(pick(all_of(employment_variables))),
    inactive_proxy = active == 0,
    multiple_categories = employment_categories > 1,
    employment_partition = case_when(
      round == 5 & r5_interview_observed == 0 ~ "not_interviewed",
      active == 0 ~ "no_activity",
      employment_categories > 1 ~ "multiple",
      employment_categories == 0 ~ "other_work",
      self_agri == 1 ~ "self_agri_only",
      wage_agri == 1 ~ "wage_agri_only",
      self_nonag == 1 ~ "self_nonag_only",
      wage_employ == 1 ~ "wage_only",
      salary_employ == 1 ~ "salary_only"
    )
  )
partition_labels <- c(
  self_agri_only = "Agricultural self-employment only",
  wage_agri_only = "Agricultural wage work only",
  self_nonag_only = "Nonfarm self-employment only",
  wage_only = "Nonfarm wage work only",
  salary_only = "Salary employment only",
  multiple = "Multiple employment categories",
  other_work = "Other recorded work",
  no_activity = "Interviewed: no reported activity",
  not_interviewed = "R5 not interviewed"
)
stopifnot(!anyNA(sample$employment_partition))
for (category in names(partition_labels)) {
  sample[[paste0("exclusive_", category)]] <-
    sample$employment_partition == category
}
partition_columns <- paste0("exclusive_", names(partition_labels))
stopifnot(all(rowSums(sample[partition_columns]) == 1))
coverage_audit <- sample |>
  group_by(round, cohort) |>
  summarise(
    observations = n(), multiple_categories = sum(multiple_categories),
    partition_sum = mean(rowSums(pick(all_of(partition_columns)))),
    .groups = "drop"
  )
write_csv(coverage_audit,
          file.path(audit_dir, "employment_partition_check.csv"))

labels <- c(
  older = "Older cohort", younger = "Younger cohort", in_r7 = "Round 7",
  male = "Male participant", grade_r5 = "Highest grade completed (R5)",
  raw_maths_r4 = "Maths test score (R4 raw)",
  raw_lang_r4 = "Language test score (R4 raw)",
  wage_employ = "Nonfarm wage employed", salary_employ = "Salary employed",
  self_agri = "Agricultural self-employed",
  wage_agri = "Agricultural wage employed",
  self_nonag = "Nonfarm self-employed",
  self_employ = "Combined self-employed (farm/nonfarm)",
  inactive_proxy = "Inactive / no reported activity",
  multiple_categories = "Multiple employment categories",
  zbfa = "BMI-for-age z-score (R1)", stunted = "Stunted (R1)",
  caregiver_female = "Female caregiver (R1)",
  careage_r1 = "Caregiver age (R1)",
  caredu_r1 = "Caregiver highest grade (R1)",
  hhsize = "Household size", wi_new = "Wealth index",
  hq_new = "Housing quality index", cd_new = "Consumer durables index",
  elecq_new = "Access to electricity",
  ownlandhse_r1 = "Owns land under house (R1)",
  rural = "Rural residence (R3)", amhara = "Amhara", oromia = "Oromia",
  snnp = "SNNP", tigray = "Tigray"
)
statistics <- map_dfr(c("Non-Addis", "Addis"), function(group) {
  d <- sample |> filter(if (group == "Addis") {
    region == "Addis Ababa"
  } else region != "Addis Ababa")
  map_dfr(names(labels), function(variable) {
    value <- d[[variable]]
    # Retain the original suppression of SDs for logical proportions.
    tibble(
      group = group, variable = variable, n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = if (is.logical(value)) NA_real_ else sd(value, na.rm = TRUE)
    )
  })
})
write_csv(statistics, file.path(audit_dir, "summary_statistics.csv"))
write_csv(count(sample, round, cohort, region),
          file.path(audit_dir, "sample_counts.csv"))

# Export in the original N/mean/SD layout, separately by Addis residence.
ending <- strrep(intToUtf8(92), 2)
row <- function(values) paste(paste(values, collapse = " & "), ending)
fmt <- function(value) ifelse(is.na(value), "", sprintf("%.3f", value))
lines <- c(
  "% Generated by code/13_pooled_summary_statistics.R",
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Summary statistics: pooled rounds 5 and 7}",
  "\\label{tab:sum-stat}",
  "\\linespread{1}\\selectfont",
  "\\begin{threeparttable}\\footnotesize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{lrrrrrr}\\toprule",
  row(c("", "\\multicolumn{3}{c}{Non-Addis sample}",
        "\\multicolumn{3}{c}{Addis sample}")),
  "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  row(c("", "N", "Mean", "SD", "N", "Mean", "SD")),
  "\\midrule"
)
for (variable_name in names(labels)) {
  s <- statistics |> filter(variable == variable_name)
  a <- s |> filter(group == "Non-Addis")
  b <- s |> filter(group == "Addis")
  lines <- c(lines, row(c(
    labels[[variable_name]], a$n, fmt(a$mean), fmt(a$sd),
    b$n, fmt(b$mean), fmt(b$sd)
  )))
}
lines <- c(lines,
  "\\bottomrule\\end{tabular}",
  "\\begin{tablenotes}\\scriptsize",
  "\\item Pooled R5 older cohort and R7 both cohorts; in-person R7 only.",
  "OC retains the historical R4 score gate; YC does not. N is available",
  "participant-round observations. Logical indicators have no displayed SD.",
  "\\item Employment indicators can overlap across R5 jobs; combined",
  "self-employment also overlaps its farm/nonfarm components. Inactive/no",
  "reported activity includes R5 non-interviews under the supplied coding.",
  "The continuation reports an exclusive partition whose shares sum to one.",
  "\\item Salary=13, nonfarm wage=12/19, agricultural self-work=1--4,",
  "agricultural wage=5--7, nonfarm self-work=8--11. R5 covers ANY reported",
  "activity, R7 MAIN only; R7 secondary job types are unavailable.",
  "\\item R1 caregiver/nutrition, R3 geography and R5 schooling/R4 scores",
  "are historical. Resources are R3 in R5 rows and current R7 in R7 rows.",
  "No regression complete-case restriction is imposed.",
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
file <- "output/tables/tableI_pooled.tex"
writeLines(lines, file)
mirror_output(file, "tables")
print(count(sample, round, cohort))


# Continue Table 1 with an exhaustive, mutually exclusive display grouping.
# This does not change any employment dummy used in the regressions.
partition_stats <- map_dfr(c("Non-Addis", "Addis"), function(group) {
  d <- sample |> filter(if (group == "Addis") {
    region == "Addis Ababa"
  } else region != "Addis Ababa")
  map_dfr(names(partition_labels), function(category) {
    x <- d[[paste0("exclusive_", category)]]
    tibble(group = group, category = category, n = length(x),
           mean = mean(x))
  })
})
write_csv(partition_stats,
          file.path(audit_dir, "exclusive_employment_shares.csv"))
stopifnot(all(abs(tapply(partition_stats$mean, partition_stats$group,
                        sum) - 1) < 1e-12))
lines <- c(
  "% Continuation of pooled Table 1; requires the caption package.",
  "\\begin{table}[!htbp]\\centering\\ContinuedFloat",
  "\\caption{Summary statistics (continued): employment groups}",
  "\\label{tab:employment-partition}",
  "\\linespread{1}\\selectfont",
  "\\begin{threeparttable}\\footnotesize",
  "\\begin{tabular}{lrrrr}\\toprule",
  row(c("", "\\multicolumn{2}{c}{Non-Addis}",
        "\\multicolumn{2}{c}{Addis}")),
  row(c("", "N", "Share", "N", "Share")), "\\midrule"
)
for (category_name in names(partition_labels)) {
  a <- partition_stats |>
    filter(group == "Non-Addis", category == category_name)
  b <- partition_stats |>
    filter(group == "Addis", category == category_name)
  lines <- c(lines, row(c(partition_labels[[category_name]],
                         a$n, fmt(a$mean), b$n, fmt(b$mean))))
}
lines <- c(lines,
  "\\midrule", row(c("Sum of shares", "", "1.000", "", "1.000")),
  "\\bottomrule\\end{tabular}\\begin{tablenotes}\\scriptsize",
  "\\item Same sample as Table 1. Each observation belongs to one group.",
  "Only means membership in exactly one of the five employment categories;",
  "respondents in two or more are assigned to Multiple employment categories.",
  "Other recorded work means active but outside those five categories.",
  "R5 non-interviews are shown separately from interviewed participants",
  "with no reported activity. Rounded category shares may not add exactly.",
  "\\item R5 categories use any reported activity; R7 uses main activity",
  "only. The absence of R7 overlaps reflects its limited measurement scope,",
  "not evidence that R7 respondents had no secondary jobs.",
  "This display grouping does not alter the regression outcomes.",
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
file <- "output/tables/tableI_employment_partition.tex"
writeLines(lines, file)
mirror_output(file, "tables")
print(coverage_audit)
