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
# Summary display: R5 first-ranked slot; R7 annual main activity.
# Regression outcomes remain the previously approved any/main indicators.
employment_variables <- c(
  "self_agri", "wage_agri", "self_nonag", "wage_employ", "salary_employ"
)
sample <- sample |>
  mutate(
    employment_categories = rowSums(pick(all_of(employment_variables))),
    employment_partition = case_when(
      round == 5 & r5_interview_observed == 0 ~ "not_interviewed",
      active == 0 ~ "no_activity",
      type_activ_1 %in% c(1:4, 8:11) ~ "self_employed",
      type_activ_1 %in% c(5:7, 12, 19) ~ "wage_employed",
      type_activ_1 == 13 ~ "salary_employed",
      type_activ_1 %in% c(15, 20) ~ "other_work",
      TRUE ~ "activity_unknown"
    )
  )
partition_labels <- c(
  self_employed = "Self-employed (farm/nonfarm)",
  wage_employed = "Wage employed (farm/nonfarm)",
  salary_employed = "Salary employed",
  other_work = "Other recorded work",
  no_activity = "Interviewed: no reported activity",
  activity_unknown = "First activity type unknown",
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
    observations = n(),
    multiple_before_ranking = sum(employment_categories > 1),
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
  exclusive_wage_employed = "Wage employed (farm/nonfarm)",
  exclusive_salary_employed = "Salary employed",
  exclusive_self_employed = "Self-employed (farm/nonfarm)",
  exclusive_other_work = "Other recorded work",
  exclusive_no_activity = "Interviewed: no reported activity",
  exclusive_activity_unknown = "First activity type unknown",
  exclusive_not_interviewed = "R5 not interviewed",
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
  "\\item Employment shares use the first-ranked R5 activity",
  "and R7 annual main activity. For R5 multiple jobs, only the first slot",
  "determines the displayed category. Wage combines codes5--7/12/19;",
  "self-employment combines1--4/8--11; salary is13. Other work is15/20.",
  "No activity, unknown first type and non-interviews remain separate.",
  "The exhaustive display shares sum to one before rounding.",
  "\\item These summary shares do not redefine regression outcomes,",
  "which retain R5 ANY-activity and R7 MAIN-activity coverage.",
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
  "R5 uses its first-ranked activity even when later activities differ.",
  "R7 classification uses annual main activity. Wage combines agricultural",
  "and nonfarm wage work; self-employment combines farm and nonfarm work.",
  "Other recorded work uses codes15/20. Active respondents with an unknown",
  "first activity type are kept in a separate unknown group, not re-ranked.",
  "R5 non-interviews are separate from interviewed/no-reported-activity.",
  "Shares may differ slightly from one when individually rounded.",
  "This display grouping does not alter the regression outcomes.",
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
file <- "output/tables/tableI_employment_partition.tex"
writeLines(lines, file)
mirror_output(file, "tables")
print(coverage_audit)
