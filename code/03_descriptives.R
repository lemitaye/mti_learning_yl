# =============================================================================
# Young Lives Ethiopia: descriptive statistics and employment figure
# Project: Mother Tongue Instruction, Learning and Employment
# Authors: Teferi Mergo and Lemi Daba
#
# Inputs: data/aa_samp.csv and data/non_aa_samp.csv (older cohort).
# Outputs: output/tables/tableI.tex and output/figures/employ_gr.pdf, mirrored
# to the revision project unless MTI_OVERLEAF_DIR is empty.
# Run from the repository root after helpers/_setup.R, or via _master.R.
#
# Both samples exclude zero/missing R4 language scores. Summary statistics use
# available cases separately by variable, not a single complete-case sample.
# Salary (activity 13) is included in broader wage employment (5/12/13);
# their missing-value/exclusion rules and therefore denominators differ.
# =============================================================================

rm(list = ls())
source("code/helpers/analysis_helpers.R")

aa_sample <- read_analysis_sample("data/aa_samp.csv")
non_aa_sample <- read_analysis_sample("data/non_aa_samp.csv")

# ---- Figure 1: available-case employment means by region --------------------
employ_gr <- non_aa_sample %>%
  group_by(region) %>%
  summarise(
    salary_employ_pct = mean(wage_employII, na.rm = TRUE),
    wage_employ_pct = mean(wage_employ, na.rm = TRUE)
  ) %>%
  pivot_longer(
    c(salary_employ_pct, wage_employ_pct),
    names_to = "employ_type",
    values_to = "pct_employ"
  ) %>%
  mutate(
    employ_type = factor(employ_type,
                         levels = c("salary_employ_pct", "wage_employ_pct"),
                         labels = c("Salary Employed", "Wage Employed"))
  ) %>%
  ggplot(aes(fct_reorder(region, pct_employ), pct_employ, fill = employ_type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  labs(
    x = "", y = "Percent Employed",
    fill = "Employment Type"
  )

# Base PDF avoids the optional Cairo/XQuartz system dependency.
ggsave(
  filename = "output/figures/employ_gr.pdf",
  plot = employ_gr,
  device = grDevices::pdf,
  width = 200,
  height = 120,
  units = "mm"
)

# ---- Table 1: build comparable variable sets --------------------------------
prepare_summary_input <- function(data, include_regions = FALSE) {
  selected <- data %>%
    mutate(male = (chsex == "Male"), care_female = (caresex_r1 == 2),
           stunted = (stunting == 1)) %>%
    select(
      male, zbfa, stunted, caredu_r1, careage_r1, care_woman = care_female,
      hhsize, wi_new, hq_new, cd_new, elecq_new, ownlandhse_r1,
      any_of(if (include_regions) "region" else character()),
      raw_maths, raw_lang, hghgrade_final_num, wage_employ,
      salary_employ = wage_employII,
      any_of(if (include_regions) "rural_3" else character())
    )
  if (include_regions) {
    selected <- selected %>%
      dummy_cols("region") %>%
      select(-region) %>%
      mutate(across(c(rural_3, starts_with("region_")), as.logical))
  }
  selected
}

# Logical indicators have no printed SD, matching the original table.
summary_statistics <- function(data, suffix) {
  data.frame(
    variable = names(data),
    obs = map_dbl(data, ~ sum(!is.na(.x))),
    mean = round(map_dbl(data, mean, na.rm = TRUE), 3),
    sd = round(map_dbl(data, ~ if (is.logical(.x)) NA_real_ else sd(.x, na.rm = TRUE)), 3),
    row.names = NULL
  ) %>% rename_with(~ paste0(.x, suffix), -variable)
}

non_aa_summary_input <- prepare_summary_input(non_aa_sample, include_regions = TRUE)
aa_summary_input <- prepare_summary_input(aa_sample)
summary_table <- full_join(
  summary_statistics(non_aa_summary_input, "_nonaa"),
  summary_statistics(aa_summary_input, "_aa"), by = "variable"
) %>%
  mutate(empty0 = NA, empty1 = NA) %>%
  select(empty0, variable, obs_nonaa, mean_nonaa, sd_nonaa,
         empty1, obs_aa, mean_aa, sd_aa)

# Named labels also specify the published row order.
summary_labels <- c(
  male = "Dummy for male child",
  hghgrade_final_num = "Highest grade completed",
  raw_maths = "Maths test Score (Raw)", raw_lang = "Language test Score (Raw)",
  wage_employ = "Dummy for wage Employment", salary_employ = "Dummy for salaried Employment",
  zbfa = "BMI-for-age z-score", stunted = "Dummy for stunting",
  care_woman = "Dummy for female caregiver", careage_r1 = "Caregiver's age",
  caredu_r1 = "Caregiver's highest grade", hhsize = "Household size",
  wi_new = "Wealth index", hq_new = "Housing quality index", cd_new = "Consumer durables index",
  elecq_new = "Household has access to electricity",
  ownlandhse_r1 = "Household owns land where house is on",
  rural_3 = "Household resides in a rural area",
  region_Amhara = "Dummy for Amhara", region_Oromia = "Dummy for Oromia",
  region_SNNP = "Dummy for SNNP", region_Tigray = "Dummy for Tigray"
)
summary_table$variable <- factor(unname(summary_labels[summary_table$variable]),
                                 levels = unname(summary_labels))
summary_table <- summary_table[order(summary_table$variable), ]
row.names(summary_table) <- NULL
summary_table$variable <- as.character(summary_table$variable)

# ---- Write the original LaTeX layout ---------------------------------------
summary_latex <- xtable(
  summary_table, display = c("s", "s", "s", "g", "g", "g", "s", "g", "g", "g"),
  digits = 4, caption = "Summary Statistics", label = "tab:sum-stat"
)

summary_note <- paste0(" \n \\\\[-1.8ex] \\multicolumn{6}{l}",
               "{\\footnotesize{\\textit{Note:} The standard deviations for proportions is
               not presented.}} \n")

table_header_rows <- list()
table_header_rows$pos <- list(0, 0, 0, 0, 0, 22, 22)
table_header_rows$command <- c(
  " \\\\[-1.8ex]",
  " & & \\multicolumn{3}{c}{Non AA Sample} & & \\multicolumn{3}{c}{AA Sample}  \\\\[0.2ex]",
  "\\cline{3-5}  \\cline{7-9}  \\\\[-1.2ex]" ,
  " & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} \\\\",
  " \\\\[-1.8ex] &  & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & &
  \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)} \\\\ ",
  " \\bottomrule ",
  summary_note
)

print(
  summary_latex,
  add.to.row = table_header_rows,
  include.rownames = FALSE, include.colnames = FALSE,
  booktabs = TRUE, caption.placement = "top",
  hline.after = c(-1, 0),
  file = "output/tables/tableI.tex"
)

# Local outputs are complete before attempting the optional Dropbox mirror.
local({
  source("code/helpers/export_paths.R", local = TRUE)
  mirror_output("output/tables/tableI.tex", "tables")
  mirror_output("output/figures/employ_gr.pdf", "figures")
})
