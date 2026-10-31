
# Date created: April 30, 2022
# Depends on: extract_clean.R

# Figures and Descriptive Statistics

rm(list = ls())

# load the data:
aa_samp <- read_csv("data/aa_samp.csv") %>% 
  mutate_if(is.character, as.factor) %>% # convert all chr. to factor.
  filter(raw_lang != 0) 

non_aa_samp <- read_csv("data/non_aa_samp.csv") %>% 
  mutate_if(is.character, as.factor) %>% # convert all chr. to factor.
  filter(raw_lang != 0)   

# Some descriptives:

non_aa_samp %>% 
  ggplot(aes(IMTI, raw_maths)) +
  geom_point() +
  facet_wrap(~ region)

non_aa_samp %>% 
  filter(!is.na(wage_employ)) %>% 
  ggplot(aes(factor(wage_employII))) +
  geom_histogram(stat = "count") +
  facet_wrap(~ region)

non_aa_samp %>% 
  count(region, wage_employII)

non_aa_samp %>% 
  ggplot(aes(IMTI, wage_employII)) +
  geom_point(position = "jitter") +
  geom_hline(yintercept = 0.5, color = "blue") +
  facet_wrap(~ region)

non_aa_samp %>% 
  ggplot(aes(factor(region), hghgrade_final_num)) +
  geom_boxplot() 

non_aa_samp %>% 
  group_by(region) %>% 
  summarise(mean_wage = mean(wage_employII, na.rm = TRUE)) %>% 
  ggplot(aes(fct_reorder(region, mean_wage), mean_wage)) +
  geom_col() +
  coord_flip() 

# Use the analysis sample; non_aa_descr is built later for Table 1 and
# drops region and renames wage_employII, so it is unsuitable for these plots.
plot_by_region <- function(var) {
  non_aa_samp %>%
    group_by(region) %>% 
    summarise(mean_wage = mean({{var}}, na.rm = TRUE)) %>% 
    ggplot(aes(fct_reorder(region, mean_wage), mean_wage)) +
    geom_col() +
    coord_flip()
}
 

plot_by_region(wage_employII)
plot_by_region(hghgrade_final_num)

non_aa_samp %>% 
  ggplot(aes(region, hghgrade_final_num)) +
  geom_boxplot() +
  coord_flip()

plot_by_region(raw_maths)
plot_by_region(raw_lang)

non_aa_samp %>%
  group_by(region) %>% 
  summarise(mean_lang = mean(raw_lang, na.rm = TRUE)) %>% 
  ggplot(aes(fct_reorder(region, mean_lang), mean_lang)) +
  geom_col() +
  coord_flip()


# 1.
non_aa_samp %>% 
  mutate(
    in_moth_tongue = ( as.character(chlang) == as.character(lang_primary) )
    ) %>% 
  # count(region, in_moth_tongue) %>% 
  group_by(region) %>% 
  summarise( in_moth_tongue_prop = mean(in_moth_tongue, na.rm = TRUE) ) %>% 
  ggplot(aes(fct_reorder(region, in_moth_tongue_prop), in_moth_tongue_prop)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Region",
    y = "",
    title = "Percent of YL Children Ever Schooled in their Mother Tongue at Primary Level"
  )

# 2. Distribution of test scores by region
standardize <- function(x){ 
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) 
  return( z)
  }

non_aa_samp %>% 
  select(region, raw_maths, raw_lang) %>% 
  group_by(region) %>% 
  # Standardization returns one value per child, not one summary per region.
  mutate(
    std_maths = standardize(raw_maths),
    std_lang = standardize(raw_lang)
  ) %>%
  ungroup() %>%
  pivot_longer(c(std_maths, std_lang), names_to = "test_type", values_to = "std_score") %>% 
  ggplot(aes(color = region, linetype = region)) +
  geom_density(aes(std_score)) +
  facet_wrap(~ test_type, scales = "free_y") +
  theme(legend.position = "top") +
  labs(
    x = "Standardized Score",
    y = "Density",
    color = "", linetype = "",
    title = "Figure 2: Distribution of Standardized Test Scores by Region"
  )

# 3. Variation in wage & salary employment by region
employ_gr <- non_aa_samp %>% 
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
    employ_type = factor(
      employ_type, 
      levels = c("salary_employ_pct", "wage_employ_pct") ) %>% 
    fct_recode(
         "Wage Employed" = "wage_employ_pct", "Salary Employed" = "salary_employ_pct"
        )
  ) %>% 
  ggplot(aes( fct_reorder(region, pct_employ), pct_employ, fill = employ_type)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = percent) +
  coord_flip() +
  # theme(legend.position = "top") +
  labs(
    x = "", y = "Percent Employed",
    fill = "Employment Type"#,
    # title = "Figure 1: Percentage of YL Pupils That Were Employed by Round 5"
  )
  
ggsave(
  filename = "output/figures/employ_gr.pdf",
  plot = employ_gr,
  device = cairo_pdf,
  width = 200,
  height = 120,
  units = "mm"
)



# Table of Summary Statistics (Table I) ####
non_aa_descr <- non_aa_samp %>% 
  mutate(
    male = (chsex == "Male"),
    care_female = (caresex_r1 == 2),
    stunted = (stunting == 1)
  ) %>% 
  select(
    male, zbfa, stunted, caredu_r1, careage_r1, care_woman = care_female, hhsize, 
    wi_new, hq_new, cd_new, elecq_new, ownlandhse_r1, region, raw_maths,
    raw_lang, hghgrade_final_num, wage_employ, salary_employ = wage_employII, 
    rural_3
  ) %>% 
  # filter(complete.cases(.)) %>%
  dummy_cols(c("region")) %>% 
  select(-c(region)) %>% 
  mutate(
    rural_3 = as.logical(rural_3),
    region_Amhara = as.logical(region_Amhara),
    region_Oromia = as.logical(region_Oromia),
    region_SNNP = as.logical(region_SNNP),
    region_Tigray = as.logical(region_Tigray)
  )


aa_descr <- aa_samp %>% 
  mutate(
    male = (chsex == "Male"),
    care_female = (caresex_r1 == 2),
    stunted = (stunting == 1)
  ) %>% 
  select(
    male, zbfa, stunted, caredu_r1, careage_r1, care_woman = care_female, hhsize, 
    wi_new, hq_new, cd_new, elecq_new, ownlandhse_r1, raw_maths,
    raw_lang, hghgrade_final_num, wage_employ, salary_employ = wage_employII
  ) #%>% 
  # filter(complete.cases(.)) 
# %>% 
#   dummy_cols(c("region")) %>% 
#   select(-c(region))


sd_rm_bin <- function(x) {
  require(purrr)
  
  if ( is_logical(x) ) {
    res <- NA
  } else {
    res <- sd(x, na.rm = TRUE)
  }
  
  res
}

obs_nonaa <- non_aa_descr %>% 
  map_dbl(~sum(!is.na(.)))

mean_nonaa <- non_aa_descr %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_nonaa <- non_aa_descr %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

obs_aa <- aa_descr %>% 
  map_dbl(~sum(!is.na(.)))

mean_aa <- aa_descr %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_aa <- aa_descr %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

x <- cbind(obs_nonaa, mean_nonaa, sd_nonaa) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

y <- cbind(obs_aa, mean_aa, sd_aa) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

z <- full_join(x, y, by = "variable") 
z$empty0 <- NA
z$empty1 <- NA
z <- select(z, empty0, variable, obs_nonaa, mean_nonaa, sd_nonaa, empty1, 
            obs_aa, mean_aa, sd_aa)

z <- z %>% 
  mutate(
    variable = str_remove(
      variable, "region_"
    ),
    variable = str_replace_all(
      variable,
      c(
        "male" = "Dummy for male child",
        "hghgrade_final_num" = "Highest grade completed",
        "raw_maths" = "Maths test Score (Raw)",
        "raw_lang" = "Language test Score (Raw)", 
        "wage_employ" = "Dummy for wage Employment", 
        "salary_employ" = "Dummy for salaried Employment",
        "zbfa" = "BMI-for-age z-score",
        "stunted" = "Dummy for stunting",
        "care_woman" = "Dummy for female caregiver",
        "careage_r1" = "Caregiver's age", 
        "caredu_r1" = "Caregiver's highest grade", 
        "hhsize" = "Household size", 
        "wi_new" = "Wealth index", 
        "hq_new" = "Housing quality index", 
        "cd_new" = "Consumer durables index", 
        "elecq_new" = "Household has access to electricity", 
        "ownlandhse_r1" = "Household owns land where house is on",
        "rural_3" = "Household resides in a rural area",
        "Amhara" = "Dummy for Amhara",
        "Oromia" = "Dummy for Oromia",
        "SNNP" = "Dummy for SNNP",
        "Tigray" = "Dummy for Tigray"
      )
    )
  )  

cols <- pull(z, "variable")

cols_arrg <- c(
  "Dummy for male child",
  "Highest grade completed",
  "Maths test Score (Raw)",
  "Language test Score (Raw)", 
  "Dummy for wage Employment", 
  "Dummy for salaried Employment",
  "BMI-for-age z-score",
  "Dummy for stunting",
  "Dummy for female caregiver",
  "Caregiver's age", 
  "Caregiver's highest grade", 
  "Household size", 
  "Wealth index", 
  "Housing quality index", 
  "Consumer durables index", 
  "Household has access to electricity", 
  "Household owns land where house is on",
  "Household resides in a rural area",
  "Dummy for Amhara",
  "Dummy for Oromia",
  "Dummy for SNNP",
  "Dummy for Tigray"
)

z$variable <- factor(z$variable, levels = cols_arrg)
z <- z[order(z$variable), ]
row.names(z) <- NULL  # reset the rownames
z$variable <- as.character( z$variable )


xtab <- xtable(
  z, display = c("s", "s", "s", "g", "g", "g", "s", "g", "g", "g"),
  digits = 4, caption = "Summary Statistics", label = "tab:sum-stat"
) 

comm <- paste0(" \n \\\\[-1.8ex] \\multicolumn{6}{l}",
               "{\\footnotesize{\\textit{Note:} The standard deviations for proportions is 
               not presented.}} \n")


addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 22, 22)
addtorow$command <- c(
  " \\\\[-1.8ex]",
  " & & \\multicolumn{3}{c}{Non AA Sample} & & \\multicolumn{3}{c}{AA Sample}  \\\\[0.2ex]",
  "\\cline{3-5}  \\cline{7-9}  \\\\[-1.2ex]" ,
  " & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} \\\\",
  " \\\\[-1.8ex] &  & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & & 
  \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)} \\\\ ",
  # " \\\\[-1.8ex] \\hline \\\\[-1.8ex] ",
  # obs_descr,
  " \\bottomrule ",
  comm
)


print(
  xtab, 
  add.to.row = addtorow,
  include.rownames = FALSE, include.colnames = FALSE, 
  booktabs = TRUE, caption.placement = "top", 
  hline.after = c(-1, 0),
  file = "output/tables/tableI.tex" 
)


# Proportion of students who were schooled in their mother tongue
# during primary school by region

# correlation between IMTI/E_is and some outcome variables (eg. wage 
# employment) by region
  # E.g., modal value of IMTI/E_is vs. proportion of wage employed
# 


