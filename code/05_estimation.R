# =============================================================================
# Young Lives Ethiopia: employment and cognitive-outcome regressions
# Project: Mother Tongue Instruction, Learning and Employment
# Authors: Teferi Mergo and Lemi Daba
#
# Inputs: older-cohort RDS and younger-cohort CSV analysis samples.
# Outputs: fitted models consumed by 06_export_res.R in the SAME R session.
# Run from the repository root after helpers/_setup.R, or via _master.R.
#
# Preserve the original formulas, control order, score standardization, filters
# and model names used by the table exporter. Most models use conventional
# inference; the updated Table 2 regional checks are in 07_table_main.R.
# Model families below consolidate repeated fits without changing specifications.
# =============================================================================

rm(list = ls())
source("code/helpers/analysis_helpers.R")

aa_sample <- prepare_employment_outcomes(readRDS("data/aa_samp.rds"))
non_aa_sample <- prepare_employment_outcomes(
  readRDS("data/non_aa_samp.rds")
)

# ---- Formula builders: preserve distinct reduced-form and IV controls -------
reduced_form_controls <- c(
  "zbfa", "caredu_r1", "careage_r1", "factor(caresex_r1)",
  "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new",
  "chsex", "factor(chldrel)", "preprim", "agegr1", "factor(foodsec_r3)"
)
iv_controls <- c(
  "chsex", "zbfa", "stunting", "caredu_r1", "careage_r1",
  "factor(caresex_r1)", "hhsize", "wi_new", "hq_new", "cd_new",
  "elecq_new", "ownlandhse_r1", "factor(foodsec_r3)"
)

make_reduced_form <- function(outcome, predictor, additional = character(),
                              cluster_by_region = FALSE) {
  formula_text <- paste(outcome, "~", paste(
    c(predictor, additional, reduced_form_controls), collapse = " + "
  ))
  if (cluster_by_region) formula_text <- paste(formula_text, "| 0 | 0 | region")
  as.formula(formula_text)
}

make_iv_formula <- function(outcome, additional = character()) {
  as.formula(paste(outcome, "~", paste(c(additional, iv_controls), collapse = " + "),
                   "| 0 | (IMTI ~ E_is)"))
}

# Numeric model IDs stay stable for the existing table exporter.
# The Addis restriction still refers to the FIRST activity slot.
employment_cognitive_outcomes <- c(
  "wage_employ", "self_employ", "raw_maths", "raw_lang",
  "hghgrade_final_num", "salary_employ", "self_nonag", "std_maths", "std_lang"
)
reduced_form_outcomes <- c(
  "wage_employ", "raw_maths", "raw_lang", "hghgrade_final_num",
  "salary_employ", "std_maths", "std_lang"
)

# ---- Initial first stage: the existing region-clustered exploratory fit -----
m1 <- felm(make_reduced_form("IMTI", "E_is", "factor(typesite_r3)",
                             cluster_by_region = TRUE), data = non_aa_sample)
stargazer(m1, keep = c("E_is", "region", "chsex"), type = "text", keep.stat = c("n", "rsq"))
waldtest(m1, ~ E_is)[["F"]]

# Standardize separately in Addis/non-Addis, before outcome-specific deletion
# and before removing Amhara. This is not within-region standardization.
aa_sample <- aa_sample %>%
  mutate(std_maths = standardize_score(raw_maths), std_lang = standardize_score(raw_lang))
non_aa_sample <- non_aa_sample %>%
  mutate(std_maths = standardize_score(raw_maths), std_lang = standardize_score(raw_lang))

# ---- Addis Ababa reduced forms ----------------------------------------------
aa_reduced_form_models <- lapply(reduced_form_outcomes, function(outcome) {
  lm(make_reduced_form(outcome, "E_is", "entype_r4"),
     data = aa_sample, subset = (type_activ_1 != 19))
})
names(aa_reduced_form_models) <- paste0("rf", seq_along(reduced_form_outcomes), "aa")
list2env(aa_reduced_form_models, envir = environment())
stargazer(rf6aa, rf7aa, rf5aa, rf1aa, keep = "E_is", type = "text", keep.stat = c("n", "rsq"))

# ---- Older-cohort 2SLS: non-Addis, excluding Amhara, and Addis ----------------
non_aa_iv_models <- lapply(employment_cognitive_outcomes, function(outcome) {
  felm(make_iv_formula(outcome), data = non_aa_sample)
})
names(non_aa_iv_models) <- paste0("iv", seq_along(employment_cognitive_outcomes))
list2env(non_aa_iv_models, envir = environment())
stargazer(iv8, iv9, iv6, iv1, keep = "IMTI", type = "text", keep.stat = c("n", "rsq"))

non_amhara_sample <- non_aa_sample %>% filter(region != "Amhara")
non_amhara_iv_models <- lapply(employment_cognitive_outcomes, function(outcome) {
  felm(make_iv_formula(outcome), data = non_amhara_sample)
})
names(non_amhara_iv_models) <- paste0("iv", seq_along(employment_cognitive_outcomes), "_nonAm")
list2env(non_amhara_iv_models, envir = environment())

# The self-employment fit iv2aa was historically omitted for rank problems.
aa_iv_indices <- c(1, 3:9)
aa_iv_models <- lapply(employment_cognitive_outcomes[aa_iv_indices], function(outcome) {
  felm(make_iv_formula(outcome, "entype_r4"), data = aa_sample, subset = (type_activ_1 != 19))
})
names(aa_iv_models) <- paste0("iv", aa_iv_indices, "aa")
list2env(aa_iv_models, envir = environment())
stargazer(iv8aa, iv9aa, iv6aa, iv1aa, keep = "IMTI", type = "text", keep.stat = c("n", "rsq"))

# ---- Tigray/Oromia comparisons: OLS with IMTI as a control -------------------
tigray_oromia_sample <- non_aa_sample %>%
  filter(region %in% c("Oromia", "Tigray")) %>%
  mutate(
    Tigray = (region == "Tigray"),
    language_match = case_when(
      region == "Tigray" & testlang_lang == "Tigrigna" & chlang == "Tigrigna" ~ 1,
      region == "Oromia" & testlang_lang == "Afaan Oromo" & chlang == "Afaan Oromo" ~ 1,
      TRUE ~ 0
    )
  )
tigray_oromia_models <- lapply(seq_along(reduced_form_outcomes), function(i) {
  formula <- make_reduced_form(reduced_form_outcomes[[i]], "Tigray",
                               c("IMTI", "ownlandhse_r1", "rural_3"))
  # Raw and standardized language models require test/home language agreement.
  if (i %in% c(3, 7)) {
    lm(formula, data = tigray_oromia_sample, subset = (language_match == 1))
  } else {
    lm(formula, data = tigray_oromia_sample)
  }
})
names(tigray_oromia_models) <- paste0("rf", seq_along(reduced_form_outcomes), "ot")
list2env(tigray_oromia_models, envir = environment())
stargazer(rf6ot, rf7ot, rf5ot, rf1ot, keep = "T", type = "text", keep.stat = c("n", "rsq"))

# ---- Younger-cohort Addis samples and models --------------------------------
school_linked_sample <- read_csv("data/schsur_yl_joined.csv", show_col_types = FALSE)
younger_cohort_sample <- read_csv("data/ylsample_yc.csv", show_col_types = FALSE)

# Unlike the older-cohort loader, these samples retain zero language scores
# and their original CSV column types; changing either would alter the models.
school_linked_aa <- school_linked_sample %>%
  filter(region == "Addis Ababa") %>%
  mutate(std_math = standardize_score(math_score),
         std_verbal = standardize_score(verbal_score),
         std_math_schsurv = standardize_score(maths_score_w2),
         std_literacy_schsurv = standardize_score(literacy_score_w2))
younger_cohort_aa <- younger_cohort_sample %>%
  filter(region == "Addis Ababa") %>%
  mutate(std_math = standardize_score(math_score), std_verbal = standardize_score(verbal_score))
younger_cohort_aa_trimmed <- younger_cohort_aa %>% filter(std_math < 2.25)

# Caregiver sex is absent from the original younger-cohort control set.
younger_controls <- c("entype_r4", setdiff(iv_controls, "factor(caresex_r1)"))
make_younger_formula <- function(outcome, iv = TRUE) {
  predictors <- c(if (!iv) "E_is", younger_controls)
  formula_text <- paste(outcome, "~", paste(predictors, collapse = " + "))
  if (iv) formula_text <- paste(formula_text, "| 0 | (IMTI ~ E_is)")
  as.formula(formula_text)
}

# Keep raw-outcome IV and standardized-outcome reduced-form definitions.
iv3aa_yc <- felm(make_younger_formula("maths_score_w2"), data = school_linked_aa)
iv4aa_yc <- felm(make_younger_formula("literacy_score_w2"), data = school_linked_aa)
iv5aa_yc <- felm(make_younger_formula("math_score"), data = younger_cohort_aa_trimmed)
iv6aa_yc <- felm(make_younger_formula("verbal_score"), data = younger_cohort_aa_trimmed)
iv5aa_yc_rf <- felm(make_younger_formula("std_math", iv = FALSE), data = younger_cohort_aa_trimmed)
iv6aa_yc_rf <- felm(make_younger_formula("std_verbal", iv = FALSE), data = younger_cohort_aa_trimmed)
iv3aa_yc_rf <- felm(make_younger_formula("std_math_schsurv", iv = FALSE), data = school_linked_aa)
iv4aa_yc_rf <- felm(make_younger_formula("std_literacy_schsurv", iv = FALSE), data = school_linked_aa)

stargazer(iv3aa_yc, iv4aa_yc, iv5aa_yc, iv6aa_yc,
          keep = "IMTI", type = "text", keep.stat = c("n", "rsq"))
stargazer(iv3aa_yc_rf, iv4aa_yc_rf, iv5aa_yc_rf, iv6aa_yc_rf,
          keep = "E_is", type = "text", keep.stat = c("n", "rsq"))
