# =============================================================================
# Young Lives Ethiopia: older-cohort analysis data
# Project: Mother Tongue Instruction, Learning and Employment
# Authors: Teferi Mergo and Lemi Daba
#
# Purpose: combine the R1--R5 constructed panel with R3/R4 cognitive inputs
# and R5 activity records; construct employment, MTI exposure and instruments.
# Outputs: data/aa_samp.csv and non_aa_samp.csv
# Run from the repository root after code/helpers/_setup.R (or via _master.R).
# Replication uses the legacy constructed release. New R1--R7 fields have changed
# definitions and require a separate sensitivity comparison before substitution.
# Release comparison audit: temp/cleaning_refactor_audit/README.txt.
# Sample filters, missing-value handling and historical treatment rules are
# intentionally preserved; this cleanup changes presentation and intermediate names.
# =============================================================================

# ---- Inputs and shared labels -----------------------------------------------
source("code/helpers/raw_data_catalog.R")
source("code/helpers/extraction_helpers.R")

constructed_data <- read_dta(raw_data_files[["constructed_legacy"]])

round3_child_data <- read_dta(raw_data_files[["r3_oc"]])

round4_cognitive_data <- read_dta(raw_data_files[["r4_oc_cognitive"]])

round5_activity_data <- read_dta(raw_data_files[["r5_oc_activity"]])

# ---- Constructed panel and cognitive tests ----------------------------------
cohort_data <- constructed_data %>%
  filter(yc == 0, is.na(deceased)) %>%
  mutate(child_id = str_remove(childid, "ET") %>% as.numeric()) %>%
  select(child_id, everything(), -c(deceased, chsmoke, childid)) %>%
  mutate(region = factor(label_survey_codes(region, survey_code_labels$region)))

round4_test_scores <- round4_cognitive_data %>%
  select(
    child_id = CHILDCODE, lang_instr = RDITLANR4, testlang_lang = RDGCMPLANR4,
    raw_lang = lang_raw, perco_lang = lang_perco, testlang_math = MTHLANR4,
    raw_maths = maths_raw, perco_maths = maths_perco
  ) %>%
  mutate(
    across(c(lang_instr, testlang_lang, testlang_math),
           ~ label_survey_codes(.x, survey_code_labels$language_r4))
  )

round3_test_scores <- round3_child_data %>%
  select(
    child_id = CHILDID, lang_math_test = MTHLNGTS, verbal_score = ppvt,
    math_score = math
  ) %>%
  mutate(
    child_id = str_remove(child_id, "ET") %>% as.numeric(),
    lang_math_test = label_survey_codes(lang_math_test, survey_code_labels$language_r3)
  )

# Characteristics are taken from R5, matching the original sample anchor.
child_characteristics <- cohort_data %>%
  filter(round == 5) %>%
  select(
    child_id, chsex, chethnic, chlang, chldrel, preprim, agegr1
  ) %>%
  mutate(
    chlang = factor(label_survey_codes(chlang, survey_code_labels$home_language)),
    chethnic = factor(label_survey_codes(chethnic, survey_code_labels$ethnicity))
  )

# Retain each reported R5 activity in columns suffixed by its activity ID.
main_activity <- round5_activity_data %>%
  select(
    child_id = CHILDCODE, activ_id = ACTIDR5, type_activ = ACTR5, work_for = ACTOWNR5,
    payment_form = PYMRECR5, pay_cash_amt = ERNCSHR5, pay_kind_amt = ERNKNDR5,
    pay_period = HWPAIDR5, still_work = PRFACTR5
  ) |> 
  pivot_wider(
    id_cols = child_id,
    names_from = activ_id,
    values_from = -c(child_id, activ_id),
    names_glue = "{.value}_{activ_id}"
  )

# ---- Round-specific covariates (retain names used by the regressions) --------
round5_covariates <- cohort_data %>%
  filter(round == 5) %>%
  select(
    child_id, hghgrade_r5 = hghgrade, region_r5 = region, hwork_r5 = hwork
  )

round4_covariates <- cohort_data %>%
  filter(round == 4) %>%
  select(
    child_id, region_r4 = region, hghgrade_r4 = hghgrade, entype_r4 = entype,
    timesch_r4 = timesch, childloc_r4 = childloc, psnp_pw_r4 = psnp_pw,
    psnp_ds_r4 = psnp_ds, othprog_r4 = othprog, hep_r4 = hep, resettled_r4 = resettled,
    eap_r4 = eap, foodsec_r4 = foodsec, credit_r4 = credit
  )

round3_covariates <- cohort_data %>%
  filter(round == 3) %>%
  select(
    child_id, region_r3 = region, hghgrade_r3 = hghgrade, typesite_r3 = typesite,
    hhsize, wi_new, hq_new, sv_new, cd_new, drwaterq_new, toiletq_new, elecq_new,
    cookingq_new, aniany, ownhouse, childloc_r3 = childloc, psnp_pw_r3 = psnp_pw,
    psnp_ds_r3 = psnp_ds, othprog_r3 = othprog, hep_r3 = hep, resettled_r3 = resettled,
    eap_r3 = eap, foodsec_r3 = foodsec, credit_r3 = credit
  )

round2_covariates <- cohort_data %>%
  filter(round == 2) %>%
  select(
    child_id, region_r2 = region, hghgrade_r2 = hghgrade, literate_r2 = literate,
    levlwrit_r2 = levlwrit, levlread_r2 = levlread, childloc_r2 = childloc,
    psnp_pw_r2 = psnp_pw, psnp_ds_r2 = psnp_ds, othprog_r2 = othprog, hep_r2 = hep,
    resettled_r2 = resettled, eap_r2 = eap, foodsec_r2 = foodsec, credit_r2 = credit
  )

round1_covariates <- cohort_data %>%
  filter(round == 1) %>%
  select(
    child_id, region_r1 = region, typesite_r1 = typesite, hghgrade_r1 = hghgrade, zwfa,
    zhfa, zbfa, underweight, stunting, thinness, caredu_r1 = caredu,
    careage_r1 = careage, caresex_r1 = caresex, carerel_r1 = carerel,
    carecantread_r1 = carecantread, ownlandhse_r1 = ownlandhse, numante,
    childloc_r1 = childloc, psnp_pw_r1 = psnp_pw, psnp_ds_r1 = psnp_ds,
    othprog_r1 = othprog, hep_r1 = hep, resettled_r1 = resettled, eap_r1 = eap,
    foodsec_r1 = foodsec, credit_r1 = credit
  )

# ---- Primary-school language and child-level joins --------------------------
# Use R4 instruction language in primary school; otherwise proxy with the R3 test.
primary_language_data <- round4_covariates %>%
  select(child_id, hghgrade_r4) %>%
  left_join(round3_test_scores, by = "child_id") %>%
  left_join(round4_test_scores, by = "child_id") %>%
  mutate(
    lang_primary =
      case_when(
        (hghgrade_r4 <= 8 & lang_instr != "English") ~ lang_instr,
        TRUE ~ lang_math_test
      ) %>% factor()
  ) %>% select(-hghgrade_r4)

# Preserve the left-join sequence and resulting row/column order.
analysis_data_0 <- child_characteristics %>%
  left_join(primary_language_data, by = "child_id") %>%
  left_join(round1_covariates, by = "child_id") %>%
  left_join(round2_covariates, by = "child_id") %>%
  left_join(round3_covariates, by = "child_id") %>%
  left_join(round4_covariates, by = "child_id") %>%
  left_join(round5_covariates, by = "child_id") %>%
  left_join(main_activity, by = "child_id")

analysis_data_1 <- analysis_data_0 %>%
  mutate(
    region = coalesce(region_r3, region_r2, region_r1),

    # Retain the legacy fallback order, including its missing-R3 branch.
    hghgrade_4 = case_when(
      !is.na(hghgrade_r4) ~ hghgrade_r4,
      is.na(hghgrade_r4) & is.na(hghgrade_r3) ~ hghgrade_r3,
      is.na(hghgrade_r4) & is.na(hghgrade_r3) &
        !is.na(hghgrade_r2) ~ hghgrade_r2,
      is.na(hghgrade_r4) & is.na(hghgrade_r3) &
        is.na(hghgrade_r2) ~ hghgrade_r1
    ),
    hghgrade_final = coalesce(hghgrade_r5, hghgrade_r4),
    hghgrade_4_num = case_when(
      hghgrade_r4 <= 14 ~ as.numeric(hghgrade_r4),
      hghgrade_r4 %in% c(29, 30) ~ 0
    ),
    hghgrade_final_num = case_when(
      hghgrade_final <= 14 ~ as.numeric(hghgrade_final),
      hghgrade_final %in% c(29, 30) ~ 0
    ),
    care_educ_num = case_when(
      caredu_r1 <= 14 ~ as.numeric(caredu_r1),
      caredu_r1 %in% c(28, 29) ~ 0
    )
  )

# type_activ                                                                              n
#    <dbl+lbl>                                                                           <int>
#  1  1 [Self Employed (Food crops)]                                                       162
#  2  2 [Self Employed (Non-food, including horticulture, sericulture and floriculture)]    10
#  3  3 [Self Employed (Aquaculture]                                                         7
#  4  4 [Self Employed.(Livestock)]                                                         21
#  5  5 [Wage Employment (Agriculture)]                                                     13
#  6  6 [Annual Farm Servant]                                                                3
#  7  7 [Other (allied) agriculture]                                                         6
#  8  8 [Self Employed (Manufacturing)]                                                      1
#  9  9 [Self Employed (Services)]                                                          31
# 10 10 [Self Employed (Business)]                                                         100
# 11 11 [Self Employed (Other non-Agriculture)]                                             29
# 12 12 [Wage Employment (Unsalaried;Non-agriculture)]                                      90
# 13 13 [Regular Salaried Employment]                                                      123
# 14 19 [House maid/Yebet Serategna]                                                        22
# 15 20 [Other non- agriculture specify]                                                    22
# 16 NA                                                                                    349

analysis_data_2 <- analysis_data_1 %>%
  mutate(
    active = as.integer(if_any(type_activ_1:type_activ_4, ~ !is.na(.x))),
    self_agri = case_when(
      if_any(type_activ_1:type_activ_4, ~ .x %in% c(1, 2, 3, 4)) ~ 1,
      TRUE ~ 0
    ),

    wage_agri = case_when(
      if_any(type_activ_1:type_activ_4, ~ .x %in% c(5, 6, 7)) ~ 1,
      TRUE ~ 0
    ),

    self_nonag = case_when(
      if_any(type_activ_1:type_activ_4, ~ .x %in% c(8, 9, 10, 11)) ~ 1,
      TRUE ~ 0
    ),

    wage_employ = case_when(
      if_any(type_activ_1:type_activ_4, ~ .x %in% c(12, 19)) ~ 1,
      TRUE ~ 0
    ),

    salary_employ = case_when(
      if_any(type_activ_1:type_activ_4, ~ .x %in% c(13)) ~ 1,
      TRUE ~ 0
    ),
    
    chsex = factor(label_survey_codes(chsex, survey_code_labels$sex)),
    rural_3 = case_when(
      typesite_r3 == 1 ~ 0, typesite_r3 == 2 ~ 1
    )
  )

analysis_data_3 <- analysis_data_2 %>%
  mutate(
    # MTI exposure caps and instrument rules are unchanged.
    IMTI = case_when(
      region == "Tigray" & chlang == "Tigrigna" & lang_primary == "Tigrigna"
      ~ top_code(hghgrade_final_num, 8),
      region == "Oromia" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo"
      ~ top_code(hghgrade_final_num, 8),

      # Historical spelling retained to preserve treatment coding.
      region == "Oromia" & chlang == "Amaharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_final_num, 8),
      region == "Amhara" & chlang == "Amharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_final_num, 6),
      region == "Amhara" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo"
      ~ top_code(hghgrade_final_num, 8),
      region == "SNNP" & chlang == "Sidamigna" & lang_primary == "Sidamigna"
      ~ top_code(hghgrade_final_num, 4),
      region == "SNNP" & chlang == "Wolayta" & lang_primary == "Wolayta"
      ~ top_code(hghgrade_final_num, 4),
      region == "SNNP" & chlang == "Hadiya" & lang_primary == "Hadiya"
      ~ top_code(hghgrade_final_num, 4),
      region == "SNNP" & chlang == "Amharic" & lang_primary == "Amharic"
      ~ top_code(hghgrade_final_num, 4),
      region == "Addis Ababa" & chethnic == "Amhara" & lang_primary == "Amharic"
      ~ top_code(hghgrade_final_num, 6),
      TRUE ~ 0
    ),
    E_is = case_when(
      region == "Oromia" & chethnic == "Oromo" ~ 1,
      region == "Tigray" & chethnic == "Tigrian" ~ 1,
      region == "Oromia" & chethnic == "Amhara" & lang_primary == "Amharic" ~ 1,
      region == "Amhara" & chethnic == "Oromo" & lang_primary == "Afaan Oromo" ~ 1,
      region == "Amhara" & chethnic == "Amhara" ~ 0.75,
      region == "SNNP" & chethnic == "Hadiya" & lang_primary == "Hadiya" ~ 0.5,
      region == "SNNP" & chethnic == "Sidama" & lang_primary == "Sidamigna" ~ 0.5,
      region == "SNNP" & chethnic == "Wolayta" & lang_primary == "Wolayta" ~ 0.5,
      region == "SNNP" & chethnic == "Amhara" & lang_primary == "Amharic" ~ 0.5,
      region == "Addis Ababa" & chethnic == "Amhara" ~ 0.75,
      TRUE ~ 0
    )
  )

analysis_data_final <- analysis_data_3 %>%
  select(-starts_with(c("region_r", "hghgrade_r")))

# ---- Export Addis and non-Addis analysis samples -----------------------------
aa_samp <- analysis_data_final %>%
  filter(region == "Addis Ababa") %>%
  as_tibble()

non_aa_samp <- analysis_data_final %>%
  filter(region != "Addis Ababa") %>%
  as_tibble()

# RDS preserves column attributes, including existing variable/value labels.
saveRDS(aa_samp, "data/aa_samp.rds")
saveRDS(non_aa_samp, "data/non_aa_samp.rds")

# Retain CSV exports for the existing downstream analysis scripts.
write_csv(aa_samp, "data/aa_samp.csv")
write_csv(non_aa_samp, "data/non_aa_samp.csv")
