# =============================================================================
# Young Lives Ethiopia: younger-cohort analysis data
# Project: Mother Tongue Instruction, Learning and Employment
# Authors: Teferi Mergo and Lemi Daba
#
# Purpose: combine the R1--R5 constructed panel with R3/R4 cognitive inputs
# and link the 2012--13 school survey; construct MTI exposure and instruments.
# Outputs: data/ylsample_yc.csv and schsur_yl_joined.csv
# Run from the repository root after code/helpers/_setup.R (or via _master.R).
# Replication uses the legacy constructed release. New R1--R7 fields have changed
# definitions and require a separate sensitivity comparison before substitution.
# Release comparison audit: output/cleaning_refactor_audit/README.txt.
# Sample filters, missing-value handling and historical treatment rules are
# intentionally preserved; this cleanup changes presentation and intermediate names.
# =============================================================================

# ---- Inputs and shared labels -----------------------------------------------
source("code/helpers/raw_data_catalog.R")
source("code/helpers/extraction_helpers.R")

school_pupils_w1 <- read_dta(raw_data_files[["school_pupils_w1"]])
school_math_w2 <- read_dta(raw_data_files[["school_math_w2"]])
school_literacy_w2 <- read_dta(raw_data_files[["school_literacy_w2"]])

constructed_data <- read_dta(raw_data_files[["constructed_legacy"]])
round3_child_data <- read_dta(raw_data_files[["r3_yc"]])
round4_cognitive_data <- read_dta(raw_data_files[["r4_yc_cognitive"]])

# ---- Constructed panel and cognitive tests ----------------------------------
cohort_data <- constructed_data %>%
  filter(yc == 1, is.na(deceased)) %>%
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
analysis_data <- child_characteristics %>%
  left_join(primary_language_data, by = "child_id") %>%
  left_join(round1_covariates, by = "child_id") %>%
  left_join(round2_covariates, by = "child_id") %>%
  left_join(round3_covariates, by = "child_id") %>%
  left_join(round4_covariates, by = "child_id") %>%
  left_join(round5_covariates, by = "child_id")

analysis_data <- analysis_data %>%
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

analysis_data <- analysis_data %>%
  mutate(
    chsex = factor(label_survey_codes(chsex, survey_code_labels$sex)),
    rural_3 = case_when(
      typesite_r3 == 1 ~ 0, typesite_r3 == 2 ~ 1
    )
  )

analysis_data <- analysis_data %>%
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

analysis_data <- analysis_data %>%
  select(-starts_with(c("region_r", "hghgrade_r")))

# Export the cohort before the separate school-survey match.
write_csv(analysis_data, "data/ylsample_yc.csv")

# ---- Link the 2012--13 school survey ----------------------------------------
# Correct answers score one; other responses are ignored when summing (all missing = 0).
score_school_test <- function(tbl, var_start, var_end) {
  tbl %>%
    select(schlid:childid, {{ var_start }}:{{ var_end }}) %>%
    pivot_longer(cols = {{ var_start }}:{{ var_end }},
                 names_to = "question_no",
                 values_to = "result") %>%
    mutate(result = case_when(
      result %in% c(0, 2) ~ 0,
      result == 1 ~ 1,
      TRUE ~ NaN
    )) %>%
    group_by(schlid, classid, pupilid, ylchild, childid) %>%
    summarize(score = sum(result, na.rm = TRUE), .groups = "drop")
}

math_scores_w1 <- score_school_test(school_pupils_w1, cmcorr01, cmcorr25) %>%
  rename(maths_score_w1 = score)

literacy_scores_w1 <- score_school_test(school_pupils_w1, cvcorr01, cvcorr25) %>%
  rename(literacy_score_w1 = score)

math_scores_w2 <- score_school_test(school_math_w2, cmcorrrt01, cmcorrrt25) %>%
  rename(maths_score_w2 = score)

literacy_scores_w2 <- score_school_test(school_literacy_w2, cvcorrrt01, cvcorrrt25) %>%
  rename(literacy_score_w2 = score)

# Match pupils across tests using all five survey identifiers.
school_test_scores <- math_scores_w1 %>%
  inner_join(literacy_scores_w1, by = c("schlid", "classid", "pupilid", "ylchild", "childid")) %>%
  inner_join(math_scores_w2, by = c("schlid", "classid", "pupilid", "ylchild", "childid")) %>%
  inner_join(literacy_scores_w2, by = c("schlid", "classid", "pupilid", "ylchild", "childid"))

# Link Young Lives pupils only; retain the existing complete-test intersection.
school_analysis_data <- school_test_scores %>%
  filter(ylchild == 1) %>%
  mutate(childid = str_remove(childid, "ET") %>% as.numeric()) %>%
  inner_join(analysis_data, by = c("childid" = "child_id"),
             suffix = c("_sch", "_yl")) %>%
  select(-c(schlid, classid, pupilid, ylchild))

write_csv(school_analysis_data, "data/schsur_yl_joined.csv")
