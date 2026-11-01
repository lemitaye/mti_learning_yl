
# Date created: March 25, 2022
# 
# Description: This script loads the necessary data from several rounds of the
#   Young Lives Data sets, selects relevant variables, joins separate files, 
#   cleans, merges, constructs new variables (both outcomes and covariates) and
#   produces file(s) ready for analyses.

# Note: load the necessary packages in "_master.R" before running this script


# Load data: ####
source("code/helpers/raw_data_catalog.R")

cons_all <- read_dta(raw_data_files[["constructed_legacy"]])

child_r3 <- read_dta(raw_data_files[["r3_oc"]])

cognitive_r4 <- read_dta(raw_data_files[["r4_oc_cognitive"]])

occh_activity <- read_dta(raw_data_files[["r5_oc_activity"]])

# clean data: ####

cons_oc <- cons_all %>% 
  filter(yc == 0) %>%  # filter older cohort 
  filter(is.na(deceased)) %>% # filter non-deceased
  mutate(child_id = str_remove(childid, "ET") %>% as.numeric()) %>% 
  select(
    child_id, everything(),
    -c(deceased, chsmoke, childid)
  ) %>% 
  mutate(
    region = case_when(
      region ==  1 ~ "Tigray",
      region ==  2 ~ "Afar",
      region ==  3 ~ "Amhara",
      region ==  4 ~ "Oromia",
      region ==  5 ~ "Somali",
      region ==  7 ~ "SNNP",
      region ==  14 ~ "Addis Ababa"
    ) %>% factor()
  )

test_r4 <- cognitive_r4 %>% 
  select(
    child_id = CHILDCODE,
    lang_instr = RDITLANR4,
    testlang_lang = RDGCMPLANR4,
    raw_lang = lang_raw,
    perco_lang = lang_perco,
    testlang_math = MTHLANR4,
    raw_maths = maths_raw,
    perco_maths = maths_perco
  ) %>% 
  mutate(
    lang_instr = case_when(
      lang_instr == 1 ~ "English",
      lang_instr == 2 ~ "Amharic",
      lang_instr == 3 ~ "Hadiya",
      lang_instr == 4 ~ "Afaan Oromo",
      lang_instr == 5 ~ "Wolayta",
      lang_instr == 6 ~ "Sidamigna",
      lang_instr == 7 ~ "Tigrigna"
    ),
    testlang_lang = case_when(
      testlang_lang == 1 ~ "English",
      testlang_lang == 2 ~ "Amharic",
      testlang_lang == 3 ~ "Hadiya",
      testlang_lang == 4 ~ "Afaan Oromo",
      testlang_lang == 5 ~ "Wolayta",
      testlang_lang == 6 ~ "Sidamigna",
      testlang_lang == 7 ~ "Tigrigna"
    ),
    testlang_math = case_when(
      testlang_math == 1 ~ "English",
      testlang_math == 2 ~ "Amharic",
      testlang_math == 3 ~ "Hadiya",
      testlang_math == 4 ~ "Afaan Oromo",
      testlang_math == 5 ~ "Wolayta",
      testlang_math == 6 ~ "Sidamigna",
      testlang_math == 7 ~ "Tigrigna"
    )
  )

test_r3 <- child_r3 %>% 
  select(
    child_id = CHILDID, # id
    lang_math_test = MTHLNGTS,
    verbal_score = ppvt,
    math_score = math,
  ) %>% 
  mutate( 
    child_id = str_remove(child_id, "ET") %>% as.numeric(),
    
    lang_math_test = case_when(
      lang_math_test == 2 ~ "Amharic",
      lang_math_test == 7 ~ "Hadiya",
      lang_math_test == 11 ~ "Afaan Oromo",
      lang_math_test == 12 ~ "Sidamigna",
      lang_math_test == 15 ~ "Tigrigna",
      lang_math_test == 16 ~ "Wolayta",
      lang_math_test %in% c(88, NA) ~ NA_character_,
    )
    )

# Extract time invariant characteristics from "constructed" file: 
time_invar <- cons_oc %>% 
  filter(round == 5) %>% 
  select(
    child_id, # id
    chsex, # sex
    chethnic, # ethnicity
    chlang, # first language
    chldrel, # religion
    preprim, # attended pre-primary school
    agegr1 # age at start of grade 1
    ) %>% 
  mutate(
    chlang = case_when(
      chlang == 1 ~  "Afar",
      chlang == 2 ~  "Amharic",
      chlang == 6 ~  "Gurage",
      chlang == 7 ~  "Hadiya",
      chlang == 11 ~ "Afaan Oromo",
      chlang == 12 ~ "Sidamigna",
      chlang == 13 ~ "Siltigna",
      chlang == 15 ~ "Tigrigna",
      chlang == 16 ~ "Wolayta",
      chlang == 20 ~ "Other",
      chlang == NA ~ NA_character_
    ) %>% factor(),
    
    chethnic = case_when(
      chethnic == 10 ~ "Other",
      chethnic == 12 ~ "Amhara",
      chethnic == 13 ~ "Gurage",
      chethnic == 14 ~ "Hadiya",
      chethnic == 16 ~ "Oromo",
      chethnic == 17 ~ "Sidama",
      chethnic == 18 ~ "Tigrian",
      chethnic == 19 ~ "Wolayta"
    ) %>% factor()
  )


activity_r5 <- occh_activity %>% 
  select(
    child_id = CHILDCODE, # id
    no_activ = ACTIDR5,   # No. of Paid Activities
    type_activ = ACTR5     # Type of activity_r5
  ) %>% 
  # Filter the main activity (child-activity combination)
  filter(no_activ ==  1) 


# Variables from round 5

vars_r5 <- cons_oc %>% 
  filter(round == 5) %>% 
  select(
    child_id, # id
    hghgrade_r5 = hghgrade, # Highest grade achieved at time of interview
    region_r5 = region, # region of residence
    hwork_r5 = hwork  # Hours/day spent in paid activity
  ) 

# Variables from round 4

vars_r4 <- cons_oc %>% 
  filter(round == 4) %>% 
  select(
    child_id, # id
    region_r4 = region, # region of residence
    hghgrade_r4 = hghgrade, # Highest grade achieved at time of interview
    entype_r4 = entype,
    timesch_r4 = timesch,
    childloc_r4 = childloc,
    psnp_pw_r4 = psnp_pw,
    psnp_ds_r4 = psnp_ds,
    othprog_r4 = othprog,
    hep_r4 = hep,
    resettled_r4 = resettled,
    eap_r4 = eap,
    foodsec_r4 = foodsec,
    credit_r4 = credit,
    
  ) 


# Variables from round 3

vars_r3 <- cons_oc %>% 
  filter(round == 3) %>% 
  select(
    # Here we collect a lot of covariates:
    child_id, # id
    region_r3 = region, # region of residence
    hghgrade_r3 = hghgrade, # Highest grade achieved at time of interview
    typesite_r3 = typesite, # Area of residence (urban/rural)
    # dadedu_r3 = dadedu, # Father's level of education
    # dadcantread_r3 = dadcantread, # Father cannot read
    # momedu_r3 = momedu, # Mother's level of education
    # momcantread_r3 = momcantread, # Mother cannot read
    # caredu_r3 = caredu, # Caregiver's level of education
    # careage_r3 = careage, # Caregiver's age
    # caresex_r3 = caresex, # Caregiver's sex
    # carerel_r3 = carerel, # Caregiver's relationship to YL child
    # carecantread_r3 = carecantread, # Caregiver cannot read
    hhsize, # household size
    wi_new, # Wealth index
    hq_new, # Housing quality index
    sv_new, # Access to services index
    cd_new, # Consumer durables index
    drwaterq_new, # Access to safe drinking water
    toiletq_new, # Access to sanitation
    elecq_new, # Access to electricity
    cookingq_new, # Access to adequate fuels for cooking
    aniany, # Household owned any livestock in the past 12 months
    # ownlandhse_r3 = ownlandhse, #  Household owns land where house is on
    ownhouse, # Household owns the house
    childloc_r3 = childloc,
    psnp_pw_r3 = psnp_pw,
    psnp_ds_r3 = psnp_ds,
    othprog_r3 = othprog,
    hep_r3 = hep,
    resettled_r3 = resettled,
    eap_r3 = eap,
    foodsec_r3 = foodsec,
    credit_r3 = credit
  ) 


# Variables from round 2

vars_r2 <- cons_oc %>% 
  filter(round == 2) %>% 
  select(
    child_id, # id
    region_r2 = region, # region of residence
    # typesite_r2 = typesite, # Area of residence (urban/rural)
    hghgrade_r2 = hghgrade, # Highest grade achieved at time of interview
    # dadedu_r2 = dadedu, # Father's level of education
    # dadcantread_r2 = dadcantread, # Father cannot read
    # dadyrdied_r2 = dadyrdied, # Year when father has died
    # momedu_r2 = momedu, # Mother's level of education
    # momcantread_r2 = momcantread, # Mother cannot read
    # momyrdied_r2 = momyrdied, # Year when mother has died
    # caredu_r2 = caredu, # Caregiver's level of education
    # careage_r2 = careage, # Caregiver's age
    # caresex_r2 = caresex, # Caregiver's sex
    # carerel_r2 = carerel, # Caregiver's relationship to YL child
    # carecantread_r2 = carecantread, # Caregiver cannot read
    # ownlandhse_r2 = ownlandhse #  Household owns land where house is on
    # bcg,
    # measles,
    literate_r2 = literate,
    levlwrit_r2 = levlwrit,
    levlread_r2 = levlread,
    childloc_r2 = childloc,
    psnp_pw_r2 = psnp_pw,
    psnp_ds_r2 = psnp_ds,
    othprog_r2 = othprog,
    hep_r2 = hep,
    resettled_r2 = resettled,
    eap_r2 = eap,
    foodsec_r2 = foodsec,
    credit_r2 = credit
  ) 

# Variables from round 1

vars_r1 <- cons_oc %>% 
  filter(round == 1) %>% 
  select(
    child_id, # id
    region_r1 = region, # region of residence
    typesite_r1 = typesite, # Area of residence (urban/rural)
    hghgrade_r1 = hghgrade, # Highest grade achieved at time of interview
    zwfa, # Weight-for-age z-score
    zhfa, # Height-for-age z-score
    zbfa, # BMI-for-age z-score
    underweight, # Low weight for age
    stunting, # Short height for age
    thinness, # Low BMI for age
    # dadedu_r1 = dadedu, # Father's level of education
    # dadcantread_r1 = dadcantread, # Father cannot read
    # dadyrdied_r1 = dadyrdied, # Year when father has died
    # momedu_r1 = momedu, # Mother's level of education
    # momcantread_r1 = momcantread, # Mother cannot read
    # momyrdied_r1 = momyrdied, # Year when mother has died
    caredu_r1 = caredu, # Caregiver's level of education
    careage_r1 = careage, # Caregiver's age
    caresex_r1 = caresex, # Caregiver's sex
    carerel_r1 = carerel, # Caregiver's relationship to YL child
    carecantread_r1 = carecantread, # Caregiver cannot read
    ownlandhse_r1 = ownlandhse, #  Household owns land where house is on
    numante,
    childloc_r1 = childloc,
    psnp_pw_r1 = psnp_pw,
    psnp_ds_r1 = psnp_ds,
    othprog_r1 = othprog,
    hep_r1 = hep,
    resettled_r1 = resettled,
    eap_r1 = eap,
    foodsec_r1 = foodsec,
    credit_r1 = credit
  ) 

# Construction of variables ####

# Construct the (local) language of instruction in primary schools

language_primary <- vars_r4 %>% 
  select(child_id, hghgrade_r4) %>% 
  left_join(test_r3, by = "child_id") %>% 
  left_join(test_r4, by = "child_id") %>% 
  mutate(
    # 
    lang_primary =
      case_when(
        # If child is in primary school and reported a local language of 
        # instruction, assign that as the language of primary school
        (hghgrade_r4 <= 8 & lang_instr != "English") ~ lang_instr,
        
        # Otherwise, use the language used during the math test in round 3 as
        # a proxy
        TRUE ~ lang_math_test
      ) %>% factor()
  ) %>% select(-hghgrade_r4) 
  # select(child_id, lang_primary)


# join everything together ####

joined <- time_invar %>% 
  left_join(language_primary, by = "child_id") %>% 
  left_join(vars_r1, by = "child_id") %>% 
  left_join(vars_r2, by = "child_id") %>% 
  left_join(vars_r3, by = "child_id") %>% 
  left_join(vars_r4, by = "child_id") %>% 
  left_join(vars_r5, by = "child_id") %>% 
  left_join(activity_r5, by = "child_id") 

# Generate a single "region" and "highest leve of education" variables
joined <- joined %>% 
  mutate(
    region = case_when(
      
      !is.na(region_r3) ~ region_r3,
      
      is.na(region_r3) & !is.na(region_r2) ~ region_r2,
      
      is.na(region_r3) & is.na(region_r2) ~ region_r1, # there are no missing values for "region_r1"
    ),
    
    hghgrade_4 = case_when(
      
      !is.na(hghgrade_r4) ~ hghgrade_r4,
      
      is.na(hghgrade_r4) & is.na(hghgrade_r3) ~ hghgrade_r3,
      
      is.na(hghgrade_r4) & is.na(hghgrade_r3) & 
        !is.na(hghgrade_r2) ~ hghgrade_r2,
      
      is.na(hghgrade_r4) & is.na(hghgrade_r3) & 
        is.na(hghgrade_r2) ~ hghgrade_r1
      
    ),
    
    hghgrade_final = case_when(
      
      !is.na(hghgrade_r5) ~ hghgrade_r5,
      
      is.na(hghgrade_r5) ~ hghgrade_r4
    ),
    
    hghgrade_4_num = case_when(
      hghgrade_r4 <= 14 ~ as.numeric(hghgrade_r4),
      # Recode "Religious education" and "Other" to 0
      hghgrade_r4 %in% c(29, 30) ~ 0
    ),
    
    hghgrade_final_num = case_when(
      hghgrade_final <= 14 ~ as.numeric(hghgrade_final),
      # Recode "Religious education" and "Other" to 0
      hghgrade_final %in% c(29, 30) ~ 0
    ),
    
    care_educ_num = case_when(
      caredu_r1 <= 14 ~ as.numeric(caredu_r1),
      # Recode "Adult literacy" and "Religious education" to 0
      caredu_r1 %in% c(28, 29) ~ 0
    )
  )

# Function to generate intensity of treatment variable:

top_code <- function(var, ceil) {
  
  out <- if_else( var > ceil, ceil, var )
  
  return(out)
}

# Generate main outcome variables and others
joined <- joined %>% 
    mutate(
      
      active = case_when(
        hwork_r5 == 0 ~ 0, 
        hwork_r5 > 0 ~ 1
      ),
      
      wage_employ = case_when(
        # The following get 1:
        #    5: Wage Employment (Agriculture)
        #   12: Wage Employment (Unsalaried; Non-agriculture)
        #   13: Regular Salaried Employment
        type_activ %in% c(5, 12, 13) ~ 1,
        
        # If both "type_activ" and "active" are missing, they get NA.
        is.na(type_activ) & is.na(active) ~ NA_real_,
        
        # Moreover, self employed are excluded:
        type_activ %in% c(1, 2, 3, 4, 8, 9, 10, 11, 19) ~ NA_real_,
        
        # Otherwise, 0.
        TRUE ~ 0
      ),
      
      self_employ = case_when(
        # run "joined %>% count(type_activ)" to see the list:
        type_activ %in% c(1, 2, 3, 4, 8, 9, 10, 11) ~ 1,
        
        # If both "type_activ" and "active" are missing, they get NA.
        is.na(type_activ) & is.na(active) ~ NA_real_,
        
        # Moreover, wage employed are excluded:
        type_activ %in% c(5, 12, 13, 19) ~ NA_real_,
        
        # Otherwise, 0.
        TRUE ~ 0
      ),
      
      wage_employII = case_when(
        # The following get 1:
        #   12: Wage Employment (Unsalaried; Non-agriculture)
        #   13: Regular Salaried Employment
        type_activ %in% c(13) ~ 1,
        
        # If both "type_activ" and "active" are missing, they get NA.
        is.na(type_activ) & is.na(active) ~ NA_real_,
        
        # Moreover, self employed in non-agri are excluded:
        type_activ %in% c(8, 9, 10, 11, 19) ~ NA_real_,
        
        # Otherwise, 0.
        TRUE ~ 0
      ),
      
      self_employII = case_when(
        # select self employed in non-agriculture
        type_activ %in% c(8, 9, 10, 11) ~ 1,
        
        # If both "type_activ" and "active" are missing, they get NA.
        is.na(type_activ) & is.na(active) ~ NA_real_,
        
        # Moreover, wage employed are excluded:
        type_activ %in% c(13, 19) ~ NA_real_,
        
        # Otherwise, 0.
        TRUE ~ 0
      ),
      
      chsex = case_when(
        chsex == 1 ~ "Male", chsex == 2 ~ "Female"
      ) %>% factor(),
      
      rural_3 = case_when(
        typesite_r3 == 1 ~ 0, typesite_r3 == 2 ~ 1
      ) 
  )

# Generate instruments
joined <- joined %>% 
  mutate(
    IMTI = case_when(

      region == "Tigray" & chlang == "Tigrigna" & lang_primary == "Tigrigna"
      ~ top_code(hghgrade_final_num, 8),

      region == "Oromia" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo"
      ~ top_code(hghgrade_final_num, 8),

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
    
    # omit child language "other" 
    # region == "SNNP" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" 
    # ~ top_code(hghgrade_final_num, 4), ?
    
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

# remove unnecessary columns:
joined <- joined %>% 
  select(-starts_with(c("region_r", "hghgrade_r"))) 

# Split the sample to two (AA and Non-AA):
aa_samp <- joined %>% 
  filter(region ==  "Addis Ababa")

non_aa_samp <- joined %>% 
  filter(region !=  "Addis Ababa")

write_csv(aa_samp, "data/aa_samp.csv")
write_csv(non_aa_samp, "data/non_aa_samp.csv")


