
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)

# Read data 
child_hh_r2 <- read_dta("raw_data/Round_2_2006/etchildlevel12yrold.dta")

child_hh_r2 <- child_hh_r2 %>% 
  select(
    child_id = CHILDID,  # id
    sex = SEX,      # sex of child
    urban = TYPESITE, # Urban/Rural
    region_r2 = REGION,   # Region of residence.
    # lang_local = EDOMLANG, # most commonly used language in the locality
    ethnic_orig = CHLDETH,  # child's ethnic origin
    lang_frst = CHLNG1ST, # the first language the child learned at home
    # speak_local = CHSPKLNG, # does child speak the most commonly used language 
    # address_born = CHILDLIV, # Where did YL child live when she/he was born?
    # yrs_lived = LONGLIV,  # How long has YL child lived here (in years)? 
    # left = LEFT      # Has the YL child ever left this locality? 
    fath_educ = DADED, # highest education grade completed by the father 
    moth_educ = MUMED, # highest education grade completed by the mother 
    care_educ = CAREED # highest education grade completed by the caregiver
  ) %>% mutate(child_id = str_sub(child_id, 3, 8) %>% as.numeric()) #%>% 
  # left_join(
  #   child_hh_r3 %>% 
  #     select(child_id, region, chldlang, chspklr3),
  #   by = "child_id"
  # ) %>% left_join(
  #   child_r3,
  #   by = "child_id"
  # )


# Ethnicity and Language

# Ethnic origin is not deterministic of first language
child_hh_r2 %>% 
  count(ethnic_orig, lang_frst) %>% 
  print(n = Inf)

# Even in Oromia, there are 12 Oromo kids whose first lang. is not Afaan Oromo
child_hh_r2 %>% 
  filter(ethnic_orig == 16) %>% 
  count(region, lang_frst) %>% 
  print(n = Inf)


# Merge this with round 3:
child_lang_r2 <- child_hh_r2 %>% 
  left_join(language_r3, by = "child_id")

child_lang_r2 %>% 
  count(lang_frst, lang_math_test, sort = TRUE) %>% 
  print(n = Inf)

# Language in round 3 can be ignored
child_lang_r2 %>% 
  count(lang_frst, chldlang, sort = TRUE) %>% 
  print(n = Inf)

# We should consider migration to d/t regions (although a minority)
child_lang_r2 %>% 
  count(region_r2, region_r3, sort = TRUE) %>% 
  print(n = Inf)


# Child level data ####

# Read data 
child_r2 <- read_dta("raw_data/Round_2_2006/etchildquest12yrold.dta")

language_r2 <- child_r2 %>% 
  select(
    child_id = CHILDID,  # id
    lang_verbal = PPCDLANG, # Language used by child during administration
    verbal_score = score_ppvt,  
    math_score = score_math
  ) 

language_r2 %>% 
  count(lang_verbal)












