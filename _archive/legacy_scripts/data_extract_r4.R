
# Date created: March 02, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(janitor)

# Educational History in Round 4

educ_hist_r4 <- read_dta(
  "raw_data/Round_4_2013-14/et_r4_occh_educationhistoryindexchild.dta"
)

educ_hist_r4 <- educ_hist_r4 %>% 
  select(
    child_id = CHILDCODE, # id
    year = EDCHSTR4, # Year (Educ. History)
    age = EDAGR4, # Age in years
    grade = GRDER4, # Grade enrolled in
    type = TYSCR4, # Type of school/educational institute/university
  ) 

occh <- read_dta(
  "raw_data/Round_4_2013-14/et_r4_occog_olderchild.dta"
)

occh %>% 
  count(RDITLANR4, sort = TRUE)

# join with educ data (load "cons_oc" from "data_extract_all")

educ_r4 <- cons_oc %>% 
  mutate(child_id = str_remove(childid, "ET") %>% as.numeric()) %>% 
  filter(round == 4) %>% 
  select(child_id, region_r4 = region, chsex, 
         chethnic, chlang, hghgrade) 

occh_educ <- occh %>% 
  rename(child_id = CHILDCODE) %>% 
  left_join( educ_r4, by = "child_id" ) %>% 
  select(child_id, region_r4, chsex, 
         chethnic, chlang, everything()) %>% 
  rename( lang_instr = RDITLANR4 )

occh_educ %>% 
  count(region_r4, lang_instr)

occh_educ %>% 
  filter(lang_instr == 1) %>% 
  count(region_r4, hghgrade, sort = TRUE) %>% 
  arrange(region_r4) %>% 
  print(n = Inf)

# We can use reported language of instruction at school only for those
# in primary school:
occh_educ %>% 
  filter(hghgrade <= 8) %>% 
  count(region_r4, lang_instr)

# for the rest: need to use language of math test as a proxy from round 3:
occh_educ <- occh_educ %>% 
  left_join(language_r3, by = "child_id") 

occh_educ <- occh_educ %>%
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
    
    lang_math_test = case_when(
      lang_math_test == 2 ~ "Amharic",
      lang_math_test == 7 ~ "Hadiya",
      lang_math_test == 11 ~ "Afaan Oromo",
      lang_math_test == 12 ~ "Sidamigna",
      lang_math_test == 15 ~ "Tigrigna",
      lang_math_test == 16 ~ "Wolayta",
      lang_math_test %in% c(88, NA) ~ NA_character_,
    ),
    
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
    ),
    
    lang_primary =
      case_when(
        hghgrade <= 8 ~ lang_instr,
        TRUE ~ lang_math_test
      ) %>% factor()
  
    )

occh_educ %>% 
  count(lang_primary)

occh_educ %>% 
  count(region_r4, lang_primary)

# Educational attainment and test scores from round 4
# Labour market outcomes from round 5
# region from round 3

# Not much migration between rounds 3 and 4:
occh_educ %>% 
  count(region_r3, region_r4, sort = TRUE) %>% 
  print(n =Inf)

# Generate intensity of treatment variable:

top_code <- function(var, ceil) {
  
  out <- if_else( var > ceil, ceil, var )
  
  return(out)
}

occh_educ_gen <- occh_educ %>% 
  mutate(
    hghgrade_num = as.numeric(hghgrade),
    
    IMTI = case_when(
      
      region_r3 == "Tigray" & chlang == "Tigrigna" & lang_primary == "Tigrigna" 
      ~ top_code(hghgrade_num, 8), 
      
      region_r3 == "Oromia" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" 
      ~ top_code(hghgrade_num, 8), 
      
      region_r3 == "Oromia" & chlang == "Amaharic" & lang_primary == "Amharic" 
      ~ top_code(hghgrade_num, 8), 
      
      region_r3 == "Amhara" & chlang == "Amharic" & lang_primary == "Amharic" 
      ~ top_code(hghgrade_num, 6),
      
      region_r3 == "Amhara" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" 
      ~ top_code(hghgrade_num, 8),
      
      region_r3 == "SNNP" & chlang == "Sidamigna" & lang_primary == "Sidamigna" 
      ~ top_code(hghgrade_num, 4),
      
      region_r3 == "SNNP" & chlang == "Wolayta" & lang_primary == "Wolayta" 
      ~ top_code(hghgrade_num, 4),
      
      region_r3 == "SNNP" & chlang == "Hadiya" & lang_primary == "Hadiya" 
      ~ top_code(hghgrade_num, 4),
      
      region_r3 == "SNNP" & chlang == "Amharic" & lang_primary == "Amharic" 
      ~ top_code(hghgrade_num, 4),
      
      region_r3 == "Addis Ababa" & chlang == "Amharic" & lang_primary == "Amharic" 
      ~ top_code(hghgrade_num, 6),
      
      TRUE ~ 0
    ),
    
    # omit child language "other" 
    # region_r3 == "SNNP" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" 
    # ~ top_code(hghgrade_num, 4), ?
    
    E_is = case_when(
      region_r3 == "Oromia" & chethnic == 16~ 1,
      
      region_r3 == "Tigray" & chethnic == 18 ~ 1,
      
      region_r3 == "Oromia" & chethnic == 12 & lang_primary == "Amharic" ~ 1,
      
      region_r3 == "Amhara" & chethnic == 16 & lang_primary == "Afaan Oromo" ~ 1,
      
      region_r3 == "Amhara" & chethnic == 12 ~ 0.75,
      
      region_r3 == "SNNP" & chethnic %in% c(14, 17, 19, 12) & 
        lang_primary %in% c("Hadiya", "Sidamigna", "Wolayta", "Amharic") ~ 0.5,
      
      TRUE ~ 0
    )
    
  )

occh_educ_gen %>% 
  count(IMTI)

occh_educ_gen %>% 
  count(E_is)

occh_educ_gen %>% 
  count(IMTI, E_is) 

mod <- lm(IMTI ~ E_is, data = occh_educ_gen)
stargazer::stargazer(mod, type = "text")

occh_educ_gen %>% 
  count(region_r3, IMTI) %>% print(n = Inf)











joined %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(region, maths_raw)) +
  geom_boxplot()

joined %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% 
  summarise(mean_math = mean(maths_raw, na.rm = TRUE)) %>% 
  ggplot(aes(region, mean_math)) +
  geom_col()

joined %>% 
  filter(!is.na(region)) %>%
  ggplot(aes(maths_raw)) +
  geom_histogram() +
  facet_wrap(~ region)

joined %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(region, lang_raw)) +
  geom_boxplot()


educ_hist_r4 <- educ_hist_r4 %>% 
  filter(REGNR4 %in% c(1, 3, 4, 7, 14)) %>% 
  mutate(
    region = case_when(
      REGNR4 ==  14 ~ "Addis Ababa",
      REGNR4 ==  3 ~ "Amhara",
      REGNR4 ==  4 ~ "Oromia",
      REGNR4 ==  7 ~ "SNNP",
      REGNR4 ==  1 ~ "Tigray"
    ) %>% factor() ) 


joined %>% 
  count(REGNR4 , RDITLANR4) %>% 
  print(n = Inf)




