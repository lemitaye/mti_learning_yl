

# Date created: March 11, 2022

# This script brings all the five rounds together to produce a single 
# file ready for analysis

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(scales)
theme_set(theme_light())

# Read data ####
constructed <- read_dta(
  "raw_data/Rounds_1-5_Constructed_Files/ethiopia_constructed.dta"
  )

cons_oc <- constructed %>% 
  filter(yc == 0) %>%  # filter older cohort 
  filter(is.na(deceased)) %>% # filter non-deceased
  select(
    CHILDID, childid:cookingq_new,
    -c(deceased, chsmoke)
    ) 



cons_oc %>% 
  select(childid, round, region, chlang, chethnic, hghgrade) 

cons_oc %>% 
  select(childid, round, inround, region, hghgrade) %>% 
  filter(round == 4) %>% 
  count(hghgrade)

lm(math_raw ~ factor(region) + factor())





















