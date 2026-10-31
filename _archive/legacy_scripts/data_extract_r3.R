
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(scales)
theme_set(theme_light())

# Read data ####
child_hh_r3 <- read_dta("raw_data/Round_3_2009/et_oc_householdlevel.dta")


child_hh_r3 <- child_hh_r3 %>% 
  select(
    child_id = childid, # id
    region,  # region
    chldlang, # Language spoken by child
    leftr3, # Has child left this locality since last visit?
    howlngr3, # How long did NAME leave the locality for (since our last visit)? (in months)
    reasonr3, # What is the most important reason NAME left this locality for more than 3 months?
    chspklr3, # Currently does NAME speak the most commonly used language in the locality?
    # esagr386, grder386, # Age in 1986; highest grade in 1986 
    # esagr387, grder387, # Age in 1987; highest grade in 1987 
    # esagr388, grder388, # Age in 1988; highest grade in 1988 
    # esagr389, grder389, # Age in 1989; highest grade in 1989 
    # esagr390, grder390, # Age in 1990; highest grade in 1990 
    # esagr391, grder391, # Age in 1991; highest grade in 1991 
    # esagr392, grder392, # Age in 1992; highest grade in 1992 
    # esagr393, grder393, # Age in 1993; highest grade in 1993 
    # esagr394, grder394, # Age in 1994; highest grade in 1994 
    # esagr395, grder395, # Age in 1995; highest grade in 1995 
    # esagr396, grder396, # Age in 1996; highest grade in 1996 
    # esagr397, grder397, # Age in 1997; highest grade in 1997 
    # esagr398, grder398, # Age in 1998; highest grade in 1998 
    # esagr399, grder399, # Age in 1999; highest grade in 1999 
    # esagr300, grder300, # Age in 2000; highest grade in 2000 
    # esagr301, grder301  # Age in 2001; highest grade in 2001 
  ) %>% 
  mutate(child_id = str_sub(child_id, 3, 8) %>% as.numeric()) %>% 
  mutate(
    region = case_when(
      region ==  11 ~ "Addis Ababa",
      region ==  12 ~ "Amhara",
      region ==  13 ~ "Oromia",
      region ==  14 ~ "SNNP",
      region ==  15 ~ "Tigray"
    ) %>% factor() ) 

child_hh_r3 %>% 
  filter(leftr3 == 1) %>% 
  count(howlngr3)

child_hh_r3 %>% 
  count(grder301)

attain <- child_hh_r3 %>% 
  select(region, grder301) %>% 
  filter(!is.na(grder301)) %>% 
  group_by(region) %>% 
  mutate(
    prop_5 = sum( (grder301 >= 5) )/n(),
    prop_6 = sum( (grder301 >= 6) )/n(),
    prop_7 = sum( (grder301 >= 7) )/n(),
    prop_8 = sum( (grder301 >= 8) )/n()
    ) %>% 
  filter(row_number() == 1) %>% 
  select(-grder301) %>% 
  pivot_longer(
    c("prop_5", "prop_6", "prop_7", "prop_8"), 
    names_to = "grade", 
    values_to = "prop"
    )

attain %>% 
  mutate(
    grade = recode(
    grade, 
    "prop_5" = "Grade 5 or higher", 
    "prop_6" = "Grade 6 or higher",
    "prop_7" = "Grade 7 or higher",
    "prop_8" = "Grade 8 or higher")
    ) %>% 
  ggplot(aes(region, prop)) + 
  geom_col(fill = "#4682B4") +
  facet_wrap(~ grade) +
  scale_y_continuous(label = percent) + 
  labs(
    title = "Percentage of Highest Grade Completed by Region (Round 3)",
    x = "Region",
    y = "Percent"
  )
  
child_hh_r3 %>% 
  count(region)


# Analysis of educational history ####

age <- child_hh_r3 %>% 
  select(child_id, region, contains("esagr")) %>% 
  pivot_longer(
    -c(child_id, region), 
    names_to = "esagr", 
    values_to = "age"
    ) %>% 
  mutate(
    last_two = str_sub(esagr, start= -2) %>% as.numeric(),
    year = 
      case_when(
      last_two < 2 ~ str_c("200", as.character(last_two)),
      TRUE ~ str_c("19", as.character(last_two))
    ) %>% as.numeric()
    ) %>% select(child_id, region, year, age)


grade <- child_hh_r3 %>% 
  select(child_id, contains("grder")) %>% 
  pivot_longer(
    cols = -child_id, 
    names_to = "grder", 
    values_to = "grade"
    ) %>% 
  mutate(
    last_two = str_sub(grder, start= -2) %>% as.numeric(),
    year = 
      case_when(
        last_two < 2 ~ str_c("200", as.character(last_two)),
        TRUE ~ str_c("19", as.character(last_two))
      ) %>% as.numeric()
  ) %>% select(child_id, year, grade)
  
educ_hist_r3 <- age %>% 
  left_join(grade, by = c("child_id", "year"))

# Check if there are missing values for all children & years:
educ_highst_r3 %>% 
  group_by(child_id) %>% 
  mutate( allNA = all(is.na(grade)) ) %>% 
  ungroup() %>% 
  count(allNA) # this should only be false


# educ_highst_r3 <- educ_hist_r3 %>% 
#   group_by(child_id) %>% 
#   filter(!is.na(grade)) %>% 
#   arrange(desc(year), .by_group = TRUE) %>%
#   slice(1) %>%
#   ungroup() 
# 
# educ_highst_r3 %>% 
#   count(grade) %>% 
#   print(n = Inf)

# Analysis of Language ####

child_r3 <- read_dta("raw_data/Round_3_2009/et_oc_childlevel.dta")

language_r3 <- child_r3 %>% 
  select(
    child_id = CHILDID, # id
    # GDR8EXR3, # Have you taken the grade 8 examination?
    # lang_teach = CLTCSPR3, # Language usually spoken by teachers
    # lang_verbal_test = VRBLNGTS, # Language in which the test was written (verbal test) 
    # lang_verbal_child = PPCDLANG, # Language used by child during administration (verbal test)
    lang_math_test = MTHLNGTS,
    verbal_score = ppvt,
    math_score = math,
  ) %>%
  mutate(child_id = str_remove(child_id, "ET") %>% as.numeric()) %>%
  left_join(
    child_hh_r3 %>%
      select(child_id, region_r3 = region),
    by = "child_id"
    )

# Assuming children take the test in the language of instruction at school:
# Math test language is a strong proxy for language of instruction  
language_r3 %>% 
  count(lang_verbal_test, lang_math_test, sort = TRUE)

language_r3 %>% 
  count(lang_math_test, sort = TRUE)

language_r3 %>% 
  select(contains("verbal"))



child_r3 %>% glimpse()
summary(child_r3$math_co)
summary(child_r3$ppvt_co)

child_r3 %>% 
  count(MTHLNGTS)


language_r3 %>% #
  group_by(region) %>% 
  summarise(mean_score_math = mean(math_score),
            )
  

language_r3 %>% 
  ggplot(aes(region_r3, verbal_score)) +
  geom_boxplot()

language_r3 %>% 
  ggplot(aes(region_r3, math_score)) +
  geom_boxplot()

language_r3 %>% 
  group_by(region_r3) %>% 
  summarise(
    mean_math = mean(math_score),
    mean_verbal = mean(verbal_score)
    ) %>% 
  ggplot(aes(region_r3, mean_math)) +
  geom_col()

language_r3 %>% 
  group_by(region_r3) %>% 
  summarise(
    mean_math = mean(math_score, na.rm = TRUE),
    mean_verbal = mean(verbal_score, na.rm = TRUE)
  ) %>% 
  ggplot(aes(region_r3, mean_math)) +
  geom_col()

language_r3 %>% 
  group_by(region_r3) %>% 
  summarise(
    mean_math = mean(math_score, na.rm = TRUE),
    mean_verbal = mean(verbal_score, na.rm = TRUE)
  ) %>% 
  ggplot(aes(region_r3, mean_verbal)) +
  geom_col()

language_r3 %>% 
  ggplot(aes(verbal_score)) +
  geom_histogram() +
  facet_wrap(~ region_r3)

hist(language_r3$math_score)
hist(language_r3$verbal_score)

language_r3 %>% 
  count(chldlang_r3, lang_math_test, sort = TRUE)

language_r3 %>% 
  count(region_r3, lang_math_test, sort = TRUE)


