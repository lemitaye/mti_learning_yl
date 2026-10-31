
# Date created: February 24, 2022

# preparation
rm(list = ls())

library(tidyverse)
library(haven)
library(janitor)

# Read data 
child_r5 <- read_dta("raw_data/Round_5_2016/et_r5_occh_olderchild.dta")
activity_r5 <- read_dta("raw_data/Round_5_2016/et_r5_occh_activity.dta")

# cleaning

child_r5 <- child_r5 %>% 
  select(
    child_id = CHILDCODE, # id
    region = NEWRGCR5, # Region of residence – OBSERVE
    highst_qual = HGHQULR5, # highest completed qualification/certificate
    now_in_educ = ENRSCHR5, # Are you currently in full‐time education?
    marital = MRTSTSR5 # What is your current marital status?
  )
  
activity_r5 <- activity_r5 %>% 
  select(
    child_id = CHILDCODE, # id
    no_activ = ACTIDR5,   # No. of Paid Activities
    type_activ = ACTR5,     # Type of activity_r5
    owner_activ = ACTOWNR5,  # Who do you do this activity_r5 for?
    pay_form = PYMRECR5,     # form of payment 
    in_cash = ERNCSHR5,      # In cash (value in Birr)
    in_kind_value = ERNKNDR5, # In kind (cash equivalent) (value in Birr)
    pay_period = HWPAIDR5,  # What period of time did this payment cover?
    current_perform = PRFACTR5 # Do you currently perform this activity_r5?  
  )

joined_ac <- activity_r5 %>% 
  left_join(child_r5, by = "child_id") %>% 
  filter(!is.na(type_activ)) %>% 
  mutate( agg_activity = case_when(
    type_activ %in% c(1:4, 8:11) ~ "Self Employed",
    type_activ %in% c(5, 12, 13) ~ "Wage employment (salaried & unsalaried)",
    type_activ == NA ~ "Unknown",
    TRUE ~ "Other"
  ))

joined_ac %>% 
  count(agg_activity)


joined_ac %>%
  filter(!(agg_activity %in% c("Unknown", "Other"))) %>% 
  mutate(
    wage_employ = case_when(
      agg_activity == "Wage employment (salaried & unsalaried)" ~ 1,
      TRUE ~ 0
  )) %>% 
  filter(region %in% c(1, 3, 4, 7, 14)) %>% 
  mutate(
    region = case_when(
      region ==  14 ~ "Addis Ababa",
      region ==  3 ~ "Amhara",
      region ==  4 ~ "Oromia",
      region ==  7 ~ "SNNP",
      region ==  1 ~ "Tigray"
    ) %>% factor() ) %>% 
  group_by(region) %>% 
  summarise(mean_wage_employ = mean(wage_employ)) %>% 
  mutate(region = fct_reorder(region, mean_wage_employ)) %>% 
  ggplot(aes(region, mean_wage_employ)) +
  geom_col() +
  scale_y_continuous(labels = percent)
  


activity_r5 %>% 
  count(no_activ)

activity_r5 %>% 
  count(type_activ)

activity_r5 %>% 
  count(pay_form)
  
activity_r5 %>% 
  count(type_activ, pay_form) %>% 
  # filter(pay_form == 0) %>% 
  print(n = Inf)

activity_r5 %>% 
  filter(type_activ %in% 1:7) 
  
child_r5 %>% 
  count(educ_high)

# After joining with r3: type of activity by region
# (think of doing this by sector)
activity_r5 %>% 
  left_join(child_hh_r3, by = "child_id") %>% 
  filter(!is.na(type_activ)) %>% 
  mutate( agg_activity = case_when(
    type_activ %in% c(1:4, 8:11) ~ "Self Employed",
    type_activ %in% c(5, 12, 13) ~ "Wage employment (salaried & unsalaried)",
    type_activ == NA ~ "Unknown",
    TRUE ~ "Other"
  )) %>% 
  count(region, agg_activity) %>% print(n = Inf)


# anti_join(x, y) drops all observations in x that have a match in y.

activity_r5 %>% 
  anti_join(child_hh_r3, by = "child_id")

child_r5 %>% 
  anti_join(child_hh_r3, by = "child_id")

# earnings:

activity_r5 %>% 
  filter(is.na(in_kind_value), is.na(in_cash)) %>% 
  count(type_activ, sort = TRUE)

activity_r5 %>% 
  filter(!is.na(in_kind_value) | !is.na(in_cash)) %>% 
  count(pay_period, sort = TRUE)

earning_month <- activity_r5 %>% 
  left_join(child_hh_r3, by = "child_id") %>% 
  filter( !is.na(in_kind_value) | !is.na(in_cash) ) %>% 
  mutate(
    in_cash = replace_na(in_cash, 0),
    in_kind_value = replace_na(in_kind_value, 0),
    raw_earning = in_cash + in_kind_value,
    std_earning = case_when(
      pay_period == 2 ~ raw_earning * 30,
      pay_period == 3 ~ raw_earning * 4,
      pay_period == 5 ~ raw_earning / 12,
      TRUE ~ raw_earning       
    )
      ) %>% 
  mutate(
    region = case_when(
      region ==  11 ~ "Addis Ababa",
      region ==  12 ~ "Amhara",
      region ==  13 ~ "Oromia",
      region ==  14 ~ "SNNP",
      region ==  15 ~ "Tigray"
    ) %>% factor() ) %>% 
  filter(in_kind_value >= 0, !is.na(region)) 
  

earning_month %>% 
  filter(std_earning < 1000) %>% 
  ggplot(aes(region, std_earning)) +
  geom_boxplot()

# should still be very careful since we have removed those who are
# self-employed (half of the data), which might lead to systematic
# bias

# Educational attainment in round 5

educ_high <- child_r5 %>% 
  select(-region) %>% 
  inner_join(
    child_hh_r3 %>% select(child_id, region), 
    by = "child_id") %>% 
  filter(highst_qual %in% c(0:3, 6, 7)) %>% 
  mutate(
    highst_qual = case_when(
      highst_qual ==  0 ~ "No certificate",
      highst_qual ==  1 ~ "Grade 8 completion",
      highst_qual ==  2 ~ "Ethiopian General secondary education",
      highst_qual ==  3 ~ "Ethiopian higher education entrance certificate",
      highst_qual ==  6 ~ "Completion TVET certificate",
      highst_qual ==  7 ~ "University Degree"
    ) %>% factor( levels = c(
      "No certificate",
      "Grade 8 completion",
      "Ethiopian General secondary education",
      "Completion TVET certificate",
      "Ethiopian higher education entrance certificate",
      "University Degree"
    ) ) 
    )

educ_high %>% 
  count(region, educ_high) %>% 
  ggplot(aes(highst_qual, n)) +
  geom_col(mapping = aes(fill = region), 
           position = "dodge") +
  coord_flip() + 
  labs(
    title = "Highest completed qualification by region",
    x = "",
    y = "Count",
    fill = "Region"
  )
  


# Round 5 Education History

educ_hist_r5 <- read_dta(
  "raw_data/Round_5_2016/et_r5_occh_educationhistoryindexchild.dta"
  )

educ_hist_r5 <- educ_hist_r5 %>% 
  select(
    child_id = CHILDCODE, # id
    year = EDCHSTR5, # Year (Educ. History)
    age = EDAGR5, # Age in years
    grade = EDGRADER5, # Grade enrolled in
    complete = CMPGRDR5, # Successfully completed 
    type = TYSCR5, # Type of school/educational institute/university
  ) %>% 
  filter(year != 1)


# group by child_id, remove all missing values and pick the latest year

educ_r45 <- educ_hist_r5 %>% 
  bind_rows(educ_hist_r4) %>% 
  arrange(child_id)

educ_r45 %>% 
  count(child_id) %>% 
  count(n)

educ_r45 <- educ_r45 %>% 
  mutate(year = case_when(
    year == 2 ~ 2007,
    year == 3 ~ 2008,
    year == 4 ~ 2009,
    year == 10 ~ 2006,
    year == 11 ~ 2005,
    year == 12 ~ 2004,
    year == 13 ~ 2003,
    year == 14 ~ 2002
  ))

# Check if there are missing values for all children & years:
educ_r45 %>% 
  group_by(child_id) %>% 
  mutate( allNA = all(is.na(grade)), all_none = all(grade == 0) ) %>% 
  ungroup() %>% 
  count(allNA, all_none) # this should only be false

# Function to omit None (0) and NAs
omitter <- function(x) {
  x <- x[x != 0 & !is.na(x)]
  return(x)
}

educ_highst_r5 <- educ_r45 %>% 
  group_by(child_id) %>% 
  arrange(desc(year), .by_group = TRUE) %>%
  mutate( allNA = all(is.na(grade)), all_none = all(grade == 0) ) %>% 
  mutate(
    grade = as.numeric( grade ),
    highst_grade = case_when(
      allNA ~ NA_real_,
      all_none ~ 0,
      TRUE ~ first(omitter(grade))
    ),
    ) %>% 
  # Try to get the year of highest educ (may be use logical var)
  slice(1) %>%
  ungroup() %>% 
  # Then join this with their highest qualification 
  left_join(child_r5, by = "child_id")



# if all_none = TRUE, then pick 0

educ_highst_r5 %>% 
  filter(highst_qual %in% c(8, 88, NA)) %>% 
  count(grade, now_in_educ) %>% 
  print(n = Inf)

# Entire educational history

# combine with educ_hist_r3 and plot prop. completing a grade by year

educ_history <- educ_r45 %>% 
  select(child_id, year, age, grade) %>% 
  bind_rows(educ_hist_r3) %>% 
  arrange(child_id, year)

educ_history %>% 
  group_by(child_id) %>% 
  mutate( allNA = all(is.na(grade)) ) %>% 
  ungroup() %>% 
  count(allNA)

educ_history %>% 
  group_by(child_id) %>% 
  mutate( all_none = all(grade == 0) ) %>% 
  ungroup() %>% 
  count(all_none)
  
educ_highst <- educ_history %>% 
  group_by(child_id) %>% 
  arrange(desc(year), .by_group = TRUE) %>%
  mutate( allNA = all(is.na(grade)), all_none = all(grade == 0) ) %>% 
  mutate(
    # grade = as.numeric( grade ),
    highst_grade = case_when(
      allNA ~ NA_real_,
      all_none ~ 0,
      TRUE ~ first(omitter(as.numeric( grade )))
    ),
    region = last(region)
  ) %>% 
  # Try to get the year of highest educ (may be use logical var)
  slice(1) %>%
  ungroup()

educ_highst %>% 
  select(child_id, region, highst_grade) %>% 
  left_join(child_r5, by = "child_id") %>% 
  filter(highst_qual %in% c(8, 77, 88, NA)) %>% 
  count(highst_grade) %>% 
  print(n = Inf)

# Finally, there is some hope!


occh_activity %>% 
  group_by(CHILDCODE) %>% 
  mutate(n = n()) %>% 
  select(CHILDCODE, ACTIDR5, n) %>% 
  filter(n > 1)
















