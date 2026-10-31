
# Date created: March 29, 2022
# DEPENDS ON: "01_extract_clean.R"

# This script runs the main regressions using the data generated in "01_extract_clean.R"

rm(list = ls())

# load the data:
aa_samp <- read_csv("data/aa_samp.csv") %>% 
  mutate_if(is.character, as.factor) %>% # convert all chr. to factor.
  filter(raw_lang != 0) 

non_aa_samp <- read_csv("data/non_aa_samp.csv") %>% 
  mutate_if(is.character, as.factor) %>% # convert all chr. to factor.
  filter(raw_lang != 0)   



# Regression with covariates: ####

make_formula_frst_stg <- function(dep_var, instrument, clus = FALSE, added = NULL) {
  # Define a vector of covariates
  covar1 <- c(
    "zbfa", "caredu_r1", "careage_r1", "factor(caresex_r1)", 
    "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new", 
    "chsex", "factor(chldrel)", "preprim", "agegr1", 
    "factor(foodsec_r3)"
    )

  covar <- paste(
    paste(covar1, collapse = "+")
  )
  
  if (clus) {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", added, covar,
      " | 0 | 0 | region"
    ))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", added, covar
      ))
  }
  
  return(f)
}

f1 <- make_formula_frst_stg("IMTI", "E_is", added = "factor(typesite_r3) +", clus = TRUE)

m1 <- felm(f1, data = non_aa_samp)

stargazer(
  m1, 
  keep = c("E_is", "region", "chsex"),
  type = "text",
  keep.stat = c("n","rsq")
  )

waldtest(m1, ~ E_is)[["F"]]

# Function to standardize test scores:
standardize <- function(x){ 
  z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) 
  return( z)
}

aa_samp <- aa_samp %>% 
  mutate(
    std_maths = standardize(raw_maths),
    std_lang = standardize(raw_lang)
  )

non_aa_samp <- non_aa_samp %>% 
  mutate(
    std_maths = standardize(raw_maths),
    std_lang = standardize(raw_lang)
  )

# Reduced form for the AA sample:

f_rf1 <- make_formula_frst_stg("wage_employ", "E_is", added = "entype_r4 +")
f_rf2 <- make_formula_frst_stg("raw_maths", "E_is", added = "entype_r4 +")
f_rf3 <- make_formula_frst_stg("raw_lang", "E_is", added = "entype_r4 +")
f_rf4 <- make_formula_frst_stg("hghgrade_final_num", "E_is", added = "entype_r4 +")
f_rf5 <- make_formula_frst_stg("wage_employII", "E_is", added = "entype_r4 +")
f_rf6 <- make_formula_frst_stg("std_maths", "E_is", added = "entype_r4 +")
f_rf7 <- make_formula_frst_stg("std_lang", "E_is", added = "entype_r4 +")

rf1aa <- lm(f_rf1, data = aa_samp, subset = (type_activ != 19))
rf2aa <- lm(f_rf2, data = aa_samp, subset = (type_activ != 19))
rf3aa <- lm(f_rf3, data = aa_samp, subset = (type_activ != 19))
rf4aa <- lm(f_rf4, data = aa_samp, subset = (type_activ != 19))
rf5aa <- lm(f_rf5, data = aa_samp, subset = (type_activ != 19))
rf6aa <- lm(f_rf6, data = aa_samp, subset = (type_activ != 19))
rf7aa <- lm(f_rf7, data = aa_samp, subset = (type_activ != 19))

stargazer(
  # rf2aa, rf3aa, 
  rf6aa, rf7aa,
  rf5aa ,rf1aa, 
  keep = c("E_is"),
  type = "text",
  keep.stat = c("n","rsq")
)

# IV estimation:
make_formula_iv <- function(
  dep_var, instrument = 0, added = NULL
  ) {
  # Define a vector of covariates
  covar1 <- c(
    "chsex", "zbfa", "stunting", "caredu_r1",
    "careage_r1", "factor(caresex_r1)", "hhsize", 
    "wi_new", "hq_new", "cd_new", "elecq_new", 
    "ownlandhse_r1", "factor(foodsec_r3)"#, 
    # "entype_r4"  # only for the AA sample (use separate formula)
  )
  covar2 <- 0
  
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )
  
  if (instrument == 0) {
    f <- as.formula(paste(dep_var, " ~ IMTI + ", added, covar))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", added, covar, 
      " | (IMTI ~ ", instrument, ")"
    ))
  }
  
  return(f)
}

fiv1 <- make_formula_iv("wage_employ", "E_is", added = NULL)
fiv2 <- make_formula_iv("self_employ", "E_is", added = NULL)
fiv3 <- make_formula_iv("raw_maths", "E_is", added = NULL)
fiv4 <- make_formula_iv("raw_lang", "E_is", added = NULL)
fiv5 <- make_formula_iv("hghgrade_final_num", "E_is", added = NULL)
fiv6 <- make_formula_iv("wage_employII", "E_is", added = NULL)
fiv7 <- make_formula_iv("self_employII", "E_is", added = NULL)
fiv8 <- make_formula_iv("std_maths", "E_is", added = NULL)
fiv9 <- make_formula_iv("std_lang", "E_is", added = NULL)


# Estimation for the Non-AA sample
iv1 <- felm(fiv1, data = non_aa_samp)
iv2 <- felm(fiv2, data = non_aa_samp)
iv3 <- felm(fiv3, data = non_aa_samp)
iv4 <- felm(fiv4, data = non_aa_samp)
iv5 <- felm(fiv5, data = non_aa_samp)
iv6 <- felm(fiv6, data = non_aa_samp)
iv7 <- felm(fiv7, data = non_aa_samp)
iv8 <- felm(fiv8, data = non_aa_samp)
iv9 <- felm(fiv9, data = non_aa_samp)

stargazer(
  iv8, iv9,
  # iv3, iv4,
  iv6, iv1,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)

# Estimation excluding the Amhara sample
iv1_nonAm <- felm(fiv1, data = non_aa_samp %>% filter(region != "Amhara"))
iv2_nonAm <- felm(fiv2, data = non_aa_samp %>% filter(region != "Amhara"))
iv3_nonAm <- felm(fiv3, data = non_aa_samp %>% filter(region != "Amhara"))
iv4_nonAm <- felm(fiv4, data = non_aa_samp %>% filter(region != "Amhara"))
iv5_nonAm <- felm(fiv5, data = non_aa_samp %>% filter(region != "Amhara"))
iv6_nonAm <- felm(fiv6, data = non_aa_samp %>% filter(region != "Amhara"))
iv7_nonAm <- felm(fiv7, data = non_aa_samp %>% filter(region != "Amhara"))
iv8_nonAm <- felm(fiv8, data = non_aa_samp %>% filter(region != "Amhara"))
iv9_nonAm <- felm(fiv9, data = non_aa_samp %>% filter(region != "Amhara"))

# Estimation for the AA sample
fiv1aa <- make_formula_iv("wage_employ", "E_is", added = "entype_r4 +")
fiv2aa <- make_formula_iv("self_employ", "E_is", added = "entype_r4 +")
fiv3aa <- make_formula_iv("raw_maths", "E_is", added = "entype_r4 +")
fiv4aa <- make_formula_iv("raw_lang", "E_is", added = "entype_r4 +")
fiv5aa <- make_formula_iv("hghgrade_final_num", "E_is", added = "entype_r4 +")
fiv6aa <- make_formula_iv("wage_employII", "E_is", added = "entype_r4 +")
fiv7aa <- make_formula_iv("self_employII", "E_is", added = "entype_r4 +")
fiv8aa <- make_formula_iv("std_maths", "E_is", added = "entype_r4 +")
fiv9aa <- make_formula_iv("std_lang", "E_is", added = "entype_r4 +")

iv1aa <- felm(fiv1aa, data = aa_samp, subset = (type_activ != 19))
# iv2aa <- felm(fiv2, data = aa_samp)  # has rank problems
iv3aa <- felm(fiv3aa, data = aa_samp, subset = (type_activ != 19))
iv4aa <- felm(fiv4aa, data = aa_samp, subset = (type_activ != 19))
iv5aa <- felm(fiv5aa, data = aa_samp, subset = (type_activ != 19))
iv6aa <- felm(fiv6aa, data = aa_samp, subset = (type_activ != 19))
iv7aa <- felm(fiv7aa, data = aa_samp, subset = (type_activ != 19))
iv8aa <- felm(fiv8aa, data = aa_samp, subset = (type_activ != 19))
iv9aa <- felm(fiv9aa, data = aa_samp, subset = (type_activ != 19))

stargazer(
  iv8aa, iv9aa,
  # iv3aa, iv4aa,
  iv6aa, iv1aa,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)


# Robustness checks ####

non_aa_samp %>% 
  count(chethnic, region, sort = TRUE) %>% 
  filter(chethnic == "Amhara")

# sample size issue
# * only 39 Amharas not in Amhara
# The others evan less.

# Focus on Amharas

non_aa_samp %>% 
  filter( 
    chethnic == "Amhara" & region != "Amhara"
    )
  

# Robustness check:

orom.tig <- non_aa_samp %>% 
  filter(region %in% c("Oromia", "Tigray")) %>% 
  mutate( 
    Tigray = (region == "Tigray"),
    
    match = case_when(
      region == "Tigray" & testlang_lang == "Tigrigna" & chlang == "Tigrigna"  ~ 1,
      region == "Oromia" & testlang_lang == "Afaan Oromo" & chlang == "Afaan Oromo" ~ 1,
      TRUE ~ 0
    )
      
  )

f_rf1 <- make_formula_frst_stg("wage_employ", "Tigray"
            , added = "IMTI + ownlandhse_r1 + rural_3 +"
                               )
f_rf2 <- make_formula_frst_stg("raw_maths", "Tigray"
            , added = "IMTI + ownlandhse_r1 + rural_3 +"
                               )
f_rf3 <- make_formula_frst_stg("raw_lang", "Tigray"
            , added = "IMTI + ownlandhse_r1 + rural_3 +"
                               )
f_rf4 <- make_formula_frst_stg("hghgrade_final_num", "Tigray"
                               , added = "IMTI + ownlandhse_r1 + rural_3 +"
                               )
f_rf5 <- make_formula_frst_stg("wage_employII", "Tigray"
                               , added = "IMTI + ownlandhse_r1 + rural_3 +"
                               )
f_rf6 <- make_formula_frst_stg("std_maths", "Tigray"
                               , added = "IMTI + ownlandhse_r1 + rural_3 +"
)
f_rf7 <- make_formula_frst_stg("std_lang", "Tigray"
                               , added = "IMTI + ownlandhse_r1 + rural_3 +"
)

# Tigray vs. Oromia
rf1ot <- lm(f_rf1, data = orom.tig)
rf2ot <- lm(f_rf2, data = orom.tig)
rf3ot <- lm(f_rf3, data = orom.tig, subset = (match == 1) )
rf4ot <- lm(f_rf4, data = orom.tig)
rf5ot <- lm(f_rf5, data = orom.tig)
rf6ot <- lm(f_rf6, data = orom.tig)
rf7ot <- lm(f_rf7, data = orom.tig, subset = (match == 1) )

stargazer(
  # rf2ot, rf3ot,
  rf6ot, rf7ot,
  rf5ot ,rf1ot, 
  keep = c("T"),
  keep.stat = c("n","rsq"),
  type = "text"
)



# orom.snnp <- non_aa_samp %>% 
#   filter(region %in% c("Oromia", "SNNP")) %>% 
#   mutate( T = (region == "Oromia") )
# 
# orom.amh <- non_aa_samp %>% 
#   filter(region %in% c("Oromia", "Amhara")) %>% 
#   mutate( T = (region == "Amhara") )
# 
# # SNNP vs. Oromia
# rf1os <- lm(f_rf1, data = orom.snnp)
# rf2os <- lm(f_rf2, data = orom.snnp)
# rf3os <- lm(f_rf3, data = orom.snnp)
# rf4os <- lm(f_rf4, data = orom.snnp)
# rf5os <- lm(f_rf5, data = orom.snnp)
# 
# stargazer(
#   rf2os, rf3os, rf4os, rf5os ,rf1os, 
#   keep = c("T"),
#   keep.stat = c("n","rsq"),
#   type = "text"
# )

# Inactive exploratory Amhara/Oromia comparison (not used by 06_export_res.R).
# Its dataset construction above is commented out, and the current f_rf*
# formulas target Tigray. Keep this block inactive until it is specified
# explicitly; it is not part of the manuscript results.
# # Amhara vs. Oromia
# rf1oa <- lm(f_rf1, data = orom.amh)
# rf2oa <- lm(f_rf2, data = orom.amh)
# rf3oa <- lm(f_rf3, data = orom.amh)
# rf4oa <- lm(f_rf4, data = orom.amh)
# rf5oa <- lm(f_rf5, data = orom.amh)
#
# stargazer(
#   rf2oa, rf3oa, rf4oa, rf5oa ,rf1oa,
#   keep = c("T"),
#   keep.stat = c("n","rsq"),
#   type = "text"
# )
#
# Dist. of IMTI (Oromia vs. Tigray)

orom.tig %>% 
  count(region, IMTI) %>% 
  ggplot(aes(IMTI, n)) +
  geom_col() +
  facet_wrap(~region)

orom.tig %>% 
  ggplot(aes(raw_lang)) +
  geom_histogram() +
  facet_wrap(~region)

orom.tig %>% 
  ggplot(aes(hghgrade_final_num)) +
  geom_histogram() +
  facet_wrap(~region)

orom.tig %>%
  count(region, childloc_r4) %>% 
  ggplot(aes(factor(childloc_r4), n)) +
  geom_col() +
  facet_wrap(~region)

# could use this as a descriptive presentation: 
non_aa_samp %>% select(region, testlang_lang, chlang, raw_lang) %>% 
  count(testlang_lang, chlang, sort = TRUE)

# 
oromia <- orom.tig %>% 
  select(region, testlang_lang, raw_lang, chlang) %>% 
  filter(region == "Oromia") %>% 
  mutate( match = case_when(
    as.character(testlang_lang) == as.character(chlang)  ~ 1, TRUE ~ 0
  ))

lm(raw_lang ~ match, data = oromia) %>% summary()


# Analyzing joined younger cohort data: ####

schsur_yl_joined <- read_csv("data/schsur_yl_joined.csv")

yl_yc <- read_csv("data/ylsample_yc.csv")

schsur_yl_aa <- schsur_yl_joined %>% 
  filter(region == "Addis Ababa") %>% 
  mutate(std_math = standardize(math_score),
         std_verbal = standardize(verbal_score),
         std_math_schsurv = standardize(maths_score_w2 ),
         std_literacy_schsurv = standardize(literacy_score_w2))

yl_yc_aa <- yl_yc %>% 
  filter(region == "Addis Ababa") %>% 
  mutate(std_math = standardize(math_score),
         std_verbal = standardize(verbal_score))

# Estimation for the AA sample

iv3aa_yc <- felm(
  formula = maths_score_w2 ~ entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new + 
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3) | 
    0 | (IMTI ~ E_is), 
  
  data = schsur_yl_aa
  )


iv4aa_yc <- felm(
  formula = literacy_score_w2 ~ entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new + 
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3) | 
    0 | (IMTI ~ E_is), 
  
  data = schsur_yl_aa
  )

stargazer(
  iv3aa_yc, iv4aa_yc,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)

stargazer(
  iv3aa_yc$stage1, iv4aa_yc$stage1, 
  keep = c("E_is"),
  keep.stat = c("n"),
  type = "text"
)

# using yl cognitive scores:
iv5aa_yc <- felm(
  formula = math_score ~ entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new +
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3) | 
    0 | (IMTI ~ E_is), 
  
  data = filter(yl_yc_aa, std_math < 2.25)
)


iv6aa_yc <- felm(
  formula = verbal_score ~ entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new + 
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3) | 
    0 | (IMTI ~ E_is), 
  
  data = filter(yl_yc_aa, std_math < 2.25)
)


stargazer(
  iv5aa_yc, iv6aa_yc,
  keep = c("IMTI"),
  keep.stat = c("n","rsq"),
  type = "text"
)

stargazer(
  iv5aa_yc$stage1, iv6aa_yc$stage1, 
  keep = c("E_is"),
  keep.stat = c("n"),
  type = "text"
)


# Reduced form

iv5aa_yc_rf <- felm(
  formula = std_math ~ E_is + entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new +  
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3), 
  
  data = filter(yl_yc_aa, std_math < 2.25)
)

iv6aa_yc_rf <- felm(
  formula = std_verbal ~ E_is + entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new +  
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3), 
  
  data = filter(yl_yc_aa, std_math < 2.25)
)

stargazer(
  iv5aa_yc_rf, iv6aa_yc_rf,
  keep = c("E_is"),
  keep.stat = c("n","rsq"),
  type = "text"
)

# using school survey test score

iv3aa_yc_rf <- felm(
  formula = std_math_schsurv ~ E_is + entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new + 
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3), 
  
  data = schsur_yl_aa
)

iv4aa_yc_rf <- felm(
  formula = std_literacy_schsurv ~ E_is + entype_r4 + chsex + zbfa + stunting + caredu_r1 + 
    careage_r1 + hhsize + wi_new + hq_new + 
    cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3), 
  
  data = schsur_yl_aa
)

stargazer(
  iv3aa_yc_rf, iv4aa_yc_rf,
  keep = c("E_is"),
  keep.stat = c("n","rsq"),
  type = "text"
)


ggplot(yl_yc_aa, aes(std_math)) +
  geom_histogram(fill = "blue", alpha = .5) +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  labs(x = "Standardized Math Score",
       y = "Count")

ggplot(yl_yc_aa, aes(std_verbal)) +
  geom_histogram() +
  scale_x_continuous(breaks = -3:3)

ggplot(yl_yc_aa, aes(std_math)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5))

yl_yc_aa %>% 
  filter(std_math > 3)
  
yl_yc_aa %>% 
  filter(std_math < 2.25) %>% 
  ggplot(aes(std_math)) +
  geom_histogram() +
  geom_histogram(data = filter(yl_yc_aa, std_math > 2.25)) +
  scale_x_continuous(breaks = -3:3)  
  
  











