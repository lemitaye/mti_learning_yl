

non_aa_samp <- non_aa_samp %>% 
  mutate(
    yrs = case_when(
    region %in% c("Oromia", "Tigray") ~ 8,
    region == "Amhara" ~ 6,
    region == "SNNP" ~ 4
  ),
  math_score_imti = std_maths * IMTI,
  lang_score_imti = std_lang * IMTI,
  )

# IV estimation:
make_formula_iv2 <- function(
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

fiv1 <- make_formula_iv2("wage_employ", "E_is", added = NULL)
fiv2 <- make_formula_iv2("wage_employII", "E_is", added = NULL)


# Estimation for the Non-AA sample
iv1 <- felm(fiv1, data = non_aa_samp)
iv2 <- felm(fiv2, data = non_aa_samp)


stargazer(
  iv1, iv2,
  # keep = c("std_maths"),
  keep.stat = c("n","rsq"),
  type = "text"
)

stargazer(
  iv1, iv2,
  # keep = c("std_maths"),
  keep.stat = c("n","rsq"),
  type = "text"
)


# Estimation for the Non-AA sample (excluding Amhara)
iv11 <- felm(fiv1, data = filter(non_aa_samp, region != "Amhara"))
iv22 <- felm(fiv2, data = filter(non_aa_samp, region != "Amhara"))


stargazer(
  iv11, iv22,
  # keep = c("std_maths"),
  keep.stat = c("n","rsq"),
  type = "text"
)





fiv3 <- make_formula_iv2("wage_employ", "E_is", added = "std_maths + ")
fiv4 <- make_formula_iv2("wage_employ", "E_is", added = "std_lang + ")
fiv5 <- make_formula_iv2("wage_employII", "E_is", added = "std_maths + ")
fiv6 <- make_formula_iv2("wage_employII", "E_is", added = "std_lang + ")


# Estimation for the Non-AA sample
iv3 <- felm(fiv3, data = non_aa_samp)
iv4 <- felm(fiv4, data = non_aa_samp)
iv5 <- felm(fiv5, data = non_aa_samp)
iv6 <- felm(fiv6, data = non_aa_samp)


stargazer(
  iv3, iv4, 
  # keep = c("std_maths"),
  keep.stat = c("n","rsq"),
  type = "text"
)

stargazer(
  iv5, iv6,
  # keep = c("std_maths"),
  keep.stat = c("n","rsq"),
  type = "text"
)


























