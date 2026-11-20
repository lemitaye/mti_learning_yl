# Common eligibility for Table 1, Table 2 and pooled robustness.
lean_terms <- c("chsex", "stunting", "caredu_r1", "hhsize", "wi_new")

baseline_reduced_terms <- c(
  lean_terms, "hq_new",
  "cd_new", "elecq_new", "factor(foodsec_r3)"
)

fixed_employment_sample <- function() {
  required <- all.vars(reformulate(c(
    lean_terms, "salary_employ", "wage_employ", "IMTI", "E_is",
    "round7", "cohort_young", "region", "child_id"
  )))
  data <- readRDS("data/r7/r5_r7_appended.rds") |>
    dplyr::mutate(dplyr::across(
      where(is.numeric), ~ as.numeric(haven::zap_labels(.x))
    )) |>
    dplyr::filter(
      round == 7 | yc == 0,
      yc == 1 | (!is.na(raw_lang_r4) & raw_lang_r4 != 0)
    ) |>
    tidyr::drop_na(dplyr::all_of(required))
  stopifnot(!anyDuplicated(data[c("child_id", "round")]))
  data
}

# The revised main-results and test-score tables use these same panel designs.
main_results_panels <- function(data) {
  data <- dplyr::filter(data, region != "Addis Ababa")
  list(
    A = list(data = dplyr::filter(data, round == 5, yc == 0),
             terms = lean_terms, title = "Round 5: older cohort"),
    B = list(data = data, terms = c(lean_terms, "round7", "cohort_young"),
             title = "Rounds 5 and 7: both cohorts")
  )
}

fit_main_results_iv <- function(data, outcome, terms) {
  controls <- paste(terms, collapse = " + ")
  first_stage <- lm(reformulate(c("E_is", terms), "IMTI"), data = data,
                    na.action = na.fail)
  model <- lfe::felm(as.formula(paste(
    outcome, "~", controls, "| 0 | (IMTI ~ E_is)"
  )), data = data, na.action = na.fail)
  stopifnot(model$N == nobs(first_stage), model$N == nrow(data))
  list(
    model = model,
    first_stage = first_stage,
    fs = coef(summary(first_stage))["E_is", ],
    iv = coef(summary(model))["`IMTI(fit)`", ]
  )
}
