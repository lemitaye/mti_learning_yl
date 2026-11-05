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
