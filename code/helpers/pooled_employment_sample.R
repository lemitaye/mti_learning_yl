# One shared sample rule for pooled data, summary statistics and regressions.
# R5 uses the first-ranked slot; R7 slot1 is the annual main activity.
classify_pooled_employment <- function(data) {
  data |>
    dplyr::mutate(
      employment_group = dplyr::case_when(
        round == 5 & yc == 1 ~ "outcome_unavailable",
        round == 5 & r5_interview_observed == 0 ~ "not_interviewed",
        active == 0 ~ "no_activity",
        type_activ_1 %in% c(1:4, 8:11) ~ "self_employed",
        type_activ_1 %in% c(5:7, 12, 19) ~ "wage_employed",
        type_activ_1 == 13 ~ "salary_employed",
        type_activ_1 %in% c(15, 20) ~ "other_work",
        TRUE ~ "activity_unknown"
      ),
      pooled_sample_eligible = employment_group %in% c(
        "self_employed", "wage_employed", "salary_employed", "no_activity"
      )
    )
}

# Retain original indicators and select a clearly documented analysis basis.
# Both wage variants combine codes5-7 with12/19, excluding salary13.
prepare_pooled_outcomes <- function(data, basis = c("any", "ranked")) {
  basis <- match.arg(basis)
  data <- data |>
    dplyr::mutate(
      inactive = as.integer(employment_group == "no_activity"),
      wage_nonfarm_original = wage_employ,
      salary_original = salary_employ,
      self_original = self_employ,
      wage_combined_any = as.integer(wage_employ == 1 | wage_agri == 1),
      wage_combined_ranked = as.integer(employment_group == "wage_employed"),
      salary_ranked = as.integer(employment_group == "salary_employed"),
      self_ranked = as.integer(employment_group == "self_employed")
    )
  if (basis == "ranked") {
    data$wage_employ <- data$wage_combined_ranked
    data$salary_employ <- data$salary_ranked
    data$self_employ <- data$self_ranked
    for (variable in c("self_agri", "wage_agri", "self_nonag")) {
      data[[paste0(variable, "_original")]] <- data[[variable]]
    }
    data$self_agri <- as.integer(data$type_activ_1 %in% 1:4)
    data$wage_agri <- as.integer(data$type_activ_1 %in% 5:7)
    data$self_nonag <- as.integer(data$type_activ_1 %in% 8:11)
    data$outcome_coverage <- dplyr::if_else(
      data$round == 5, "first_ranked_activity_only", "annual_main_activity_only"
    )
  } else {
    data$wage_employ <- data$wage_combined_any
  }
  for (variable in c("wage_employ", "salary_employ", "self_employ",
                     "self_agri", "wage_agri", "self_nonag", "inactive")) {
    data[[variable]] <- haven::labelled(
      as.numeric(data[[variable]]), c(No = 0, Yes = 1),
      label = paste(variable, "farm/nonfarm where applicable;", basis,
                    "activity basis in R5, annual main in R7")
    )
  }
  data$outcome_definition_version <- paste0("selected_sample_combined_", basis)
  data$regression_activity_basis <- basis
  tibble::as_tibble(data)
}
