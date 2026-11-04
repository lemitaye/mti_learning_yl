# Shared preparation for descriptive statistics and model estimation.
# Factors retain alphabetical observed levels, matching CSV reloads in the
# original pipeline. The language-score filter is applied before standardizing.
read_analysis_sample <- function(path) {
  sample_data <- if (tolower(tools::file_ext(path)) == "rds") {
    readRDS(path)
  } else {
    read_csv(path, show_col_types = FALSE)
  }
  prepare_employment_outcomes(sample_data) %>%
    mutate(across(where(is.character), as.factor)) %>%
    filter(raw_lang != 0)
}

# Standardize within the supplied sample, using available observations only.
standardize_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


# Restore the combined self-employment model outcome from the new split.
# A known positive takes precedence; otherwise unknown components stay NA.
prepare_employment_outcomes <- function(data) {
  if (all(c("self_agri", "self_nonag") %in% names(data))) {
    data <- data %>% mutate(
      self_employ = case_when(
        self_agri == 1 | self_nonag == 1 ~ 1,
        self_agri == 0 & self_nonag == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  }
  data
}
