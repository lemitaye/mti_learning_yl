# Shared preparation for descriptive statistics and model estimation.
# Factors retain alphabetical observed levels, matching CSV reloads in the
# original pipeline. The language-score filter is applied before standardizing.
read_analysis_sample <- function(path) {
  sample_data <- if (tolower(tools::file_ext(path)) == "rds") {
    readRDS(path)
  } else {
    read_csv(path, show_col_types = FALSE)
  }
  sample_data %>%
    mutate(across(where(is.character), as.factor)) %>%
    filter(raw_lang != 0)
}

# Standardize within the supplied sample, using available observations only.
standardize_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
