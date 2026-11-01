# Shared preparation for descriptive statistics and model estimation.
# Factors retain alphabetical observed levels, matching CSV reloads in the
# original pipeline. The language-score filter is applied before standardizing.
read_analysis_sample <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(across(where(is.character), as.factor)) %>%
    filter(raw_lang != 0)
}

# Standardize within the supplied sample, using available observations only.
standardize_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
