# Add metadata to the RDS after the aggregate dictionary has been written.
# Values and missingness are preserved; CSV remains a plain interchange file.
label_r7_cross_section <- function(data, audit_dir) {
  dictionary <- read.csv(file.path(audit_dir, "variable_dictionary.csv"))
  codes <- read.csv(file.path(audit_dir, "source_value_labels.csv"))
  data <- tibble::as_tibble(data)

  for (variable in names(data)) {
    description <- dictionary$definition[
      match(variable, dictionary$variable)
    ]
    if (is.na(description) || description == variable) {
      description <- gsub("_", " ", variable)
    }

    # Match archived codes only to fields representing the same categories.
    source <- sub("_(raw|clean)$", "", variable)
    source <- sub("_raw_r7$", "", source)
    source <- switch(variable,
      activity_annual_raw_r7 = "typwrk12r7",
      activity_annual_r7 = "typwrk12r7",
      activity_weekly_job_raw_r7 = "typwrk7r7",
      activity_weekly_job_r7 = "typwrk7r7",
      qualification_code_r7 = "lstqualr7",
      qualification_code_raw_r7 = "lstqualr7",
      foodsec_r7 = "fdsec00r7",
      foodsec_raw_r7 = "fdsec00r7",
      electricity_r7 = "electr7",
      electricity_raw_r7 = "electr7",
      source
    )
    labels <- unique(codes[codes$variable == source, c("code", "label")])
    stopifnot(!anyDuplicated(labels$code))
    value_labels <- setNames(labels$code, labels$label)
    if (variable == "yc") {
      value_labels <- c("Older cohort" = 0, "Younger cohort" = 1)
    }
    binary <- grepl(
      "^(worked_|salary_|wage_|self_employment_|nonfarm_self_employment_)",
      variable
    ) || variable %in% c("ownhouse_r7", "public_sector_annual_r7")
    if (binary && !grepl("_hist$", variable)) {
      value_labels <- c("No" = 0, "Yes" = 1)
    }
    if (is.logical(data[[variable]])) {
      attr(data[[variable]], "labels") <- c("No" = FALSE, "Yes" = TRUE)
    } else if (is.numeric(data[[variable]]) && length(value_labels)) {
      data[[variable]] <- haven::labelled(
        data[[variable]], labels = value_labels
      )
    }
    attr(data[[variable]], "label") <- description
  }
  data
}
