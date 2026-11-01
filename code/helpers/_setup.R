# Session, project paths and package setup. Run from the repository root.
rm(list = ls())

# Keep setup variables private: downstream scripts clear the shared workspace.
# Packages and session options remain available after those workspace resets.
local({
  if (getRversion() < "4.0.0") {
    stop("This pipeline requires R >= 4.0.0.", call. = FALSE)
  }

  # Fail early if launched from the wrong directory or an incomplete checkout.
  pipeline_scripts <- file.path("code", c(
    "01_extract_clean.R", "02_cleaning_yc.R", "03_descriptives.R",
    "helpers/export_paths.R", "05_estimation.R", "06_export_res.R",
    "07_table_main.R", "helpers/cluster_inference.R", "09_table_rev.R"
  ))
  required_paths <- c(
    "README.md", "rawdata", pipeline_scripts,
    "code/helpers/raw_data_catalog.R", "code/helpers/inventory_round7.R",
    "code/helpers/extraction_helpers.R", "code/helpers/analysis_helpers.R"
  )
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths)) {
    stop(
      "Run from the project root with the raw inputs available. Missing paths: ",
      paste(missing_paths, collapse = ", "), call. = FALSE
    )
  }

  # Check legacy replication inputs and newly added Round 7 resources.
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  missing_inputs <- raw_data_files[!file.exists(raw_data_files)]
  if (length(missing_inputs)) stop("Raw inputs missing: ",
    paste(missing_inputs, collapse = ", "), call. = FALSE)

  # Create generated-output folders if this is a fresh checkout.
  for (path in c("data", "output/tables", "output/figures")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop("Could not create output directory: ", path, call. = FALSE)
    }
  }

  # Retain configured repositories; replace R's interactive CRAN placeholder.
  repos <- getOption("repos")
  if (!length(repos)) repos <- c(CRAN = "https://cloud.r-project.org")
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  options(repos = repos, timeout = max(300, getOption("timeout", 60)))

  # CRAN dependencies used across extraction, estimation, and table creation.
  cran_packages <- c(
    "tidyverse",    # wrangling and ggplot2
    "haven",        # Stata inputs
    "scales",       # figure labels
    "stargazer",    # regression tables
    "AER",          # IV diagnostics
    "clubSandwich", # CR2/Satterthwaite inference for few regional clusters
    "lfe",          # felm() estimation
    "fastDummies",  # descriptive-statistic indicators
    "xtable"        # summary-statistics LaTeX
  )
  missing_packages <- function(packages) {
    packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  }
  install_cran <- function(packages) {
    missing <- missing_packages(packages)
    if (length(missing)) {
      message("Installing missing CRAN packages: ", paste(missing, collapse = ", "))
      install.packages(missing, repos = repos)
      # install.packages() may warn rather than stop when an installation fails.
      still_missing <- missing_packages(missing)
      if (length(still_missing)) {
        stop(
          "Package installation failed: ", paste(still_missing, collapse = ", "),
          ". Check the installation messages, network, and R library permissions.",
          call. = FALSE
        )
      }
    }
  }
  install_cran(cran_packages)

  # starpolishr is hosted on GitHub rather than CRAN. Bootstrap remotes only
  # when needed; avoid its interactive dependency-upgrade prompt.
  if (!requireNamespace("starpolishr", quietly = TRUE)) {
    install_cran("remotes")
    message("Installing starpolishr from ChandlerLutz/starpolishr on GitHub.")
    remotes::install_github("ChandlerLutz/starpolishr", upgrade = "never")
    if (!requireNamespace("starpolishr", quietly = TRUE)) {
      stop("GitHub installation of starpolishr failed; see installation messages.",
           call. = FALSE)
    }
  }

  # Attach packages once; the sourced scripts use unqualified function names.
  for (package in c(cran_packages, "starpolishr")) {
    library(package, character.only = TRUE)
  }
  theme_set(theme_light())
  message("Setup complete. Project root: ", normalizePath("."))
})
