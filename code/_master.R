########################################################################~
#
# Project: Mother Tongue Instruction, Learning and Employment (Ethiopia)
# Owners : Teferi Mergo (PhD) and Lemi Daba
#
# MASTER SCRIPT — runs the manuscript analysis pipeline.
# See README.md for export destinations and reproduction limitations.
#
# HOW TO RUN (from the project root)
#   R console: source("code/_master.R")
#   Terminal:  Rscript code/_master.R
#
# Missing packages are installed automatically (internet access and a writable
# R library are required). Existing packages are not deliberately upgraded.
# This script clears workspace objects and overwrites processed data/results,
# including files mirrored to Overleaf. Use a fresh R session for a full run.
########################################################################~

# Exports also mirror tables/figures to the Overleaf Dropbox project.
# Override MTI_OVERLEAF_DIR, or set it to "" to keep exports local only.

# ---- 0. Session and project setup -------------------------------------------
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
    "04_export_paths.R", "05_estimation.R", "06_export_res.R",
    "07_table_main.R", "08_cluster_inference.R", "09_table_rev.R"
  ))
  required_paths <- c(
    "README.md", "rawdata", pipeline_scripts,
    "temp/raw_data_catalog.R", "temp/inventory_round7.R"
  )
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths)) {
    stop(
      "Run from the project root with the raw inputs available. Missing paths: ",
      paste(missing_paths, collapse = ", "), call. = FALSE
    )
  }

  # Check legacy replication inputs and newly added Round 7 resources.
  source("temp/raw_data_catalog.R", local = TRUE)
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

# WORKSPACE DEPENDENCY: 03_descriptives.R and 05_estimation.R each run rm(list = ls())
# and reload their inputs. 06_export_res.R consumes 05_estimation.R's model objects;
# keep them adjacent and source them in the same R session.

# ---- 1. Build analysis data (raw .dta -> data/*.csv) ------------------------
message("Step 1/4: building analysis datasets.")
source("code/01_extract_clean.R", echo = TRUE, max = 1000)  # -> data/aa_samp.csv, data/non_aa_samp.csv  (older cohort)
source("code/02_cleaning_yc.R",   echo = TRUE, max = 1000)  # -> data/ylsample_yc.csv, data/schsur_yl_joined.csv (younger cohort)

# Inventory new releases separately; they do not enter the current models.
source("temp/inventory_round7.R")

# ---- 2. Descriptives: Table 1 + Figure 1 ------------------------------------
message("Step 2/4: generating descriptive tables and figures.")
source("code/03_descriptives.R",  echo = TRUE, max = 1000)  # -> output/tables/tableI.tex, output/figures/employ_gr.pdf

# ---- 3. Estimation + regression tables --------------------------------------
message("Step 3/4: estimating models and exporting regression tables.")
source("code/05_estimation.R",    echo = TRUE, max = 1000)  # fits all models into the workspace
source("code/06_export_res.R",    echo = TRUE, max = 1000)  # -> staged tables, including tableII_mod.tex with IV diagnostics

# ---- 4. Table 3: "Likely Channels" (local output and Dropbox) ------
message("Step 4/4: regenerating manuscript Table 3.")
source("code/09_table_rev.R",     echo = TRUE, max = 1000)  # -> output/tables/table_rev.tex and Dropbox tables/

# ---- Done -------------------------------------------------------------------
# Results remain in output/ and mirror to the revision Overleaf project unless
# MTI_OVERLEAF_DIR is empty. The original manuscript project is protected.
message("Pipeline complete. Review generated results in output/ and the revision project.")
