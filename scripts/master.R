########################################################################~
#
# Project: Mother Tongue Instruction, Learning and Employment (Ethiopia)
# Owners : Teferi Mergo (PhD) and Lemi Daba
#
# MASTER SCRIPT — runs the analysis pipeline for the manuscript in
# MTI__learning_and_job_outcomes/. See README.md for reproduction limitations.
#
# HOW TO RUN (from the project root)
#   R console: source("scripts/master.R")
#   Terminal:  Rscript scripts/master.R
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
  pipeline_scripts <- file.path("scripts", c(
    "extract_clean.R", "cleaning_yc.R", "raw_data_catalog.R", "inventory_round7.R", "descriptives.R",
    "estimation.R", "export_res.R", "export_paths.R", "table_main.R", "cluster_inference.R", "table_rev.R"
  ))
  required_paths <- c(
    "mti_learning_yl.Rproj", "raw_data",
    pipeline_scripts
  )
  missing_paths <- required_paths[!file.exists(required_paths)]
  if (length(missing_paths)) {
    stop(
      "Run from the project root with the raw inputs available. Missing paths: ",
      paste(missing_paths, collapse = ", "), call. = FALSE
    )
  }

  # Check legacy replication inputs and newly added Round 7 resources.
  source("scripts/raw_data_catalog.R", local = TRUE)
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

# WORKSPACE DEPENDENCY: descriptives.R and estimation.R each run rm(list = ls())
# and reload their inputs. export_res.R consumes estimation.R's model objects;
# keep them adjacent and source them in the same R session.

# ---- 1. Build analysis data (raw .dta -> data/*.csv) ------------------------
message("Step 1/4: building analysis datasets.")
source("scripts/extract_clean.R", echo = TRUE, max = 1000)  # -> data/aa_samp.csv, data/non_aa_samp.csv  (older cohort)
source("scripts/cleaning_yc.R",   echo = TRUE, max = 1000)  # -> data/ylsample_yc.csv, data/schsur_yl_joined.csv (younger cohort)

# Inventory new releases separately; they do not enter the current models.
source("scripts/inventory_round7.R")

# ---- 2. Descriptives: Table 1 + Figure 1 ------------------------------------
message("Step 2/4: generating descriptive tables and figures.")
source("scripts/descriptives.R",  echo = TRUE, max = 1000)  # -> output/tables/tableI.tex, output/figures/employ_gr.pdf

# ---- 3. Estimation + regression tables --------------------------------------
message("Step 3/4: estimating models and exporting regression tables.")
source("scripts/estimation.R",    echo = TRUE, max = 1000)  # fits all models into the workspace
source("scripts/export_res.R",    echo = TRUE, max = 1000)  # -> staged tables, including tableII_mod.tex with IV diagnostics

# ---- 4. Table 3: "Likely Channels" (local output and Dropbox) ------
message("Step 4/4: regenerating manuscript Table 3.")
source("scripts/table_rev.R",     echo = TRUE, max = 1000)  # -> output/tables/table_rev.tex and Dropbox tables/

# ---- Done -------------------------------------------------------------------
# Generated tables/figures land in output/. Review them, then copy the ones the
# manuscript uses into MTI__learning_and_job_outcomes/tables|figures and compile
# main.tex. See README.md for the table-number -> file mapping and the
# reproduction-status caveats (only table_rev.R is byte-verified against the
# current manuscript; export_res.R is an earlier vintage — see README).

message("Pipeline complete. Review generated results before updating the manuscript.")
