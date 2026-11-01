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
source("code/helpers/_setup.R")

# WORKSPACE DEPENDENCY: 03_descriptives.R and 05_estimation.R each run rm(list = ls())
# and reload their inputs. 06_export_res.R consumes 05_estimation.R's model objects;
# keep them adjacent and source them in the same R session.

# ---- 1. Build analysis data (raw .dta -> data/*.csv) ------------------------
message("Step 1/4: building analysis datasets.")
source("code/01_extract_clean.R", echo = TRUE, max = 1000)  # -> data/aa_samp.csv, data/non_aa_samp.csv  (older cohort)
source("code/02_cleaning_yc.R",   echo = TRUE, max = 1000)  # -> data/ylsample_yc.csv, data/schsur_yl_joined.csv (younger cohort)

# Inventory new releases separately; they do not enter the current models.
source("code/helpers/inventory_round7.R")

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
