# R7 helpers: narrow tasks reused by the standalone build.
# These functions do not change the production R1-R5 scripts or results.

# Recover historical lookup rows excluded by the old death-status filter.
# The extraction runs in a temporary project with external exports disabled.
recover_r7_history <- function(root, audit_dir) {
  sandbox <- tempfile("r7_history_lookup_")
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  stopifnot(
    file.copy("code", sandbox, recursive = TRUE),
    file.symlink(file.path(root, "rawdata"), file.path(sandbox, "rawdata"))
  )
  dir.create(file.path(sandbox, "data"))
  for (script in c("01_extract_clean.R", "02_cleaning_yc.R")) {
    path <- file.path(sandbox, "code", script)
    text <- readLines(path)
    needle <- paste0("filter(yc == ", if (script == "01_extract_clean.R") "0" else "1", ", is.na(deceased))")
    stopifnot(sum(grepl(needle, text, fixed = TRUE)) == 1L)
    text <- gsub(needle, paste0("filter(yc == ", if (script == "01_extract_clean.R") "0" else "1", ")"), text, fixed = TRUE)
    writeLines(text, path)
  }
  runner <- c(
    "suppressPackageStartupMessages(library(tidyverse))",
    "suppressPackageStartupMessages(library(haven))", "Sys.setenv(MTI_OVERLEAF_DIR='')",
    "source('code/01_extract_clean.R')", "write.csv(analysis_data,'data/full_oc.csv',row.names=FALSE)",
    "source('code/02_cleaning_yc.R')", "write.csv(analysis_data,'data/full_yc.csv',row.names=FALSE)"
  )
  writeLines(runner, file.path(sandbox, "lookup.R"))
  prior_wd <- getwd()
  setwd(sandbox)
  status <- system2(file.path(R.home("bin"), "Rscript"), "lookup.R",
    stdout = file.path(root, audit_dir, "historical_lookup.log"),
    stderr = file.path(root, audit_dir, "historical_lookup.log")
  )
  setwd(prior_wd)
  stopifnot(status == 0L)
  recovered_oc <- read.csv(file.path(sandbox, "data/full_oc.csv"))
  recovered_oc$yc <- 0L
  recovered_yc <- read.csv(file.path(sandbox, "data/full_yc.csv"))
  recovered_yc$yc <- 1L
  recovered <- bind_rows(recovered_oc, recovered_yc)
  stopifnot(!anyDuplicated(recovered$child_id))
  recovered
}

# Provisional exposure: original region/language caps with updated schooling.
# Keep the historical spelling "Amaharic" to preserve the original rule.
# This candidate still requires substantive validation before YC estimation.
calculate_mti_candidate <- function(history, school_grade) {
  history |>
    mutate(
      school_grade = school_grade,
      mti_candidate = case_when(
        region == "Tigray" & chlang == "Tigrigna" & lang_primary == "Tigrigna" ~ pmin(school_grade, 8),
        region == "Oromia" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" ~ pmin(school_grade, 8),
        region == "Oromia" & chlang == "Amaharic" & lang_primary == "Amharic" ~ pmin(school_grade, 8),
        region == "Amhara" & chlang == "Amharic" & lang_primary == "Amharic" ~ pmin(school_grade, 6),
        region == "Amhara" & chlang == "Afaan Oromo" & lang_primary == "Afaan Oromo" ~ pmin(school_grade, 8),
        region == "SNNP" & chlang == lang_primary & chlang %in% c("Sidamigna", "Wolayta", "Hadiya", "Amharic") ~ pmin(school_grade, 4),
        region == "Addis Ababa" & chethnic == "Amhara" & lang_primary == "Amharic" ~ pmin(school_grade, 6),
        TRUE ~ 0
      )
    ) |>
    pull(mti_candidate)
}
