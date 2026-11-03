# Run from the project root: Rscript code/10_build_r7_cross_section.R
# Separate R7 in-person master, both cohorts. Never sources the estimation master.
local({
  stopifnot(requireNamespace("haven", quietly = TRUE))
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  root <- normalizePath(".")
  destination <- "data/r7"
  audit_dir <- "output/r7_cross_section_build"
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  protected <- c(raw_data_files, file.path("data", c("aa_samp.csv", "non_aa_samp.csv",
    "ylsample_yc.csv", "schsur_yl_joined.csv")),
    list.files("output/tables", recursive = TRUE, full.names = TRUE),
    list.files("output/figures", recursive = TRUE, full.names = TRUE))
  protected <- protected[file.exists(protected) & !dir.exists(protected)]
  hash <- tools::md5sum(protected)
  audit <- function(d, f) write.csv(d, file.path(audit_dir, f), row.names = FALSE, na = "")
  # Numeric codes remain in separate *_raw variables; analytic fields use only
  # declared valid domains. Missing/unknown/refused are never converted to no.
  number <- function(x) as.double(x)
  domain <- function(x, values) {x <- number(x); x[!x %in% values] <- NA_real_; x}
  r7_root <- raw_data_files[["round7_ethiopia"]]
  constructed <- haven::read_dta(raw_data_files[["constructed_r1_r7"]])
  constructed <- constructed[constructed$round == 7, ]
  stopifnot(!anyDuplicated(constructed$childid))
  oc <- rbind(read.csv("data/aa_samp.csv"), read.csv("data/non_aa_samp.csv"))
  yc <- read.csv("data/ylsample_yc.csv")
  oc$yc <- 0L; yc$yc <- 1L
  stack <- function(tables) {
    columns <- unique(unlist(lapply(tables, names)))
    do.call(rbind, lapply(tables, function(x) {
      for (v in setdiff(columns, names(x))) x[[v]] <- NA
      x[columns]
    }))
  }
  existing <- stack(list(oc, yc))
  stopifnot(!anyDuplicated(existing$child_id))
  # Recover a historical lookup for R7 respondents excluded by the OLD R5
  # deceased filter. Existing lookup values always take precedence. The legacy
  # scripts themselves and their production CSVs are not changed.
  sandbox <- tempfile("r7_history_lookup_")
  dir.create(sandbox)
  stopifnot(file.copy("code", sandbox, recursive = TRUE),
    file.symlink(file.path(root, "rawdata"), file.path(sandbox, "rawdata")))
  dir.create(file.path(sandbox, "data"))
  for (script in c("01_extract_clean.R", "02_cleaning_yc.R")) {
    path <- file.path(sandbox, "code", script)
    text <- readLines(path)
    needle <- paste0("filter(yc == ", if (script == "01_extract_clean.R") "0" else "1", ", is.na(deceased))")
    stopifnot(sum(grepl(needle, text, fixed = TRUE)) == 1L)
    text <- gsub(needle, paste0("filter(yc == ", if (script == "01_extract_clean.R") "0" else "1", ")"), text, fixed = TRUE)
    writeLines(text, path)
  }
  runner <- c("suppressPackageStartupMessages(library(tidyverse))",
    "suppressPackageStartupMessages(library(haven))", "Sys.setenv(MTI_OVERLEAF_DIR='')",
    "source('code/01_extract_clean.R')", "write.csv(analysis_data,'data/full_oc.csv',row.names=FALSE)",
    "source('code/02_cleaning_yc.R')", "write.csv(analysis_data,'data/full_yc.csv',row.names=FALSE)")
  writeLines(runner, file.path(sandbox, "lookup.R"))
  prior_wd <- getwd(); setwd(sandbox)
  status <- system2(file.path(R.home("bin"), "Rscript"), "lookup.R",
    stdout = file.path(root, audit_dir, "historical_lookup.log"),
    stderr = file.path(root, audit_dir, "historical_lookup.log"))
  setwd(prior_wd)
  stopifnot(status == 0L)
  recovered_oc <- read.csv(file.path(sandbox, "data/full_oc.csv")); recovered_oc$yc <- 0L
  recovered_yc <- read.csv(file.path(sandbox, "data/full_yc.csv")); recovered_yc$yc <- 1L
  recovered <- stack(list(recovered_oc, recovered_yc))
  stopifnot(!anyDuplicated(recovered$child_id))
  extra <- recovered[!recovered$child_id %in% existing$child_id, ]
  history <- stack(list(existing, extra))
  result <- list(); checks <- list(); sources <- list(); definitions <- list(); labels <- list()
  define <- function(variable, origin, period, description) {
    definitions[[variable]] <<- data.frame(variable = variable, origin = origin,
      period = period, definition = description)
  }
  for (cohort in c("oc", "yc")) {
    path <- file.path(r7_root, paste0("et_r7_", cohort, "ch_arch.dta"))
    raw <- haven::read_dta(path)
    phone_path <- file.path(r7_root, paste0("et_r7_", cohort, "ch_phone_arch.dta"))
    phone <- haven::read_dta(phone_path)
    roster_path <- file.path(r7_root, paste0("et_r7_", cohort, "hh_arch.dta"))
    roster <- haven::read_dta(roster_path)
    sources[[cohort]] <- data.frame(path = c(path, roster_path, phone_path),
      md5 = unname(tools::md5sum(c(path, roster_path, phone_path))))
    labelled_fields <- c("typwrk12r7", "typwrk7r7", "lstqualr7", "fdsec00r7", "electr7",
      "employer12r7", "employer7r7", "econsec12r7", "econsec7r7", "cntrctwk7r7")
    labels[[cohort]] <- do.call(rbind, lapply(labelled_fields, function(v) {
      lab <- attr(raw[[v]], "labels")
      data.frame(cohort=cohort, variable=v, code=number(lab), label=names(lab))
    }))
    ids <- number(raw$childcode)
    stopifnot(!anyNA(ids), !anyDuplicated(ids), !any(ids %in% number(phone$childcode)),
      !anyDuplicated(paste(roster$childcode, roster$memidr7)))
    c_id <- match(ids, as.numeric(sub("ET", "", constructed$childid)))
    z <- constructed[c_id, ]
    stopifnot(!anyNA(c_id), all(z$in_phone_r7 == 0), all(z$inround == 1),
      all(z$yc == as.integer(cohort == "yc")))
    h_id <- match(ids, history$child_id)
    h <- history[h_id, ]; stopifnot(all(h$yc == as.integer(cohort == "yc")))
    d <- data.frame(child_id = ids, childid = paste0("ET", sprintf("%06d", ids)),
      yc = as.integer(cohort == "yc"), cohort = if (cohort == "yc") "younger" else "older",
      round = 7L, interview_mode = "in_person", interview_date_r7 = as.Date(raw$dintr7),
      legacy_r5_sample_member = ids %in% existing$child_id,
      historical_lookup_available = !is.na(h_id),
      historical_lookup_source = ifelse(ids %in% existing$child_id, "existing_legacy_analysis_CSV",
        "legacy_extraction_without_deceased_filter_fallback"), stringsAsFactors = FALSE)
    for (v in setdiff(names(h), c("child_id", "yc"))) {
      target <- paste0(v, "_hist")
      d[[target]] <- h[[v]]
      define(target, "Original legacy analysis lookup; isolated legacy-source fallback if absent",
        "R1-R5; original variable timing retained", paste("Original", v, "unchanged; suffix prevents treating it as a current R7 measurement"))
    }
    # Explicitly keep the inherited gate as a flag; do NOT apply it to the master.
    d$original_language_gate <- if (cohort == "oc") !is.na(h$raw_lang) & h$raw_lang != 0 else TRUE
    d$legacy_comparison_eligible <- d$legacy_r5_sample_member & d$original_language_gate
    d$yc_exposure_review_required <- cohort == "yc"
    d$yc_r5_schooling_below8_or_missing <- cohort == "yc" & (is.na(h$hghgrade_final_num) | h$hghgrade_final_num < 8)
    d$school_grade_r7 <- domain(z$hghschoolgrade, 0:12)
    d$qualification_code_raw_r7 <- number(raw$lstqualr7)
    qualification_codes <- setdiff(number(attr(raw$lstqualr7, "labels")), c(-99,-88,-79,-77,77,79,88,99))
    stopifnot(length(qualification_codes) > 0)
    d$qualification_code_r7 <- domain(raw$lstqualr7, qualification_codes)
    # Reuse the original MTI rules verbatim as a clearly provisional sensitivity:
    # only the schooling input is updated; historical language/region remain fixed.
    expr <- parse("code/01_extract_clean.R")
    rule <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
      identical(e[[2L]], as.name("analysis_data")) &&
      grepl("IMTI = case_when", paste(deparse(e), collapse = ""), fixed = TRUE), as.list(expr))
    stopifnot(length(rule) == 1L)
    rule_env <- new.env(parent = globalenv())
    suppressPackageStartupMessages(require(dplyr))
    source("code/helpers/extraction_helpers.R", local = rule_env)
    rule_env$analysis_data <- h
    rule_env$analysis_data$hghgrade_final_num <- d$school_grade_r7
    eval(rule[[1L]], envir = rule_env)
    d$IMTI_r7_schooling_candidate <- number(rule_env$analysis_data$IMTI)
    define("IMTI_r7_schooling_candidate", "Original IMTI rules + R7 constructed school grade",
      "R7 school attainment, historical language/policy cells", "Provisional sensitivity only; not validated full schooling-language exposure, particularly for YC")
    define("school_grade_r7", "UKDA-9543 hghschoolgrade", "R7", "School-only highest completed grade; not the broader legacy grade scale")
    define("qualification_code_r7", paste(basename(path), "lstqualr7"), "R7", "Label-validated qualification category; special codes are NA; not years of schooling")
    screens <- c("lst12frmr7", "lst12nhsr7", "lst12bsnr7")
    ann <- sapply(raw[screens], domain, values = 0:1)
    d$worked_annual_r7 <- ifelse(rowSums(ann == 1, na.rm = TRUE) > 0, 1,
      ifelse(rowSums(ann == 0, na.rm = TRUE) == 3, 0, NA_real_))
    wk_names <- c("lstwkfrmr7", "lstwknhsr7", "lstwkbsnr7")
    wk <- sapply(seq_along(wk_names), function(j) {
      v <- domain(raw[[wk_names[j]]], 0:1)
      v[is.na(v) & !is.na(ann[,j]) & ann[,j] == 0] <- 0
      v
    })
    d$worked_weekly_r7 <- ifelse(rowSums(wk == 1, na.rm = TRUE) > 0, 1,
      ifelse(rowSums(wk == 0, na.rm = TRUE) == 3, 0, NA_real_))
    current_job <- domain(raw$curjbr7, 0:1)
    for (window in c("annual", "weekly_job")) {
      v <- if (window == "annual") "typwrk12r7" else "typwrk7r7"
      activity <- domain(raw[[v]], c(1:13, 15, 19:22))
      d[[paste0("activity_", window, "_raw_r7")]] <- number(raw[[v]])
      mapped <- activity; mapped[mapped %in% 21] <- 1; mapped[mapped %in% 22] <- 4
      d[[paste0("activity_", window, "_r7")]] <- mapped
      known_no <- if (window == "annual") !is.na(d$worked_annual_r7) & d$worked_annual_r7 == 0 else
        !is.na(d$worked_weekly_r7) & d$worked_weekly_r7 == 0 &
        ((!is.na(d$worked_annual_r7) & d$worked_annual_r7 == 0) | (!is.na(current_job) & current_job == 0))
      for (outcome in c("salary", "wage", "self_employment", "nonfarm_self_employment")) {
        positive <- switch(outcome, salary = 13, wage = c(5,12,13), self_employment = c(1:4,8:11), nonfarm_self_employment = 8:11)
        exclude <- switch(outcome, salary = c(8:11,19), wage = c(1:4,8:11,19),
          self_employment = c(5,12,13,19), nonfarm_self_employment = c(13,19))
        y <- rep(NA_real_, nrow(raw)); y[known_no] <- 0
        y[!is.na(mapped)] <- 0; y[mapped %in% exclude] <- NA; y[mapped %in% positive] <- 1
        target <- paste0(outcome, "_", window, "_r7")
        d[[target]] <- y
        define(target, paste(basename(path), v, "and work screens"), window,
          paste("Legacy", outcome, "positive/exclusion rules; unknown stays NA, valid nonwork=0; activity 21->1 and 22->4"))
      }
      # All-respondent alternatives use a different denominator, explicitly named.
      for (outcome in c("salary", "wage")) {
        y <- rep(NA_real_, nrow(raw)); y[known_no] <- 0
        y[!is.na(mapped)] <- as.numeric(mapped[!is.na(mapped)] %in% if (outcome == "salary") 13 else c(5,12,13))
        d[[paste0(outcome, "_", window, "_allrespondents_r7")]] <- y
      }
    }
    for (v in c("employer12r7", "employer7r7", "econsec12r7", "econsec7r7", "cntrctwk7r7")) {
      values <- setdiff(number(attr(raw[[v]], "labels")), c(-99,-88,-79,-77,77,79,88,99))
      d[[paste0(v, "_raw")]] <- number(raw[[v]])
      d[[paste0(v, "_clean")]] <- domain(raw[[v]], values)
    }
    d$public_sector_annual_r7 <- ifelse(is.na(d$employer12r7_clean), NA, as.integer(d$employer12r7_clean == 4))
    # Current roster: keep only actual resident members, require complete roster.
    hh <- tapply(as.integer(roster$livhser7 == 1), roster$childcode, sum)
    d$roster_complete_r7 <- domain(raw$cmplthhr7, 0:1)
    d$hhsize_r7 <- as.numeric(hh[as.character(ids)])
    d$hhsize_r7[is.na(d$roster_complete_r7) | d$roster_complete_r7 != 1] <- NA
    define("hhsize_r7", paste(basename(roster_path), "livhser7=1; core cmplthhr7=1"), "R7", "Current resident count including index child; incomplete rosters remain missing")
    for (v in c("wi", "hq", "wi_new", "hq_new", "sv", "cd")) {
      target <- switch(v, wi = "wi_r7_legacy_definition", hq = "hq_r7_legacy_definition",
        wi_new = "wi_r7_revised_definition", hq_new = "hq_r7_revised_definition", sv = "services_r7", cd = "durables_r7")
      d[[target]] <- number(z[[v]]); d[[target]][d[[target]] < 0 | d[[target]] > 1] <- NA
      define(target, paste("UKDA-9543 R7", v), "R7", "Imported constructed index, range checked; see raw component checks and definition bridge")
    }
    d$electricity_raw_r7 <- number(raw$electr7)
    d$electricity_r7 <- domain(raw$electr7, 0:1)
    d$foodsec_raw_r7 <- number(raw$fdsec00r7)
    d$foodsec_r7 <- domain(raw$fdsec00r7, 1:4)
    d$ownhouse_r7 <- domain(raw$ownhser7, 0:1)
    d$region_current_code_r7 <- number(raw$regionr7)
    d$region_current_r7 <- as.character(haven::as_factor(raw$regionr7))
    d$age_years_r7 <- number(z$ageyea)
    for (v in c("wallr7", "roofr7", "floorr7", "rmshser7", "srcwtrr7", "toiletr7", "flcookr7", paste0("hhitemsr7_", 1:9), "hhitemsr7_11")) {
      d[[paste0(v, "_raw")]] <- number(raw[[v]])
    }
    # Consumer durables: original ten-item set (fan is not in the original set).
    asset_names <- c(paste0("hhitemsr7_", 1:9), "hhitemsr7_11")
    assets <- sapply(raw[asset_names], domain, values = 0:1)
    d$durables_raw_reconstruction_r7 <- rowMeans(assets)
    d$durables_component_discrepancy_r7 <- !is.na(d$durables_raw_reconstruction_r7) & !is.na(d$durables_r7) &
      abs(d$durables_raw_reconstruction_r7 - d$durables_r7) > 1e-6
    d$electricity_component_discrepancy_r7 <- !is.na(d$electricity_r7) & !is.na(z$elecq) & d$electricity_r7 != z$elecq
    # TN43 Ethiopia definitions; un-suffixed constructed HQ/WI retain the
    # original wall-material rule. R7 _new versions additionally accept code 25.
    rooms <- domain(raw$rmshser7, 1:50)
    wall <- domain(raw$wallr7, 1:25); roof <- domain(raw$roofr7, 1:25); floor <- domain(raw$floorr7, 1:21)
    indicator <- function(x, good) ifelse(is.na(x), NA_real_, as.numeric(x %in% good))
    crowding <- pmin(rooms / d$hhsize_r7 / 1.5, 1)
    d$hq_raw_reconstruction_r7 <- (crowding + indicator(wall,c(3,8,9,14)) +
      indicator(roof,c(4,6,15)) + indicator(floor,c(1,3,6,7))) / 4
    water <- indicator(domain(raw$srcwtrr7,1:14),c(3,4,8,10))
    sanitation <- indicator(domain(raw$toiletr7,1:10),c(1,5,6))
    cooking <- indicator(domain(raw$flcookr7,1:16),c(8,9))
    d$services_raw_reconstruction_r7 <- (d$electricity_r7 + water + sanitation + cooking) / 4
    d$wi_raw_reconstruction_r7 <- (d$hq_raw_reconstruction_r7 + d$services_raw_reconstruction_r7 + d$durables_raw_reconstruction_r7)/3
    for (v in c("hq","services","wi")) {
      reconstructed <- d[[paste0(v,"_raw_reconstruction_r7")]]
      imported <- d[[switch(v,hq="hq_r7_legacy_definition",services="services_r7",wi="wi_r7_legacy_definition")]]
      d[[paste0(v,"_component_discrepancy_r7")]] <- !is.na(reconstructed) & !is.na(imported) & abs(reconstructed-imported)>1e-6
      stopifnot(!any(d[[paste0(v,"_component_discrepancy_r7")]]))
    }
    # YC reading responses are available; do not invent an answer key or IRT scale.
    for (v in c(paste0("raq", 1:12, "r7"), "cnsntreadr7", "readtesttime")) {
      d[[paste0(v, "_raw_r7")]] <- if (v %in% names(raw)) number(raw[[v]]) else NA_real_
    }
    d$reading_items_observed_r7 <- if (cohort == "yc") rowSums(!is.na(d[paste0("raq", 1:12, "r7_raw_r7")])) else NA_integer_
    d$reading_score_r7 <- NA_real_
    d$reading_score_requires_validated_key <- cohort == "yc"
    historical_controls <- c("IMTI", "E_is", "chsex", "zbfa", "stunting", "caredu_r1", "careage_r1", "caresex_r1",
      "hhsize", "wi_new", "hq_new", "cd_new", "elecq_new", "ownlandhse_r1", "foodsec_r3")
    # Include caregiver sex for both cohorts, flag completeness rather than silently drop it.
    hist_columns <- paste0(historical_controls, "_hist")
    d$historical_controls_complete <- complete.cases(d[hist_columns])
    mixed <- c(paste0(setdiff(historical_controls, c("hhsize","wi_new","hq_new","cd_new","elecq_new")), "_hist"),
      "hhsize_r7", "wi_r7_legacy_definition", "hq_r7_legacy_definition", "durables_r7", "electricity_r7")
    d$r7_household_controls_complete <- complete.cases(d[mixed])
    mixed_food <- c(setdiff(mixed, "foodsec_r3_hist"), "foodsec_r7")
    d$r7_household_plus_food_controls_complete <- complete.cases(d[mixed_food])
    for (y in c("salary", "wage")) d[[paste0("complete_case_", y, "_annual_r7")]] <- d$r7_household_controls_complete & !is.na(d[[paste0(y,"_annual_r7")]])
    stopifnot(all(!is.na(d$worked_annual_r7)),
      !any(d$worked_annual_r7 == 1 & is.na(d$activity_annual_r7)),
      !any(d$worked_annual_r7 == 0 & !is.na(d$activity_annual_r7)),
      all(d$salary_annual_allrespondents_r7 <= d$wage_annual_allrespondents_r7, na.rm=TRUE))
    checks[[cohort]] <- data.frame(cohort = cohort, rows = nrow(d), phone_ids_retained = sum(ids %in% number(phone$childcode)),
      existing_history = sum(d$legacy_r5_sample_member), fallback_history = sum(!d$legacy_r5_sample_member & d$historical_lookup_available),
      annual_workers = sum(d$worked_annual_r7 == 1), roster_complete = sum(d$roster_complete_r7 == 1, na.rm=TRUE),
      durables_discrepancies = sum(d$durables_component_discrepancy_r7), electricity_discrepancies = sum(d$electricity_component_discrepancy_r7),
      hq_raw_pairs = sum(!is.na(d$hq_raw_reconstruction_r7)&!is.na(d$hq_r7_legacy_definition)),
      services_raw_pairs = sum(!is.na(d$services_raw_reconstruction_r7)&!is.na(d$services_r7)),
      wi_raw_pairs = sum(!is.na(d$wi_raw_reconstruction_r7)&!is.na(d$wi_r7_legacy_definition)),
      hq_discrepancies=sum(d$hq_component_discrepancy_r7),services_discrepancies=sum(d$services_component_discrepancy_r7),wi_discrepancies=sum(d$wi_component_discrepancy_r7))
    result[[cohort]] <- d
  }
  master <- stack(result); master <- master[order(master$child_id), ]; rownames(master) <- NULL
  stopifnot(nrow(master) == 2038L, sum(master$yc == 0) == 632L, sum(master$yc == 1) == 1406L,
    !anyNA(master$child_id), !anyDuplicated(master$child_id), all(master$interview_mode == "in_person"))
  # Match source ID strings and ensure historical values in the existing sample are unchanged.
  original_id <- match(master$child_id, existing$child_id)
  for (v in setdiff(names(existing), c("child_id", "yc"))) {
    ix <- !is.na(original_id)
    stopifnot(isTRUE(all.equal(master[[paste0(v,"_hist")]][ix], existing[[v]][original_id[ix]], check.attributes=FALSE)))
  }
  write.csv(master, file.path(destination, "r7_inperson_analysis.csv"), row.names = FALSE, na = "")
  saveRDS(master, file.path(destination, "r7_inperson_analysis.rds"))
  audit(do.call(rbind, checks), "build_checks.csv")
  audit(do.call(rbind, sources), "r7_source_manifest.csv")
  audit(do.call(rbind, labels), "source_value_labels.csv")
  audit(do.call(rbind, lapply(split(master, master$cohort), function(d) {
    data.frame(cohort=unique(d$cohort), rows=nrow(d), age_min=min(d$age_years_r7,na.rm=TRUE),
      age_max=max(d$age_years_r7,na.rm=TRUE), r5_schooling_below8_or_missing=sum(d$yc_r5_schooling_below8_or_missing),
      imti_candidate_changed=sum(abs(d$IMTI_r7_schooling_candidate-d$IMTI_hist)>1e-8,na.rm=TRUE),
      reading_any_items=sum(d$reading_items_observed_r7>0,na.rm=TRUE),
      reading_all12_items=sum(d$reading_items_observed_r7==12,na.rm=TRUE))
  })), "cohort_summary.csv")
  audit(do.call(rbind, lapply(split(master, master$cohort), function(d) {
    do.call(rbind,lapply(c("worked_annual_r7","worked_weekly_r7","salary_annual_r7","wage_annual_r7",
      "self_employment_annual_r7","nonfarm_self_employment_annual_r7", "salary_weekly_job_r7","wage_weekly_job_r7"), function(v) {
      data.frame(cohort=unique(d$cohort), variable=v, zero=sum(d[[v]]==0,na.rm=TRUE),
        one=sum(d[[v]]==1,na.rm=TRUE), missing=sum(is.na(d[[v]])))
    }))
  })), "outcome_counts.csv")
  availability <- do.call(rbind, lapply(c("older", "younger"), function(co) {
    d <- master[master$cohort == co, ]
    do.call(rbind, lapply(names(d), function(v) data.frame(cohort=co, variable=v, rows=nrow(d), observed=sum(!is.na(d[[v]])), missing=sum(is.na(d[[v]])))))
  }))
  audit(availability, "variable_availability.csv")
  define("child_id", "Raw R7 childcode", "All rounds", "Unique numeric participant join key; childid retains the ET-prefixed string")
  define("yc", "Raw cohort file + constructed cohort check", "Time invariant", "0=older cohort; 1=younger cohort")
  define("IMTI_hist", "Original legacy analysis lookup", "Schooling through R5", "Frozen original schooling-capped MTI measure; YC may complete primary schooling after R5")
  define("E_is_hist", "Original legacy policy/ethnicity/language rules", "Historical assignment", "Frozen policy instrument; current residence must not overwrite policy geography")
  define("legacy_comparison_eligible", "Existing legacy CSV membership and original OC language gate", "Sample flag", "OC: original history member and raw_lang nonzero/nonmissing; YC: original history member. Not applied to master")
  define("yc_exposure_review_required", "Cohort flag", "R7", "TRUE for all YC: validate completed-primary MTI exposure before estimation")
  define("yc_r5_schooling_below8_or_missing", "Legacy completed schooling", "R5", "Conservative review flag for YC schooling below eight or missing; not a finding of miscoding")
  define("reading_score_r7", "Not scored", "R7", "Intentionally missing: archived raw reading responses require a validated answer key and a comparability decision")
  define("reading_items_observed_r7", "Raw raq1r7 to raq12r7", "R7", "Count of nonmissing raw response cells, not correct answers or a validated test-completion flag")
  define("foodsec_r7", "Raw fdsec00r7", "R7", "Food situation category, valid codes 1:4; same four-category construct as historical control, different date")
  define("electricity_r7", "Raw electr7", "R7", "0=no, 1=yes; missing and special codes remain NA")
  define("r7_household_controls_complete", "Outcome-independent completeness", "Mixed timing", "Frozen IMTI/E_is, sex, childhood nutrition, R1 caregiver/land and R3 food; R7 size, WI/HQ/CD/electricity")
  define("r7_household_plus_food_controls_complete", "Outcome-independent completeness", "Mixed timing", "Same as household completeness but replaces R3 food situation with R7 food situation")
  for (w in c("annual","weekly_job")) for (y in c("salary","wage")) {
    define(paste0(y,"_",w,"_allrespondents_r7"), "Raw activity and valid nonwork screens", w,
      "Alternative denominator: any valid other activity=0, including categories excluded in legacy outcome; known nonwork=0")
  }
  dictionary <- do.call(rbind, lapply(names(master), function(v) {
    if (v %in% names(definitions)) definitions[[v]] else data.frame(variable=v, origin="R7 source or explicitly named build flag", period="R7 unless marked hist", definition=v)
  }))
  audit(dictionary, "variable_dictionary.csv")
  sample_counts <- do.call(rbind, lapply(c("older", "younger"), function(co) {
    d <- master[master$cohort == co, ]; reg <- d$region_hist
    do.call(rbind, lapply(c("all", "non_Addis", "excluding_Amhara", "Addis"), function(g) {
      ix <- if(g=="all")rep(TRUE,nrow(d))else !is.na(reg)&if(g=="Addis")reg=="Addis Ababa" else if(g=="non_Addis")reg!="Addis Ababa" else !reg%in%c("Addis Ababa","Amhara")
      do.call(rbind, lapply(c("salary","wage"), function(y) {
        eligible <- ix & d$legacy_comparison_eligible
        if(g=="Addis") eligible<-eligible & !is.na(d$entype_r4_hist)
        data.frame(cohort=co,group=g,outcome=y,master_rows=sum(ix),legacy_comparison_eligible=sum(eligible),
          complete_current_controls=sum(eligible & d[[paste0("complete_case_",y,"_annual_r7")]]),
          complete_current_controls_and_food=sum(eligible & d$r7_household_plus_food_controls_complete & !is.na(d[[paste0(y,"_annual_r7")]])))
      }))
    }))
  }))
  audit(sample_counts,"candidate_sample_counts.csv")
  stopifnot(identical(hash, tools::md5sum(protected)))
  audit(data.frame(path=names(hash),md5=unname(hash),unchanged=TRUE),"legacy_integrity.csv")
  writeLines(capture.output(sessionInfo()),file.path(audit_dir,"session.txt"))
  unlink(sandbox, recursive = TRUE)
  message("Created R7 master: ", nrow(master), " in-person participants; CSV and RDS in ", destination)
})
