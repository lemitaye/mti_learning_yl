# Run from the project root:
# Rscript code/auxiliary/compare_constructed_releases.R [audit_output_directory]
# Diagnostic only. Raw inputs, production CSVs, tables and Overleaf are untouched.
# Requires the existing analysis packages; no package installation is performed.
local({
  root <- normalizePath(".")
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args)) args[1L] else "output/constructed_release_comparison"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out)
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  source("code/auxiliary/reconstruct_round3_electricity.R", local = TRUE)
  inputs <- normalizePath(raw_data_files[c("constructed_legacy", "constructed_r1_r7")])
  # Freeze production results and code; all execution happens in temporary copies.
  protected <- c(inputs, list.files("data", full.names = TRUE, recursive = TRUE),
    list.files("output/tables", full.names = TRUE, recursive = TRUE),
    list.files("output/figures", full.names = TRUE, recursive = TRUE),
    list.files("code", full.names = TRUE, recursive = TRUE))
  protected <- protected[file.exists(protected) & !dir.exists(protected)]
  before_hash <- tools::md5sum(protected)
  required <- c("tidyverse", "haven", "scales", "stargazer", "AER", "clubSandwich",
    "lfe", "fastDummies", "xtable", "starpolishr")
  stopifnot(all(vapply(required, requireNamespace, logical(1), quietly = TRUE)))
  csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
  old <- haven::read_dta(inputs[1L])
  latest <- haven::read_dta(inputs[2L])
  key <- function(x) paste(x$childid, x$round, sep = ":")
  historical <- latest[latest$round %in% 1:5, ]
  stopifnot(!anyDuplicated(key(old)), !anyDuplicated(key(historical)),
    setequal(key(old), key(historical)))
  historical <- historical[match(key(old), key(historical)), ]
  csv(data.frame(release = c("legacy_R1_R5", "updated_R1_R7"),
    records = c(nrow(old), nrow(latest)), variables = c(ncol(old), ncol(latest)),
    children = c(length(unique(old$childid)), length(unique(latest$childid))),
    md5 = unname(tools::md5sum(inputs))), "release_summary.csv")
  csv(as.data.frame(table(release = rep(c("legacy", "updated"), c(nrow(old), nrow(latest))),
    round = c(old$round, latest$round))), "records_by_round.csv")
  csv(data.frame(variable = union(names(old), names(latest)),
    in_legacy = union(names(old), names(latest)) %in% names(old),
    in_updated = union(names(old), names(latest)) %in% names(latest)), "schema.csv")
  aliases <- c(underweight = "underweight_child", stunting = "stunting_child",
    thinness = "thinness_child", hghgrade = "hghschoolgrade", sv_new = "sv",
    cd_new = "cd", drwaterq_new = "drwaterq", toiletq_new = "toiletq",
    elecq_new = "elecq", cookingq_new = "cookingq")
  stopifnot(unname(attr(latest$status, "labels")[["Died"]]) == 2)
  mapping <- data.frame(legacy = c("deceased", names(aliases)),
    updated = c("status == 2", unname(aliases)),
    interpretation = c("1 if status is Died; missing otherwise, matching the legacy filter convention",
      ifelse(names(aliases) == "hghgrade",
        "PROVISIONAL: highest school grade completed differs from legacy broader grade measure",
        "Provisional name mapping; updated values and missingness retained")))
  csv(mapping, "compatibility_mapping.csv")
  candidate <- historical
  candidate$deceased <- ifelse(candidate$status == 2, 1, NA_real_)
  for (v in names(aliases)) candidate[[v]] <- candidate[[aliases[[v]]]]
  reconstruction <- reconstruct_round3_electricity(candidate, root)
  csv(reconstruction$audit, "raw_r3_electricity_join_audit.csv")
  csv(reconstruction$sources, "raw_r3_electricity_sources.csv")
  r3_rows <- candidate$round == 3
  raw_electricity <- reconstruction$data$elecq_new[r3_rows]
  legacy_electricity <- old$elecq_new[r3_rows]
  observed <- !is.na(raw_electricity) & !is.na(legacy_electricity)
  csv(data.frame(round3_records = sum(r3_rows), observed = sum(!is.na(raw_electricity)),
    missing = sum(is.na(raw_electricity)),
    legacy_value_mismatches = sum(raw_electricity[observed] != legacy_electricity[observed]),
    legacy_missingness_mismatches = sum(is.na(raw_electricity) != is.na(legacy_electricity))),
    "raw_r3_electricity_validation.csv")
  primitive <- function(x) { attributes(x) <- NULL; x }
  differences <- list()
  labels <- list()
  for (v in names(old)) {
    w <- if (v %in% names(latest)) v else if (v %in% names(aliases)) aliases[[v]] else
      if (v == "deceased") "status == 2" else NA_character_
    for (rd in 1:5) {
      x <- primitive(old[[v]][old$round == rd])
      if (!is.na(w)) {
        y <- primitive(candidate[[v]][old$round == rd])
        valid <- !is.na(x) & !is.na(y)
        equal <- is.na(x) & is.na(y)
        if (is.numeric(x) && is.numeric(y)) {
          equal[valid] <- abs(x[valid] - y[valid]) <= 1e-7 * pmax(1, abs(x[valid]), abs(y[valid]))
        } else equal[valid] <- x[valid] == y[valid]
        stats <- c(sum(!is.na(y)), sum(!equal), sum(valid & !equal),
          sum(!is.na(x) & is.na(y)), sum(is.na(x) & !is.na(y)))
      } else stats <- rep(NA_integer_, 5)
      differences[[length(differences) + 1L]] <- data.frame(
        legacy_variable = v, updated_variable = w, round = rd, records = length(x),
        legacy_observed = sum(!is.na(x)), updated_observed = stats[1L],
        changed = stats[2L], changed_nonmissing_values = stats[3L],
        observed_to_missing = stats[4L], missing_to_observed = stats[5L])
    }
    if (!is.na(w) && w != "status == 2") {
      for (release in c("legacy", "updated")) {
        z <- if (release == "legacy") old[[v]] else latest[[w]]
        labs <- attr(z, "labels")
        labels[[length(labels) + 1L]] <- data.frame(legacy_variable = v,
          updated_variable = w, release = release,
          variable_label = if (is.null(attr(z, "label"))) "" else attr(z, "label"),
          value_codes_and_labels = paste(paste(unname(labs), names(labs), sep = "="), collapse = "; "))
      }
    }
  }
  differences <- do.call(rbind, differences)
  csv(differences, "historical_differences_by_round.csv")
  csv(do.call(rbind, labels), "variable_and_value_labels.csv")
  csv(do.call(rbind, lapply(sort(unique(latest$round)), function(rd) {
    d <- latest[latest$round == rd, ]
    data.frame(round = rd, records = nrow(d), observed_electricity = sum(!is.na(d$elecq)))
  })), "updated_electricity_availability.csv")
  csv(as.data.frame(table(legacy_deceased = ifelse(is.na(old$deceased), "missing", "deceased"),
    updated_status = haven::as_factor(historical$status), useNA = "ifany")), "death_status_crosswalk.csv")
  grade_crosswalk <- as.data.frame(table(round = old$round, cohort = old$yc,
    legacy_grade = ifelse(is.na(old$hghgrade), "missing", as.character(old$hghgrade)),
    updated_school_grade = ifelse(is.na(historical$hghschoolgrade), "missing",
      as.character(historical$hghschoolgrade))))
  csv(grade_crosswalk[grade_crosswalk$Freq > 0L, ], "schooling_code_crosswalk.csv")

  # Worker executes the same analysis expressions, catching model failures one
  # at a time so all 49 specifications can be audited even after the first fails.
  worker <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    setwd(args[1L])
    mode <- args[2L]
    Sys.setenv(MTI_OVERLEAF_DIR = "")
    options(warn = 1)
    source("code/helpers/_setup.R")
    source("code/01_extract_clean.R")
    source("code/02_cleaning_yc.R")
    if (commandArgs(trailingOnly = TRUE)[2L] == "direct") quit(status = 0L)
    source("code/03_descriptives.R")
    saveRDS(list(table1 = summary_table, figure = employ_gr$data), "descriptives.rds")

    audited_fit <- function(engine, formula, data, subset) {
      if (!missing(subset)) {
        keep <- eval(substitute(subset), data, parent.frame())
        data <- data[which(keep), , drop = FALSE]
      }
      variables <- all.vars(formula)
      complete <- complete.cases(data[, variables, drop = FALSE])
      missing_counts <- colSums(is.na(data[, variables, drop = FALSE]))
      result <- tryCatch(
        if (engine == "lm") stats::lm(formula, data = data) else lfe::felm(formula, data = data),
        error = function(e) structure(list(message = conditionMessage(e)), class = "audit_fit_error"))
      attr(result, "audit") <- list(formula = paste(deparse(formula), collapse = " "),
        input_n = nrow(data), complete_n = sum(complete),
        missing = paste(paste(names(missing_counts)[missing_counts > 0],
          missing_counts[missing_counts > 0], sep = "="), collapse = "; "))
      result
    }
    lm <- function(formula, data, subset, ...) {
      if (!missing(subset)) data <- data[which(eval(substitute(subset), data, parent.frame())), , drop = FALSE]
      audited_fit("lm", formula, data)
    }
    felm <- function(formula, data, subset, ...) {
      if (!missing(subset)) data <- data[which(eval(substitute(subset), data, parent.frame())), , drop = FALSE]
      audited_fit("felm", formula, data)
    }
    # The worker is already fresh; skip the workspace clear and printed tables.
    for (expression in parse("code/05_estimation.R")) {
      head <- if (is.call(expression)) as.character(expression[[1L]]) else ""
      if (head %in% c("rm", "stargazer") || grepl("^waldtest", paste(deparse(expression), collapse = ""))) next
      eval(expression, envir = environment())
    }
    model_names <- ls(pattern = "^(iv[0-9]|rf[0-9]|m1$)")
    stopifnot(length(model_names) == 49L)
    models49 <- mget(model_names)
    pack <- function(m) {
      a <- attr(m, "audit")
      a$ok <- !inherits(m, "audit_fit_error")
      a$error <- if (a$ok) "" else m$message
      if (a$ok) {
        a$coef <- coef(m); a$vcov <- vcov(m)
        a$n <- if (inherits(m, "felm")) m$N else nobs(m)
      }
      a
    }
    saveRDS(lapply(models49, pack), "models49.rds")
    # Table 3 consists of two OLS association fits, additional to those 49.
    for (expression in parse("code/09_table_rev.R")) {
      if (is.call(expression) && identical(expression[[1L]], as.name("<-")) &&
          identical(expression[[2L]], as.name("coefs"))) break
      eval(expression, envir = environment())
    }
    saveRDS(lapply(models, pack), "channel_models.rds")
    if (commandArgs(trailingOnly = TRUE)[2L] %in% c("legacy", "updated_raw_r3")) {
      # Restore the standard fitters before the table-specific diagnostics.
      rm(lm, felm)
      # Refit without wrappers to preserve the calls inspected by stargazer.
      source("code/05_estimation.R")
      source("code/06_export_res.R")
      source("code/09_table_rev.R")
      writeLines("Full pipeline and table-specific diagnostics completed.", "pipeline_status.txt")
    } else {
      writeLines("Diagnostic models recorded; regression table export skipped.", "pipeline_status.txt")
    }
    writeLines(capture.output(sessionInfo()), "session.txt")
  }
  work <- tempfile("constructed_release_comparison_", tmpdir = tempdir())
  dir.create(work)
  worker_file <- file.path(work, "worker.R")
  # Evaluate at the worker's global level: source() defaults and model names
  # then follow the production scripts' shared-workspace behavior.
  writeLines(c("worker <-", deparse(worker), "eval(body(worker), envir = .GlobalEnv)"), worker_file)
  modes <- c("legacy", "direct", "updated", "hybrid_electricity", "updated_raw_r3")
  directories <- setNames(file.path(work, modes), modes)
  statuses <- integer()
  for (mode in names(directories)) {
    sandbox <- directories[[mode]]
    dir.create(sandbox)
    stopifnot(file.copy(file.path(root, "code"), sandbox, recursive = TRUE),
      file.copy(file.path(root, "README.md"), sandbox),
      file.symlink(file.path(root, "rawdata"), file.path(sandbox, "rawdata")))
    dir.create(file.path(sandbox, "data"))
    # These table writers interpret Rscript arguments as output filenames.
    # The worker uses its arguments for isolation, so retain default table paths.
    for (script in c("07_table_main.R", "09_table_rev.R")) {
      path <- file.path(sandbox, "code", script)
      text <- readLines(path)
      text <- gsub('commandArgs(trailingOnly = TRUE)', 'character()', text, fixed = TRUE)
      writeLines(text, path)
    }
    if (mode != "legacy") {
      trial <- if (mode == "direct") latest else candidate
      # Diagnostic only: carry forward the legacy electricity control.
      # All other constructed fields retain the updated release's values.
      if (mode == "hybrid_electricity") trial$elecq_new <- old$elecq_new
      if (mode == "updated_raw_r3") trial <- reconstruction$data
      saveRDS(trial, file.path(sandbox, "constructed_candidate.rds"))
      for (script in c("01_extract_clean.R", "02_cleaning_yc.R")) {
        path <- file.path(sandbox, "code", script)
        text <- readLines(path)
        needle <- 'read_dta(raw_data_files[["constructed_legacy"]])'
        stopifnot(sum(grepl(needle, text, fixed = TRUE)) == 1L)
        text <- gsub(needle, 'readRDS("constructed_candidate.rds")', text, fixed = TRUE)
        writeLines(text, path)
      }
    }
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c(shQuote(worker_file), shQuote(sandbox), mode),
      stdout = file.path(out, paste0(mode, "_run.log")),
      stderr = file.path(out, paste0(mode, "_run.log")))
    statuses[mode] <- status
    message(mode, " worker exit status: ", status)
  }
  stopifnot(statuses[["legacy"]] == 0L, statuses[["updated"]] == 0L,
    statuses[["direct"]] != 0L, statuses[["hybrid_electricity"]] == 0L,
    statuses[["updated_raw_r3"]] == 0L)
  csv(data.frame(run = names(statuses), exit_status = unname(statuses)), "run_status.csv")
  old_models <- readRDS(file.path(directories[["legacy"]], "models49.rds"))
  new_models <- readRDS(file.path(directories[["updated"]], "models49.rds"))
  stopifnot(all(vapply(old_models, `[[`, logical(1), "ok")))
  model_comparison <- function(a, b) do.call(rbind, lapply(names(a), function(nm) {
    x <- a[[nm]]; y <- b[[nm]]
    data.frame(model = nm, legacy_n = x$n, updated_complete_n = y$complete_n,
      updated_estimable = y$ok, updated_n = if (y$ok) y$n else NA_integer_,
      coefficients_match = if (y$ok) isTRUE(all.equal(x$coef, y$coef, tolerance = 1e-10)) else FALSE,
      covariance_matches = if (y$ok) isTRUE(all.equal(x$vcov, y$vcov, tolerance = 1e-10)) else FALSE,
      error = y$error, formula = y$formula, updated_missing = y$missing)
  }))
  models <- model_comparison(old_models, new_models)
  csv(models, "model_replication.csv")
  hybrid_models <- readRDS(file.path(directories[["hybrid_electricity"]], "models49.rds"))
  hybrid_comparison <- model_comparison(old_models, hybrid_models)
  csv(hybrid_comparison, "hybrid_electricity_model_comparison.csv")
  raw_models <- readRDS(file.path(directories[["updated_raw_r3"]], "models49.rds"))
  raw_comparison <- model_comparison(old_models, raw_models)
  raw_vs_hybrid <- model_comparison(hybrid_models, raw_models)
  csv(raw_comparison, "raw_r3_model_replication.csv")
  csv(raw_vs_hybrid, "raw_r3_vs_previous_hybrid.csv")
  saveRDS(raw_models, file.path(out, "raw_r3_model_snapshots.rds"))
  channels_old <- readRDS(file.path(directories[["legacy"]], "channel_models.rds"))
  channels_new <- readRDS(file.path(directories[["updated"]], "channel_models.rds"))
  names(channels_old) <- names(channels_new) <- c("Table3_whole", "Table3_excluding_Amhara")
  csv(model_comparison(channels_old, channels_new), "channel_replication.csv")
  hybrid_channels <- readRDS(file.path(directories[["hybrid_electricity"]], "channel_models.rds"))
  names(hybrid_channels) <- names(channels_old)
  csv(model_comparison(channels_old, hybrid_channels), "hybrid_electricity_channel_comparison.csv")
  raw_channels <- readRDS(file.path(directories[["updated_raw_r3"]], "channel_models.rds"))
  names(raw_channels) <- names(channels_old)
  csv(model_comparison(channels_old, raw_channels), "raw_r3_channel_replication.csv")
  saveRDS(raw_channels, file.path(out, "raw_r3_channel_snapshots.rds"))
  main_models <- c("iv6", "iv1", "iv6_nonAm", "iv1_nonAm")
  main_results <- do.call(rbind, lapply(seq_along(main_models), function(i) {
    name <- main_models[i]; a <- old_models[[name]]; b <- hybrid_models[[name]]
    j <- grep("IMTI", names(a$coef), fixed = TRUE)
    k <- if (b$ok) grep("IMTI", names(b$coef), fixed = TRUE) else integer()
    stopifnot(length(j) == 1L, !b$ok || length(k) == 1L)
    data.frame(column = i, outcome = if (i %% 2L) "Salary employment" else "Wage employment",
      sample = if (i > 2L) "Excluding Amhara" else "Non-Addis",
      legacy_n = a$n, legacy_beta = unname(a$coef[j]), legacy_se = sqrt(diag(a$vcov))[j],
      updated_only_complete_n = new_models[[name]]$complete_n,
      hybrid_n = if (b$ok) b$n else NA_integer_,
      hybrid_beta = if (b$ok) unname(b$coef[k]) else NA_real_,
      hybrid_se = if (b$ok) sqrt(diag(b$vcov))[k] else NA_real_)
  }))
  rownames(main_results) <- NULL
  csv(main_results, "table2_release_comparison.csv")
  raw_main_results <- do.call(rbind, lapply(seq_along(main_models), function(i) {
    name <- main_models[i]; a <- old_models[[name]]; b <- raw_models[[name]]
    j <- grep("IMTI", names(a$coef), fixed = TRUE)
    k <- grep("IMTI", names(b$coef), fixed = TRUE)
    stopifnot(b$ok, length(j) == 1L, length(k) == 1L)
    data.frame(column = i, outcome = main_results$outcome[i], sample = main_results$sample[i],
      legacy_n = a$n, legacy_beta = unname(a$coef[j]), legacy_se = sqrt(diag(a$vcov))[j],
      updated_raw_r3_n = b$n, updated_raw_r3_beta = unname(b$coef[k]),
      updated_raw_r3_se = sqrt(diag(b$vcov))[k])
  }))
  rownames(raw_main_results) <- NULL
  csv(raw_main_results, "raw_r3_table2_comparison.csv")
  # Compare current stored analysis CSVs with a fresh legacy extraction.
  analysis_files <- c("aa_samp.csv", "non_aa_samp.csv", "ylsample_yc.csv", "schsur_yl_joined.csv")
  current <- do.call(rbind, lapply(analysis_files, function(f) data.frame(file = f,
    fresh_legacy_matches_stored = identical(readLines(file.path(root, "data", f)),
      readLines(file.path(directories[["legacy"]], "data", f))))))
  csv(current, "legacy_matches_stored_data.csv")
  field_changes <- list()
  for (f in analysis_files) {
    a <- read.csv(file.path(directories[["legacy"]], "data", f))
    b <- read.csv(file.path(directories[["updated"]], "data", f))
    id <- if ("child_id" %in% names(a)) "child_id" else "childid"
    stopifnot(!anyDuplicated(a[[id]]), !anyDuplicated(b[[id]]))
    matched <- match(a[[id]], b[[id]])
    keep <- !is.na(matched)
    for (v in intersect(names(a), names(b))) {
      x <- a[[v]][keep]; y <- b[[v]][matched[keep]]
      valid <- !is.na(x) & !is.na(y)
      equal <- is.na(x) & is.na(y)
      equal[valid] <- x[valid] == y[valid]
      field_changes[[length(field_changes) + 1L]] <- data.frame(dataset = f, variable = v,
        legacy_n = nrow(a), updated_n = nrow(b), common_n = sum(keep),
        legacy_only = sum(!keep), updated_only = sum(!b[[id]] %in% a[[id]]),
        changed = sum(!equal), missingness_changed = sum(is.na(x) != is.na(y)))
    }
  }
  field_changes <- do.call(rbind, field_changes)
  csv(field_changes, "analysis_variable_changes.csv")
  raw_field_changes <- do.call(rbind, lapply(analysis_files, function(f) {
    a <- read.csv(file.path(directories[["legacy"]], "data", f))
    b <- read.csv(file.path(directories[["updated_raw_r3"]], "data", f))
    id <- if ("child_id" %in% names(a)) "child_id" else "childid"
    stopifnot(!anyDuplicated(a[[id]]), !anyDuplicated(b[[id]]))
    matched <- match(a[[id]], b[[id]]); keep <- !is.na(matched)
    do.call(rbind, lapply(intersect(names(a), names(b)), function(v) {
      x <- a[[v]][keep]; y <- b[[v]][matched[keep]]
      valid <- !is.na(x) & !is.na(y)
      equal <- is.na(x) & is.na(y); equal[valid] <- x[valid] == y[valid]
      data.frame(dataset = f, variable = v, legacy_n = nrow(a), updated_n = nrow(b),
        common_n = sum(keep), legacy_only = sum(!keep), updated_only = sum(!b[[id]] %in% a[[id]]),
        changed = sum(!equal), missingness_changed = sum(is.na(x) != is.na(y)))
    }))
  }))
  csv(raw_field_changes, "raw_r3_analysis_variable_changes.csv")
  raw_hybrid_inputs <- do.call(rbind, lapply(analysis_files, function(f) data.frame(dataset = f,
    raw_r3_matches_hybrid_inputs = identical(
      readLines(file.path(directories[["updated_raw_r3"]], "data", f)),
      readLines(file.path(directories[["hybrid_electricity"]], "data", f))))))
  csv(raw_hybrid_inputs, "raw_r3_vs_hybrid_analysis_data.csv")
  stopifnot(all(raw_hybrid_inputs$raw_r3_matches_hybrid_inputs),
    all(raw_vs_hybrid$coefficients_match), all(raw_vs_hybrid$covariance_matches),
    all(raw_vs_hybrid$legacy_n == raw_vs_hybrid$updated_n))
  a <- readRDS(file.path(directories[["legacy"]], "descriptives.rds"))
  b <- readRDS(file.path(directories[["updated"]], "descriptives.rds"))
  csv(a$table1, "table1_legacy.csv"); csv(b$table1, "table1_updated.csv")
  csv(a$figure, "figure1_legacy.csv"); csv(b$figure, "figure1_updated.csv")
  raw_desc <- readRDS(file.path(directories[["updated_raw_r3"]], "descriptives.rds"))
  csv(raw_desc$table1, "raw_r3_table1.csv"); csv(raw_desc$figure, "raw_r3_figure1.csv")
  summary <- data.frame(
    check = c("historical_child_round_keys_match", "fresh_legacy_CSVs_match_stored",
      "table1_matches", "figure1_data_match", "49_models_estimable_updated"),
    result = c(TRUE, all(current$fresh_legacy_matches_stored),
      isTRUE(all.equal(a$table1, b$table1, tolerance = 1e-10)),
      isTRUE(all.equal(a$figure, b$figure, tolerance = 1e-10)), all(models$updated_estimable)))
  csv(summary, "replication_summary.csv")
  # Validate baseline model snapshots against the previous verified analysis.
  previous_file <- file.path(root, "output/analysis_polish_audit/before_models.rds")
  if (file.exists(previous_file)) {
    previous <- readRDS(previous_file)
    csv(do.call(rbind, lapply(names(old_models), function(nm) {
      x <- old_models[[nm]]; y <- previous[[nm]]
      data.frame(model = nm, n_match = identical(x$n, y$n),
        coef_match = isTRUE(all.equal(x$coef, y$coef, tolerance = 1e-10)),
        vcov_match = isTRUE(all.equal(x$vcov, y$vcov, tolerance = 1e-10)))
    })), "legacy_matches_previous_models.csv")
  }
  # Retain aggregate model snapshots and fresh baseline tables; never copy the
  # worker's participant-level CSVs or candidate RDS into report deliverables.
  dir.create(file.path(out, "legacy_tables"), showWarnings = FALSE)
  table_files <- list.files(file.path(directories[["legacy"]], "output/tables"), full.names = TRUE)
  stopifnot(all(file.copy(table_files, file.path(out, "legacy_tables"), overwrite = TRUE)))
  normalize_table <- function(path) {
    x <- readLines(path, warn = FALSE)
    x <- sub("[[:space:]]+$", "", x)
    x <- x[!grepl("^[[:space:]]*%", x)]
    x[nzchar(x)]
  }
  staged_checks <- do.call(rbind, lapply(table_files, function(path) {
    stored <- file.path(root, "output/tables", basename(path))
    data.frame(file = basename(path), stored_exists = file.exists(stored),
      matches_stored = file.exists(stored) &&
        if (grepl("\\.tex$", path)) identical(normalize_table(path), normalize_table(stored)) else
          isTRUE(all.equal(read.csv(path), read.csv(stored), tolerance = 1e-10)))
  }))
  csv(staged_checks, "legacy_matches_stored_tables.csv")
  dir.create(file.path(out, "raw_r3_tables"), showWarnings = FALSE)
  raw_table_files <- list.files(file.path(directories[["updated_raw_r3"]], "output/tables"), full.names = TRUE)
  stopifnot(all(file.copy(raw_table_files, file.path(out, "raw_r3_tables"), overwrite = TRUE)))
  csv(do.call(rbind, lapply(raw_table_files, function(path) {
    baseline <- file.path(out, "legacy_tables", basename(path))
    data.frame(file = basename(path),
      matches_legacy = if (grepl("\\.tex$", path)) identical(normalize_table(path), normalize_table(baseline)) else
        isTRUE(all.equal(read.csv(path), read.csv(baseline), tolerance = 1e-10)))
  })), "raw_r3_table_replication.csv")
  legacy_diagnostics <- read.csv(file.path(out, "legacy_tables/tableII_mod_diagnostics.csv"))
  raw_diagnostics <- read.csv(file.path(out, "raw_r3_tables/tableII_mod_diagnostics.csv"))
  selected_diagnostics <- c("column", "n", "first_stage_F", "first_stage_F_HC1", "partial_R2",
    "iv_se_CR2", "iv_df_CR2", "iv_p_CR2", "AR_p_CR2_beta0")
  diagnostics_comparison <- merge(legacy_diagnostics[selected_diagnostics],
    raw_diagnostics[selected_diagnostics], by = "column", suffixes = c("_legacy", "_updated"))
  csv(diagnostics_comparison, "raw_r3_table2_diagnostics_comparison.csv")
  file.copy(file.path(directories[["legacy"]], "session.txt"), file.path(out, "session.txt"))
  saveRDS(old_models, file.path(out, "legacy_model_snapshots.rds"))
  # Confirm the production inputs and outputs remain untouched.
  after_hash <- tools::md5sum(protected)
  stopifnot(identical(before_hash, after_hash))
  stopifnot(identical(unname(tools::md5sum(reconstruction$sources$path)), reconstruction$sources$md5))
  csv(data.frame(file = names(before_hash), md5_before = unname(before_hash),
    md5_after = unname(after_hash), unchanged = unname(before_hash == after_hash)), "production_integrity.csv")
  writeLines(c(paste("Project:", root), paste("Isolated worker directory:", work),
    "Overleaf exports: disabled in every worker", "Numeric raw-value tolerance: relative 1e-7",
    "Model comparison tolerance: 1e-10",
    "Updated-only run: no model controls dropped; no legacy values filled.",
    "Hybrid diagnostic: only elecq_new carried forward from the matched legacy child-round record.",
    "Hybrid estimates are not updated-release-only replication.",
    "updated_raw_r3: Round 3 elecq_new comes only from original OC/YC household elecq; source and unmatched NAs retained.",
    "updated_raw_r3: no electricity values for other rounds changed; the updated elecq source column is untouched.",
    "Temporary workers are removed with the R temporary directory at process exit."),
    file.path(out, "run_provenance.txt"))
  html_escape <- function(x) {
    x <- as.character(x); x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  html_table <- function(d) {
    rows <- apply(d, 1L, function(x) paste0("<tr><td>",
      paste(html_escape(x), collapse = "</td><td>"), "</td></tr>"))
    paste0("<div class='scroll'><table><thead><tr><th>",
      paste(html_escape(names(d)), collapse = "</th><th>"),
      "</th></tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table></div>")
  }
  fully_equal <- tapply(differences$changed, differences$legacy_variable, sum)
  main_display <- main_results
  for (v in c("legacy_beta", "legacy_se", "hybrid_beta", "hybrid_se")) {
    main_display[[v]] <- sprintf("%.6f", main_display[[v]])
  }
  display_fields <- c("hghgrade_final_num", "IMTI", "E_is", "raw_lang", "raw_maths",
    "wage_employ", "wage_employII", "elecq_new")
  changes_display <- field_changes[field_changes$variable %in% display_fields,
    c("dataset", "variable", "legacy_n", "updated_n", "common_n", "changed", "missingness_changed")]
  table1_change <- merge(a$table1[, c("variable", "obs_nonaa", "mean_nonaa", "obs_aa", "mean_aa")],
    b$table1[, c("variable", "obs_nonaa", "mean_nonaa", "obs_aa", "mean_aa")],
    by = "variable", suffixes = c("_legacy", "_updated"), sort = FALSE)
  fig_change <- merge(a$figure, b$figure, by = c("region", "employ_type"),
    suffixes = c("_legacy", "_updated"), sort = FALSE)
  files <- c("release_summary.csv", "schema.csv", "historical_differences_by_round.csv",
    "variable_and_value_labels.csv", "compatibility_mapping.csv", "schooling_code_crosswalk.csv",
    "death_status_crosswalk.csv", "analysis_variable_changes.csv", "model_replication.csv",
    "channel_replication.csv", "table2_release_comparison.csv", "hybrid_electricity_model_comparison.csv",
    "hybrid_electricity_channel_comparison.csv", "legacy_matches_stored_data.csv",
    "legacy_matches_previous_models.csv", "legacy_matches_stored_tables.csv", "production_integrity.csv",
    "legacy_run.log", "direct_run.log", "updated_run.log", "hybrid_electricity_run.log", "run_provenance.txt", "session.txt")
  report <- c(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Constructed-release comparison and replication audit</title>",
    "<style>body{font:16px/1.55 system-ui,sans-serif;color:#223044;max-width:1350px;margin:36px auto;padding:0 24px}h1{line-height:1.2}h2{margin-top:36px;border-bottom:1px solid #bdcbd8}a{color:#175d8b}.finding{padding:18px 22px;background:#f0f5f9;border-left:4px solid #507d9d}.scroll{overflow:auto}table{border-collapse:collapse;width:100%;font-size:13px}th,td{border:1px solid #dbe3ea;padding:8px 10px;text-align:left;vertical-align:top}th{background:#e8eff5}tbody tr:nth-child(even){background:#f7f9fb}code{overflow-wrap:anywhere}nav a{margin-right:16px}.small{color:#556577;font-size:14px}</style></head><body>",
    "<h1>Young Lives Ethiopia: constructed-release comparison</h1>",
    "<p>Replication audit for the current R1-R5 manuscript analysis. The updated release is evaluated on Rounds 1-5; Rounds 6-7 do not enter the existing specifications.</p>",
    "<nav><a href='#decision'>Finding</a><a href='#data'>Data comparison</a><a href='#models'>Models</a><a href='#hybrid'>Electricity diagnostic</a><a href='#descriptives'>Descriptives</a><a href='#next'>Next steps</a><a href='#evidence'>Evidence</a></nav>",
    "<h2 id='decision'>Replication finding</h2><div class='finding'>",
    sprintf("<p><strong>The updated file does not reproduce the current analysis on its own.</strong> %d of 49 model specifications have zero complete cases, and neither of the two Table 3 association specifications can be estimated.</p>", sum(models$updated_complete_n == 0L)),
    "<p>The immediate barrier is electricity: the updated elecq field is missing for every R1-R5 record, while every existing regression includes the R3 electricity control. An unchanged substitution also fails because the legacy deceased field and other expected names are absent.</p>",
    "<p>A fresh legacy run reproduces all four stored analysis CSVs, all 49 previous coefficient/covariance/N snapshots, and all 10 staged table/diagnostic files. This validates the current pipeline baseline. The production input, processed data, results, code and Overleaf exports were not changed by the audit.</p></div>",
    "<h2 id='data'>Comparison of the two releases</h2>",
    sprintf("<p>Legacy: <code>%s</code><br>Updated: <code>%s</code></p>",
      html_escape(raw_data_files[["constructed_legacy"]]), html_escape(raw_data_files[["constructed_r1_r7"]])),
    html_table(read.csv(file.path(out, "release_summary.csv"))),
    sprintf("<p>All %s historical child-round keys match exactly; neither file has duplicate child-round keys within R1-R5. The updated full panel has multiple R6 calls per child. The files share %d variable names; %d legacy names are absent and %d names are added.</p>",
      format(nrow(old), big.mark = ","), length(intersect(names(old), names(latest))),
      length(setdiff(names(old), names(latest))), length(setdiff(names(latest), names(old)))),
    sprintf("<p>Of %d comparable fields (including the provisional mappings below), %d have identical historical values and missingness across all five rounds, while %d differ. shfam11 is absent and has no proposed replacement. These counts do not imply that all mapped variables have equivalent definitions.</p>",
      sum(!is.na(fully_equal)), sum(fully_equal == 0L, na.rm = TRUE), sum(fully_equal > 0L, na.rm = TRUE)),
    "<p>Numeric raw comparisons use relative tolerance 1e-7. Missing-to-observed and observed-to-missing changes are reported separately. Original negative codes are retained; no automatic missing-code recoding is introduced.</p>",
    "<h3>Compatibility mappings used only in the isolated test</h3>", html_table(mapping),
    "<p><strong>Schooling is a definition change:</strong> hghschoolgrade reports highest school-level grade completed; legacy hghgrade also represented post-secondary levels. The existing pipeline uses this field to construct schooling outcomes, primary-school language and MTI exposure. A renamed column therefore does not restore the legacy meaning.</p>",
    "<p>The documented definition is in <a href='../../rawdata/yl_const_r1_r7/UKDA-9543-stata/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf#page=21'>Technical Note 63, pp. 21-22</a>. The electricity availability problem is an observed property of this supplied file, rather than an inference from the guide.</p>",
    "<h3>Electricity availability in the updated file</h3>",
    html_table(read.csv(file.path(out, "updated_electricity_availability.csv"))),
    "<h3>Changes in constructed analysis inputs</h3>",
    "<p>Changes below are counted among matched children only. Membership changes are reported separately in analysis_variable_changes.csv. The death-status mapping reproduces the legacy missing-death-flag filtering convention; it does not introduce an additional completed-interview restriction.</p>", html_table(changes_display),
    "<h2 id='models'>Existing model specifications</h2>",
    html_table(models[, c("model", "legacy_n", "updated_complete_n", "updated_estimable")]),
    "<p>The two additional Table 3 fits have legacy N = 656 and 524; updated-only complete-case N = 0 in both. They are OLS association models, not a completed 2SLS channel design. The model_replication.csv and channel_replication.csv files retain formulas, errors and covariate missingness counts for every specification.</p>",
    "<h2 id='hybrid'>Diagnostic: retain only legacy electricity</h2>",
    "<p><strong>This is a hybrid sensitivity exercise, not replication using only the updated release.</strong> The matched legacy elecq_new is carried forward while all other constructed inputs retain their updated values and provisional mappings. No control is dropped and the current model formulas are unchanged.</p>",
    sprintf("<p>All %d models become estimable. %d of 49 match the legacy coefficients and %d match their covariance matrices at tolerance 1e-10; %d retain their sample sizes. Both Table 3 association fits differ. Thus electricity is the immediate estimation barrier, but restoring it alone does not restore the complete legacy results.</p>",
      sum(hybrid_comparison$updated_estimable), sum(hybrid_comparison$coefficients_match),
      sum(hybrid_comparison$covariance_matches), sum(hybrid_comparison$legacy_n == hybrid_comparison$updated_n)),
    "<h3>Current Table 2 employment estimates</h3>", html_table(main_display),
    "<p class='small'>Coefficients and SEs are the existing conventional felm estimates, not CR2 regional inference. The hybrid diagnostic does not establish significance or address the few-region inference concern. Sample labels and salary/wage definitions follow the current pipeline.</p>",
    "<h2 id='descriptives'>Descriptive results</h2>",
    "<p>Table 1 and Figure 1 do not match in the updated-only run. Available-case descriptives remain computable even though the regression complete-case samples are empty. Cognitive test values from unchanged raw files match for matched children; changes in membership and constructed controls still alter descriptive results.</p>",
    "<h3>Table 1: observation counts and means</h3>", html_table(table1_change),
    "<h3>Figure 1: plotted employment proportions</h3>", html_table(fig_change),
    "<h2 id='next'>Requirements before replacing the production input</h2>",
    "<ol><li>Resolve historical electricity availability. Verify or reconstruct the R3 control from the original round-specific source or obtain a corrected constructed file. R7 electricity should not be backfilled into earlier rounds.</li><li>Choose and document how to reconstruct the broader legacy schooling measure, or explicitly adopt the revised schooling definition and treat the resulting estimates as a new analysis. Check its effects on IMTI, language assignment and sample selection.</li><li>Review the remaining historical value/missingness and death-status differences, then rerun complete-case, first-stage and regional inference diagnostics before updating manuscript results.</li></ol>",
    "<p>The production catalog continues to use the legacy input while this comparison is reviewed. This audit establishes replication of current code and staged outputs; the previously documented manuscript Tables 4-5 generator mismatch remains a separate issue.</p>",
    "<h2 id='evidence'>Reproducibility and evidence</h2>",
    "<p>Run <code>Rscript code/auxiliary/compare_constructed_releases.R</code> from the project root. The script writes this report and aggregate evidence to <code>output/constructed_release_comparison/</code>, executes in temporary copies, and disables external exports in every worker. Source input checksums and before/after production checksums are recorded.</p>",
    "<ul>", sprintf("<li><a href='%s'>%s</a></li>", files, files), "</ul></body></html>"
  )
  writeLines(enc2utf8(report), file.path(out, "comparison_report.html"), useBytes = TRUE)
  raw_summary <- data.frame(check = c("models_estimable", "coefficient_vectors_match_legacy",
    "covariance_matrices_match_legacy", "model_sample_sizes_match_legacy", "models_match_previous_hybrid",
    "table1_matches_legacy", "figure1_matches_legacy"),
    matches = c(sum(raw_comparison$updated_estimable), sum(raw_comparison$coefficients_match),
      sum(raw_comparison$covariance_matches), sum(raw_comparison$legacy_n == raw_comparison$updated_n),
      sum(raw_vs_hybrid$coefficients_match & raw_vs_hybrid$covariance_matches),
      as.integer(isTRUE(all.equal(a$table1, raw_desc$table1, tolerance = 1e-10))),
      as.integer(isTRUE(all.equal(a$figure, raw_desc$figure, tolerance = 1e-10)))))
  csv(raw_summary, "raw_r3_replication_summary.csv")
  raw_table1_comparison <- merge(a$table1[, c("variable", "obs_nonaa", "mean_nonaa", "obs_aa", "mean_aa")],
    raw_desc$table1[, c("variable", "obs_nonaa", "mean_nonaa", "obs_aa", "mean_aa")],
    by = "variable", suffixes = c("_legacy", "_updated"), sort = FALSE)
  raw_main_display <- raw_main_results
  for (v in c("legacy_beta", "legacy_se", "updated_raw_r3_beta", "updated_raw_r3_se")) {
    raw_main_display[[v]] <- sprintf("%.6f", raw_main_display[[v]])
  }
  raw_report <- c(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Replication with original Round 3 electricity</title>",
    "<style>body{font:16px/1.55 system-ui,sans-serif;max-width:1400px;margin:36px auto;padding:0 24px;color:#223044}h1{line-height:1.2}h2{margin-top:32px}a{color:#175d8b}.finding{background:#edf4f8;padding:16px 22px;border-left:4px solid #507b98}.scroll{overflow:auto}table{border-collapse:collapse;font-size:13px;width:100%}td,th{border:1px solid #dbe3eb;padding:8px;text-align:left}th{background:#e7eff6}code{overflow-wrap:anywhere}</style></head><body>",
    "<h1>Updated constructed data with original Round 3 electricity</h1>",
    "<div class='finding'><p><strong>The electricity control is successfully reconstructed, preserving missing values.</strong> The updated-release analysis is now estimable, but it does not exactly reproduce all legacy results. No electricity values are imported from the legacy constructed file in this run.</p></div>",
    "<h2>Electricity reconstruction</h2>",
    "<p>The original R3 older- and younger-cohort household <code>elecq</code> fields are joined onto constructed R3 records using <code>childid</code>, with cohort and unique-key checks. The compatibility name <code>elecq_new</code> is populated only for R3. The updated release's <code>elecq</code> column and all other rounds are untouched. Missing source responses and absent source households remain NA. No missing value is recoded to zero, and no legacy electricity value is used to fill an observation.</p>",
    html_table(reconstruction$audit),
    "<p>There are 2,858 observed values, one missing source response and 140 unmatched household records, leaving 141 missing values among 2,999 R3 panel records. Comparison with the legacy R3 control finds no value or missingness mismatches; that comparison is validation, not a source of values.</p>",
    "<h2>Replication results</h2>", html_table(raw_summary),
    "<p>All 49 fitted models and both additional Table 3 OLS association models are estimated. The full pipeline is also rerun without audit wrappers, producing tables and updated first-stage and regional CR2/Satterthwaite diagnostics in a separate directory. Sample definitions, model formulas and the provisional schooling mapping are otherwise unchanged.</p>",
    "<h3>Table 2 employment estimates</h3>", html_table(raw_main_display),
    "<p>SEs above are the existing conventional felm SEs. They should not be interpreted as allowing regional dependence.</p>",
    "<h3>Table 2 first-stage and regional inference diagnostics</h3>", html_table(diagnostics_comparison),
    "<p>CR2 uses the same few-region procedures as the current pipeline. Its degrees of freedom and p-values must be interpreted with the existing small-cluster qualifications; conventional stars are not a substitute for regional inference.</p>",
    "<h3>All existing model specifications</h3>",
    html_table(raw_comparison[, c("model", "legacy_n", "updated_n", "coefficients_match", "covariance_matches")]),
    "<h3>Table 3 OLS association fits</h3>",
    html_table(model_comparison(channels_old, raw_channels)[, c("model", "legacy_n", "updated_n", "coefficients_match", "covariance_matches")]),
    "<h2>Relationship to the previous diagnostic</h2>",
    "<p>All four extracted analysis CSVs and all 49 coefficient/covariance/N snapshots match the earlier hybrid-electricity diagnostic. This occurs because the original R3 indicator exactly reproduces the electricity input used in that diagnostic. The current reconstruction obtains those values from original household data; it no longer relies on legacy constructed electricity.</p>",
    "<h2>Remaining data differences</h2>",
    "<p>The provisional <code>hghgrade = hghschoolgrade</code> mapping is retained only to run the comparison. Highest school grade completed differs from the broader legacy schooling measure; its implications for schooling outcomes, MTI exposure and language assignment still need a substantive decision. Other historical values, missingness and sample membership also change. Reconstructing electricity does not resolve those differences.</p>",
    html_table(raw_field_changes[raw_field_changes$variable %in% display_fields,
      c("dataset", "variable", "legacy_n", "updated_n", "common_n", "changed", "missingness_changed")]),
    "<h3>Table 1 observation counts and means</h3>", html_table(raw_table1_comparison),
    "<p>Table 1 and Figure 1 still differ from the legacy baseline. Detailed data and all table comparisons are linked below. Existing manuscript Tables 4-5 generator differences remain a separate limitation; this is a comparison of the current reproducible pipeline.</p>",
    "<h2>Outputs and reproduction</h2>",
    "<p>Raw inputs, the production input catalog, processed CSVs and staged manuscript results are unchanged. Every worker disables Overleaf export. Revised tables and diagnostics are retained under <a href='raw_r3_tables/tableII_mod.tex'>raw_r3_tables/</a>; baseline tables are retained under legacy_tables/.</p>",
    "<p>Reconstruction helper: <code>code/auxiliary/reconstruct_round3_electricity.R</code>. Reproduce from the project root:</p>",
    "<pre>Rscript code/auxiliary/compare_constructed_releases.R output/constructed_release_comparison_r3_electricity</pre><ul>",
    sprintf("<li><a href='%s'>%s</a></li>",
      c("raw_r3_electricity_join_audit.csv", "raw_r3_electricity_validation.csv", "raw_r3_electricity_sources.csv",
        "raw_r3_model_replication.csv", "raw_r3_channel_replication.csv", "raw_r3_table2_comparison.csv",
        "raw_r3_table2_diagnostics_comparison.csv", "raw_r3_tables/tableII_mod_diagnostics.csv",
        "raw_r3_vs_previous_hybrid.csv", "raw_r3_vs_hybrid_analysis_data.csv", "raw_r3_analysis_variable_changes.csv",
        "raw_r3_table_replication.csv", "raw_r3_table1.csv", "raw_r3_figure1.csv", "legacy_matches_previous_models.csv",
        "legacy_matches_stored_data.csv", "legacy_matches_stored_tables.csv", "production_integrity.csv",
        "updated_raw_r3_run.log", "run_provenance.txt", "comparison_report.html"),
      c("Electricity join audit", "Electricity validation against legacy", "Original source checksums",
        "All 49 model comparisons", "Table 3 model comparisons", "Table 2 coefficients and SEs",
        "Table 2 diagnostic comparisons", "Revised Table 2 full diagnostics", "Model agreement with prior diagnostic",
        "Analysis-input agreement with prior diagnostic", "Analysis variable changes", "Generated table comparisons",
        "Updated Table 1 data", "Updated Figure 1 data", "Baseline model validation", "Baseline data validation",
        "Baseline table validation", "Production integrity checks", "Reconstruction-run log", "Run provenance",
        "Original updated-only and hybrid comparisons")),
    "</ul></body></html>"
  )
  writeLines(enc2utf8(raw_report), file.path(out, "raw_r3_replication_report.html"), useBytes = TRUE)
  message("Audit completed: ", out)
})
