# Run from the project root:
# Rscript code/auxiliary/attribute_constructed_differences.R [output_directory]
# Diagnostic counterfactuals ONLY: legacy blocks are restored to identify causes,
# not as an approved harmonization. No production data, code or results changed.
local({
  root <- normalizePath(".")
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args)) args[1L] else "temp/constructed_difference_attribution"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out)
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  source("code/auxiliary/reconstruct_round3_electricity.R", local = TRUE)
  protected <- c(raw_data_files[c("constructed_legacy", "constructed_r1_r7")],
    list.files("data", recursive = TRUE, full.names = TRUE),
    list.files("code", recursive = TRUE, full.names = TRUE),
    list.files("output/tables", recursive = TRUE, full.names = TRUE),
    list.files("output/figures", recursive = TRUE, full.names = TRUE))
  protected <- protected[!dir.exists(protected)]
  hashes <- tools::md5sum(protected)
  csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
  old <- haven::read_dta(raw_data_files[["constructed_legacy"]])
  full_new <- haven::read_dta(raw_data_files[["constructed_r1_r7"]])
  key <- function(d) paste(d$childid, d$round, sep = ":")
  new <- full_new[full_new$round %in% 1:5, ]
  stopifnot(!anyDuplicated(key(old)), !anyDuplicated(key(new)), setequal(key(old), key(new)))
  new <- new[match(key(old), key(new)), ]
  aliases <- c(underweight = "underweight_child", stunting = "stunting_child",
    thinness = "thinness_child", hghgrade = "hghschoolgrade", sv_new = "sv", cd_new = "cd",
    drwaterq_new = "drwaterq", toiletq_new = "toiletq", elecq_new = "elecq", cookingq_new = "cookingq")
  for (v in names(aliases)) new[[v]] <- new[[aliases[[v]]]]
  new$deceased <- ifelse(new$status == 2, 1, NA_real_)
  reconstructed <- reconstruct_round3_electricity(new, root)
  new <- reconstructed$data
  csv(reconstructed$audit, "electricity_join_audit.csv")
  groups <- list(schooling = "hghgrade", early_school = c("preprim", "agegr1"),
    school_type = "entype", death_flag = "deceased")
  csv(data.frame(block = rep(names(groups), lengths(groups)),
    fields = unlist(groups, use.names = FALSE)), "attribution_blocks.csv")

  # Aggregate record-level diagnostics; no participant identifiers are exported.
  missing_changes <- list()
  for (v in c("preprim", "agegr1", "entype")) {
    i <- !is.na(old[[v]]) & is.na(new[[v]])
    d <- as.data.frame(table(round = old$round[i], cohort = old$yc[i],
      status = haven::as_factor(new$status[i]), inround = new$inround[i]))
    d <- d[d$Freq > 0, ]; d$variable <- v
    missing_changes[[v]] <- d
    stopifnot(all(new$inround[i] == 0))
  }
  csv(do.call(rbind, missing_changes), "education_missingness_by_interview.csv")
  code <- function(x) ifelse(is.na(x), "missing", as.character(as.numeric(x)))
  grade <- as.data.frame(table(round = old$round, cohort = old$yc,
    legacy_grade = code(old$hghgrade), updated_grade = code(new$hghgrade)))
  grade <- grade[grade$Freq > 0 & as.character(grade$legacy_grade) != as.character(grade$updated_grade), ]
  csv(grade, "changed_schooling_codes.csv")
  changed_deaths <- is.na(old$deceased) != is.na(new$deceased)
  deaths <- data.frame(round = old$round[changed_deaths], cohort = old$yc[changed_deaths],
    legacy_deceased = !is.na(old$deceased[changed_deaths]),
    updated_status = as.character(haven::as_factor(new$status[changed_deaths])),
    updated_inround = new$inround[changed_deaths])
  csv(aggregate(list(records = rep(1L, nrow(deaths))), deaths, sum), "changed_death_flags_aggregate_rows.csv")
  csv(data.frame(changed_child_rounds = sum(changed_deaths),
    affected_participants = length(unique(old$childid[changed_deaths]))), "death_flag_change_summary.csv")

  # Reuse the already validated isolated estimation worker, without executing
  # the parent audit. All scenarios run its same extraction/model expressions.
  audit_ast <- parse("code/auxiliary/compare_constructed_releases.R")[[1L]][[2L]]
  assignments <- as.list(audit_ast)[-1L]
  worker_expression <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
    identical(e[[2L]], as.name("worker")), assignments)
  stopifnot(length(worker_expression) == 1L)
  worker <- eval(worker_expression[[1L]][[3L]])
  work <- tempfile("release_attribution_")
  dir.create(work)
  worker_path <- file.path(work, "worker.R")
  writeLines(c("worker <-", deparse(worker), "eval(body(worker), envir = .GlobalEnv)"), worker_path)
  scenarios <- data.frame(scenario = c("legacy_reference", sprintf("factorial_%02d", 0:15),
    "restored_all_five", "updated_except_wealth"),
    mask = c(NA_integer_, 0:15, 0L, 15L), stringsAsFactors = FALSE)
  # In each mask, 1 means retain that UPDATED block; 0 restores the LEGACY block.
  # Unlisted fields remain updated, so the all-zero scenario tests residual causes.
  for (j in seq_along(groups)) scenarios[[names(groups)[j]]] <- ifelse(is.na(scenarios$mask),
    FALSE, bitwAnd(scenarios$mask, bitwShiftL(1L, j - 1L)) != 0)
  scenarios$wealth <- !scenarios$scenario %in% c("legacy_reference", "restored_all_five", "updated_except_wealth")
  csv(scenarios, "scenarios.csv")
  snapshots <- list(); channels <- list(); descriptions <- list(); data_summaries <- list()
  analysis_files <- c("aa_samp.csv", "non_aa_samp.csv", "ylsample_yc.csv", "schsur_yl_joined.csv")
  baseline_data <- list(); latest_data <- list()
  for (s in seq_len(nrow(scenarios))) {
    name <- scenarios$scenario[s]
    sandbox <- file.path(work, name)
    dir.create(sandbox)
    stopifnot(file.copy(file.path(root, "code"), sandbox, recursive = TRUE),
      file.copy(file.path(root, "README.md"), sandbox),
      file.symlink(file.path(root, "rawdata"), file.path(sandbox, "rawdata")))
    dir.create(file.path(sandbox, "data"))
    trial <- if (name == "legacy_reference") old else new
    if (name != "legacy_reference") {
      for (g in names(groups)) if (!scenarios[[g]][s]) {
        for (v in groups[[g]]) trial[[v]] <- old[[v]]
      }
      if (!scenarios$wealth[s]) for (v in c("wi_new", "hq_new")) trial[[v]] <- old[[v]]
    }
    saveRDS(trial, file.path(sandbox, "constructed_candidate.rds"))
    for (script in c("01_extract_clean.R", "02_cleaning_yc.R")) {
      path <- file.path(sandbox, "code", script)
      text <- readLines(path)
      needle <- 'read_dta(raw_data_files[["constructed_legacy"]])'
      stopifnot(sum(grepl(needle, text, fixed = TRUE)) == 1L)
      writeLines(gsub(needle, 'readRDS("constructed_candidate.rds")', text, fixed = TRUE), path)
    }
    for (script in c("07_table_main.R", "09_table_rev.R")) {
      path <- file.path(sandbox, "code", script)
      writeLines(gsub('commandArgs(trailingOnly = TRUE)', 'character()', readLines(path), fixed = TRUE), path)
    }
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c(shQuote(worker_path), shQuote(sandbox), name),
      stdout = file.path(out, paste0(name, ".log")), stderr = file.path(out, paste0(name, ".log")))
    if (status != 0L) stop("Scenario failed: ", name)
    snapshots[[name]] <- readRDS(file.path(sandbox, "models49.rds"))
    channels[[name]] <- readRDS(file.path(sandbox, "channel_models.rds"))
    descriptions[[name]] <- readRDS(file.path(sandbox, "descriptives.rds"))
    stopifnot(all(vapply(snapshots[[name]], `[[`, logical(1), "ok")))
    for (f in analysis_files) {
      d <- read.csv(file.path(sandbox, "data", f))
      if (name == "legacy_reference") baseline_data[[f]] <- d
      if (name == "factorial_15") latest_data[[f]] <- d
      if (name %in% c("factorial_00", "restored_all_five")) {
        a <- baseline_data[[f]]
        data_summaries[[paste(name, f)]] <- data.frame(scenario = name, dataset = f, values_match =
          isTRUE(all.equal(a, d, check.attributes = FALSE, tolerance = 1e-10)))
      }
    }
    message("Completed scenario ", s, "/", nrow(scenarios), ": ", name)
  }
  csv(do.call(rbind, data_summaries), "restored_analysis_data_matches.csv")
  baseline <- snapshots[["legacy_reference"]]
  prior <- readRDS("temp/constructed_release_comparison_r3_electricity/legacy_model_snapshots.rds")
  prior_new <- readRDS("temp/constructed_release_comparison_r3_electricity/raw_r3_model_snapshots.rds")
  model_equal <- function(a, b) isTRUE(all.equal(a$coef, b$coef, tolerance = 1e-10)) &&
    isTRUE(all.equal(a$vcov, b$vcov, tolerance = 1e-10)) && a$n == b$n
  stopifnot(all(vapply(names(baseline), function(m) model_equal(baseline[[m]], prior[[m]]), logical(1))),
    all(vapply(names(baseline), function(m) model_equal(snapshots$factorial_15[[m]], prior_new[[m]]), logical(1))))
  model_results <- do.call(rbind, lapply(names(snapshots), function(s) {
    do.call(rbind, lapply(names(baseline), function(m) {
      a <- baseline[[m]]; b <- snapshots[[s]][[m]]
      data.frame(scenario = s, model = m, legacy_n = a$n, scenario_n = b$n,
        coefficients_match = isTRUE(all.equal(a$coef, b$coef, tolerance = 1e-10)),
        covariance_matches = isTRUE(all.equal(a$vcov, b$vcov, tolerance = 1e-10)))
    }))
  }))
  csv(model_results, "model_attribution.csv")
  fully_restored <- model_results[model_results$scenario == "restored_all_five", ]
  stopifnot(all(fully_restored$coefficients_match), all(fully_restored$covariance_matches),
    all(fully_restored$legacy_n == fully_restored$scenario_n))
  summary <- do.call(rbind, lapply(names(snapshots), function(s) {
    d <- model_results[model_results$scenario == s, ]
    data.frame(scenario = s, coefficient_vectors_matching_legacy = sum(d$coefficients_match),
      covariances_matching_legacy = sum(d$covariance_matches), sample_sizes_matching_legacy = sum(d$scenario_n == d$legacy_n),
      table1_matches_legacy = isTRUE(all.equal(descriptions[[s]]$table1, descriptions$legacy_reference$table1, tolerance = 1e-10)),
      figure1_matches_legacy = isTRUE(all.equal(descriptions[[s]]$figure, descriptions$legacy_reference$figure, tolerance = 1e-10)))
  }))
  csv(merge(scenarios, summary, by = "scenario", sort = FALSE), "scenario_summary.csv")
  # Per-model Shapley decomposition: average the coefficient and N increments
  # over every ordering of the four updated blocks. This allocates interactions
  # without claiming an order-independent causal effect of a data definition.
  main_models <- c("iv6", "iv1", "iv6_nonAm", "iv1_nonAm")
  term <- function(m) tail(grep("IMTI", names(m$coef), fixed = TRUE), 1L)
  main <- do.call(rbind, lapply(names(snapshots), function(s) do.call(rbind,
    lapply(seq_along(main_models), function(i) {
      m <- snapshots[[s]][[main_models[i]]]; j <- term(m)
      data.frame(scenario = s, column = i, model = main_models[i],
        beta = unname(m$coef[j]), se = sqrt(diag(m$vcov))[j], n = m$n)
    }))))
  rownames(main) <- NULL; csv(main, "table2_by_scenario.csv")
  shapley <- list()
  for (m in main_models) for (j in seq_along(groups)) {
    beta_contribution <- n_contribution <- 0
    for (mask in 0:15) {
      bit <- bitwShiftL(1L, j - 1L)
      if (bitwAnd(mask, bit) != 0) next
      k <- sum(as.integer(intToBits(mask))[1:4])
      weight <- factorial(k) * factorial(3 - k) / factorial(4)
      a <- snapshots[[sprintf("factorial_%02d", mask)]][[m]]
      b <- snapshots[[sprintf("factorial_%02d", mask + bit)]][[m]]
      beta_contribution <- beta_contribution + weight * (b$coef[term(b)] - a$coef[term(a)])
      n_contribution <- n_contribution + weight * (b$n - a$n)
    }
    shapley[[length(shapley) + 1L]] <- data.frame(model = m, block = names(groups)[j],
      beta_contribution = unname(beta_contribution), n_contribution = n_contribution)
  }
  shapley <- do.call(rbind, shapley); csv(shapley, "table2_shapley_attribution.csv")
  saveRDS(snapshots, file.path(out, "model_snapshots_by_scenario.rds"))
  saveRDS(channels, file.path(out, "channel_snapshots_by_scenario.rds"))
  saveRDS(descriptions, file.path(out, "descriptives_by_scenario.rds"))
  # Aggregate child-level membership and field differences in the extracted data.
  fields <- list(); membership <- list()
  for (f in analysis_files) {
    a <- baseline_data[[f]]; b <- latest_data[[f]]
    id <- if ("child_id" %in% names(a)) "child_id" else "childid"
    matched <- match(a[[id]], b[[id]]); keep <- !is.na(matched)
    membership[[f]] <- data.frame(dataset = f, legacy_n = nrow(a), updated_n = nrow(b),
      common = sum(keep), legacy_only = sum(!keep), updated_only = sum(!b[[id]] %in% a[[id]]))
    for (v in intersect(names(a), names(b))) {
      x <- a[[v]][keep]; y <- b[[v]][matched[keep]]
      valid <- !is.na(x) & !is.na(y)
      eq <- is.na(x) & is.na(y); eq[valid] <- x[valid] == y[valid]
      if (any(!eq)) fields[[length(fields) + 1L]] <- data.frame(dataset = f, variable = v,
        changed = sum(!eq), changed_observed_values = sum(valid & !eq),
        observed_to_missing = sum(!is.na(x) & is.na(y)), missing_to_observed = sum(is.na(x) & !is.na(y)))
    }
  }
  csv(do.call(rbind, fields), "changed_analysis_fields.csv")
  csv(do.call(rbind, membership), "analysis_membership.csv")
  # Save aggregate Table 2 complete-case membership transitions with attribution.
  # Formula variables are fixed; comparison always uses matched child identifiers.
  source("code/helpers/analysis_helpers.R", local = TRUE)
  complete_case_changes <- list()
  controls <- c("chsex", "zbfa", "stunting", "caredu_r1", "careage_r1", "caresex_r1", "hhsize",
    "wi_new", "hq_new", "cd_new", "elecq_new", "ownlandhse_r1", "foodsec_r3", "IMTI", "E_is")
  for (i in seq_along(main_models)) {
    a <- baseline_data[["non_aa_samp.csv"]]; b <- latest_data[["non_aa_samp.csv"]]
    a <- a[!is.na(a$raw_lang) & a$raw_lang != 0, ]; b <- b[!is.na(b$raw_lang) & b$raw_lang != 0, ]
    if (i > 2L) {a <- a[!is.na(a$region) & a$region != "Amhara", ]; b <- b[!is.na(b$region) & b$region != "Amhara", ]}
    outcome <- if (i %% 2L) "wage_employII" else "wage_employ"
    a_ids <- a$child_id[complete.cases(a[c(outcome, controls)])]
    b_ids <- b$child_id[complete.cases(b[c(outcome, controls)])]
    complete_case_changes[[i]] <- data.frame(column = i, legacy_n = length(a_ids), updated_n = length(b_ids),
      retained = length(intersect(a_ids, b_ids)), lost = length(setdiff(a_ids, b_ids)), gained = length(setdiff(b_ids, a_ids)))
  }
  csv(do.call(rbind, complete_case_changes), "table2_sample_transitions.csv")
  # Changes within the shared estimation samples distinguish value changes
  # from gains in complete-case observations. No model is exported as a result.
  fixed_sample_results <- list(); gained_reasons <- list()
  for (i in seq_along(main_models)) {
    a <- baseline_data[["non_aa_samp.csv"]]; b <- latest_data[["non_aa_samp.csv"]]
    a <- a[!is.na(a$raw_lang) & a$raw_lang != 0, ]; b <- b[!is.na(b$raw_lang) & b$raw_lang != 0, ]
    if (i > 2L) {a <- a[!is.na(a$region) & a$region != "Amhara", ]; b <- b[!is.na(b$region) & b$region != "Amhara", ]}
    outcome <- if (i %% 2L) "wage_employII" else "wage_employ"
    av <- a[complete.cases(a[c(outcome, controls)]), ]; bv <- b[complete.cases(b[c(outcome, controls)]), ]
    common <- intersect(av$child_id, bv$child_id)
    aa <- av[match(common, av$child_id), ]; bb <- bv[match(common, bv$child_id), ]
    formula <- as.formula(paste(outcome, "~ chsex + zbfa + stunting + caredu_r1 + careage_r1 + factor(caresex_r1)",
      "+ hhsize + wi_new + hq_new + cd_new + elecq_new + ownlandhse_r1 + factor(foodsec_r3) | 0 | (IMTI ~ E_is)"))
    fits <- lapply(list(aa, bb), function(d) {
      d[] <- lapply(d, function(x) if (is.character(x)) factor(x) else x)
      lfe::felm(formula, data = d)
    })
    fixed_sample_results[[i]] <- data.frame(column = i, common_n = length(common),
      IMTI_values_changed = sum(aa$IMTI != bb$IMTI),
      legacy_beta = unname(tail(coef(fits[[1L]]), 1L)),
      updated_beta_same_sample = unname(tail(coef(fits[[2L]]), 1L)),
      outcome_values_changed = sum(aa[[outcome]] != bb[[outcome]]),
      instrument_values_changed = sum(aa$E_is != bb$E_is))
    gained <- setdiff(bv$child_id, av$child_id)
    matched <- match(gained, a$child_id)
    for (j in seq_along(gained)) {
      cause <- if (is.na(matched[j])) "not in legacy selected cohort" else {
        missing <- names(a[c(outcome, controls)])[is.na(a[matched[j], c(outcome, controls)])]
        paste(missing, collapse = "; ")
      }
      gained_reasons[[length(gained_reasons) + 1L]] <- data.frame(column = i, reason_legacy_incomplete = cause)
    }
  }
  csv(do.call(rbind, fixed_sample_results), "table2_fixed_sample_diagnostics.csv")
  gained_reasons <- do.call(rbind, gained_reasons)
  csv(aggregate(list(records = rep(1L, nrow(gained_reasons))), gained_reasons, sum), "table2_gained_case_reasons.csv")
  stopifnot(identical(hashes, tools::md5sum(protected)))
  csv(data.frame(path = names(hashes), md5 = unname(hashes), unchanged = TRUE), "production_integrity.csv")
  writeLines(capture.output(sessionInfo()), file.path(out, "session.txt"))
  message("Attribution evidence saved: ", out)
})
