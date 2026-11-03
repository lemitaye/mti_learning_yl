# Summarize the aggregate outputs of attribute_constructed_differences.R.
# Rscript code/auxiliary/write_constructed_attribution_report.R [audit_directory]
write_constructed_attribution_report <- function(out = "output/constructed_difference_attribution") {
  read <- function(name) read.csv(file.path(out, name), stringsAsFactors = FALSE)
  csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)
  summary <- read("scenario_summary.csv")
  main <- read("table2_by_scenario.csv")
  snapshots <- readRDS(file.path(out, "model_snapshots_by_scenario.rds"))
  channels <- readRDS(file.path(out, "channel_snapshots_by_scenario.rds"))
  descriptions <- readRDS(file.path(out, "descriptives_by_scenario.rds"))
  equal <- function(a, b) isTRUE(all.equal(a$coef, b$coef, tolerance = 1e-10)) &&
    isTRUE(all.equal(a$vcov, b$vcov, tolerance = 1e-10)) && a$n == b$n
  stopifnot(all(vapply(seq_along(channels$legacy_reference), function(i)
    equal(channels$legacy_reference[[i]], channels$restored_all_five[[i]]), logical(1))))
  marginal <- do.call(rbind, lapply(c("schooling", "early_school", "school_type", "death_flag", "wealth"), function(block) {
    alternative <- c(schooling = "factorial_14", early_school = "factorial_13",
      school_type = "factorial_11", death_flag = "factorial_07", wealth = "updated_except_wealth")[[block]]
    d <- snapshots[[alternative]]; u <- snapshots$factorial_15
    data.frame(block = block, models_changed_when_restored = sum(vapply(names(u), function(m)
      !isTRUE(all.equal(d[[m]]$coef, u[[m]]$coef, tolerance = 1e-10)), logical(1))),
      model_sample_sizes_changed = sum(vapply(names(u), function(m) d[[m]]$n != u[[m]]$n, logical(1))))
  }))
  csv(marginal, "block_effects_in_updated_analysis.csv")
  # Confirm school-type missingness has no independent effect in any tested
  # combination, rather than inferring that from counts of baseline matches.
  for (mask in 0:15) if (bitwAnd(mask, 4L) == 0L) {
    a <- snapshots[[sprintf("factorial_%02d", mask)]]
    b <- snapshots[[sprintf("factorial_%02d", mask + 4L)]]
    stopifnot(all(vapply(names(a), function(m) equal(a[[m]], b[[m]]), logical(1))))
  }
  # Independent raw-data corroboration of the housing-quality revision.
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  old <- haven::read_dta(raw_data_files[["constructed_legacy"]])
  new <- haven::read_dta(raw_data_files[["constructed_r1_r7"]])
  new <- new[match(paste(old$childid, old$round), paste(new$childid, new$round)), ]
  i <- which(old$round == 3 & !is.na(old$wi_new) & !is.na(new$wi_new) & abs(old$wi_new - new$wi_new) > 1e-7)
  stopifnot(length(i) == 1L, old$yc[i] == 1)
  hh <- haven::read_dta("rawdata/yl_r3/UKDA-6853-stata/stata/stata13/r3_yc/ethiopia/et_yc_householdlevel.dta")
  j <- match(old$childid[i], hh$childid)
  wealth <- data.frame(round = 3, cohort = "younger", region = as.character(haven::as_factor(old$region[i])),
    raw_wall_code = as.numeric(hh$wallr3[j]), raw_wall_label = as.character(haven::as_factor(hh$wallr3[j])),
    legacy_hq = as.numeric(old$hq_new[i]), updated_hq = as.numeric(new$hq_new[i]),
    legacy_wi = as.numeric(old$wi_new[i]), updated_wi = as.numeric(new$wi_new[i]))
  csv(wealth, "housing_revision_source_evidence.csv")
  # Raw R5 interview records corroborate the two status discrepancies, without
  # assuming that the timing of an unobserved death record can be inferred.
  status_checks <- do.call(rbind, lapply(c("oc", "yc"), function(co) {
    p <- file.path("rawdata/yl_r5/UKDA-8357-stata/stata/stata13/ethiopia_r5", paste0("et", co, "_ch_anon"),
      if (co == "oc") "et_r5_occh_olderchild.dta" else "et_r5_ycch_youngerchild.dta")
    raw <- haven::read_dta(p)
    ix <- old$round == 5 & old$yc == as.integer(co == "yc") & !is.na(old$deceased) & new$status == 0
    ids <- as.numeric(sub("ET", "", old$childid[ix]))
    data.frame(cohort = co, legacy_deceased_updated_interviewed = sum(ix),
      present_in_original_R5_child_file = sum(ids %in% raw$CHILDCODE))
  }))
  csv(status_checks, "death_status_raw_interview_check.csv")
  grade <- read("changed_schooling_codes.csv")
  grade_summary <- do.call(rbind, lapply(c(4L, 5L), function(r) {
    d <- grade[grade$round == r, ]
    data.frame(round = r, changed = sum(d$Freq),
      changed_nonmissing_values = sum(d$Freq[d$legacy_grade != "missing" & d$updated_grade != "missing"]),
      observed_to_missing = sum(d$Freq[d$updated_grade == "missing"]),
      missing_to_observed = sum(d$Freq[d$legacy_grade == "missing"]))
  }))
  gained_grade <- old$round == 5 & is.na(old$hghgrade) & !is.na(new$hghschoolgrade)
  lost_grade <- old$round == 5 & !is.na(old$hghgrade) & is.na(new$hghschoolgrade)
  stopifnot(sum(gained_grade) == 447L, sum(lost_grade) == 189L,
    all(new$inround[gained_grade] == 1), all(new$inround[lost_grade] == 0))
  csv(grade_summary, "schooling_change_summary.csv")
  selection <- c("legacy_reference", "factorial_01", "factorial_08", "factorial_15")
  labels <- c("Legacy", "Update schooling only", "Update death status only", "Updated + original R3 electricity")
  table2 <- do.call(rbind, lapply(seq_along(selection), function(i) {
    d <- main[main$scenario == selection[i], ]; d <- d[order(d$column), ]
    data.frame(scenario = labels[i], salary_nonAA = sprintf("%.6f (N=%d)", d$beta[1], d$n[1]),
      wage_nonAA = sprintf("%.6f (N=%d)", d$beta[2], d$n[2]),
      salary_nonAmhara = sprintf("%.6f (N=%d)", d$beta[3], d$n[3]),
      wage_nonAmhara = sprintf("%.6f (N=%d)", d$beta[4], d$n[4]))
  }))
  csv(table2, "table2_source_decomposition.csv")
  channel_summary <- do.call(rbind, lapply(c("legacy_reference", "factorial_01", "factorial_08", "factorial_15", "restored_all_five"), function(s) {
    do.call(rbind, lapply(seq_along(channels[[s]]), function(i) {
      m <- channels[[s]][[i]]
      data.frame(scenario = s, sample = if (i == 1) "non-Addis" else "excluding Amhara", n = m$n,
        interaction_coefficient = unname(m$coef["inst"]))
    }))
  }))
  csv(channel_summary, "channel_source_decomposition.csv")
  esc <- function(x) {
    x <- as.character(x); x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  tab <- function(d) paste0("<div class='scroll'><table><thead><tr><th>",
    paste(esc(names(d)), collapse = "</th><th>"), "</th></tr></thead><tbody>",
    paste(apply(d, 1L, function(r) paste0("<tr><td>", paste(esc(r), collapse = "</td><td>"), "</td></tr>")), collapse = "\n"),
    "</tbody></table></div>")
  links <- c("scenarios.csv", "scenario_summary.csv", "model_attribution.csv", "block_effects_in_updated_analysis.csv",
    "table2_source_decomposition.csv", "table2_sample_transitions.csv", "table2_gained_case_reasons.csv",
    "table2_fixed_sample_diagnostics.csv", "table2_shapley_attribution.csv", "changed_schooling_codes.csv",
    "schooling_change_summary.csv", "education_missingness_by_interview.csv", "death_flag_change_summary.csv",
    "changed_death_flags_aggregate_rows.csv", "death_status_raw_interview_check.csv", "housing_revision_source_evidence.csv",
    "changed_analysis_fields.csv", "analysis_membership.csv", "restored_analysis_data_matches.csv",
    "channel_source_decomposition.csv", "production_integrity.csv", "session.txt")
  report <- c(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>Sources of the remaining constructed-release differences</title>",
    "<style>body{font:16px/1.55 system-ui,sans-serif;color:#223044;max-width:1400px;margin:36px auto;padding:0 24px}h1{line-height:1.2}h2{margin-top:32px}a{color:#175d8b}.finding{background:#edf4f8;padding:16px 22px;border-left:4px solid #507b98}.scroll{overflow:auto}table{border-collapse:collapse;font-size:13px;width:100%}td,th{border:1px solid #dbe3eb;padding:8px;text-align:left}th{background:#e7eff6}code{overflow-wrap:anywhere}</style></head><body>",
    "<h1>Sources of the remaining release differences</h1>",
    "<div class='finding'><p><strong>The remaining differences are traced to schooling definitions/coverage, historical education-field missingness, revised death/interview status, and one housing-quality/wealth revision.</strong> A controlled diagnostic restoring five field blocks reproduces all four legacy analysis datasets, all 49 coefficient/covariance/N snapshots, both additional Table 3 association fits, Table 1 and Figure 1. Electricity is sourced from the original R3 household data throughout the updated scenarios.</p></div>",
    "<p>These restorations identify the sources of differences. They are not an approved replacement dataset, and they do not establish that older definitions or status flags are preferable.</p>",
    "<h2>1. Schooling changes are the principal source of the employment differences</h2>",
    "<p>The legacy do-file builds Round 5 hghgrade from the most recently completed education-history record during 2013-2016, with post-secondary and university categories and a conditional R4 fallback. The updated hghschoolgrade is explicitly restricted to school-level completion. Using it as an alias changes both the grade outcome and schooling-based IMTI, even when the original test scores and employment activity records remain unchanged.</p>",
    tab(grade_summary),
    "<p>Examples include legacy vocational code 13 becoming school grade 10, and university code 14 becoming grade 12. The change is not a single numeric recode: missingness changes substantially. All 447 newly observed R5 grades occur among interviewed participants; all 189 R5 grade values lost to missingness occur where inround = 0. The new construction code is unavailable, so this identifies observed transitions and the documented definition change, not the exact implementation of every transition.</p>",
    "<h3>Table 2: isolate schooling and death-status changes</h3>", tab(table2),
    "<p>The two single-block rows hold the other tested education/status blocks at legacy values. Housing/wealth revisions do not affect these employment models. The last row is the actual updated-release-plus-R3-electricity comparison. Updating schooling and death status together exactly reproduces all four final employment estimates and sample sizes.</p>",
    "<h3>Where the extra complete cases come from</h3>", tab(read("table2_sample_transitions.csv")), tab(read("table2_gained_case_reasons.csv")),
    "<p>No legacy Table 2 complete case is lost. Schooling changes supply nonmissing IMTI for 4/3/2/1 additional cases across the four columns; the revised death flag admits one additional participant in every column.</p>",
    "<h3>Changes also occur inside the original samples</h3>", tab(read("table2_fixed_sample_diagnostics.csv")),
    "<p>On the shared complete-case records, employment outcomes and E_is are unchanged, while IMTI changes in 34/13/32/11 records. Consequently the differences are not explained solely by larger sample sizes.</p>",
    "<h2>2. Death/interview-status revisions change sample membership</h2>",
    "<p>Seven child-round death flags differ, affecting five participants. In R5 the updated file classifies three previously unflagged participants as deceased and two legacy-deceased participants as interviewed. Both of the latter have records in the original R5 child interview files. This corroborates interview availability; it does not establish death timing or explain how the producers resolved every discrepancy.</p>",
    tab(read("changed_death_flags_aggregate_rows.csv")), tab(status_checks),
    "<p>The older-cohort re-entry is in Oromia and has salary activity code 13 in the original R5 activity data. That participant accounts for the extra employment case in each Table 2 column and the changed Oromia employment proportions in Figure 1. Reverting only the death-status block restores the legacy Figure 1 in the tested scenarios. Revised membership also changes the mean/SD used to standardize cognitive scores, so standardized-score coefficients can change even when matched children's raw test scores do not.</p>",
    "<h2>3. Historical education controls are blanked in non-interview rounds</h2>",
    "<p>Every newly missing preprim, agegr1 and entype value occurs where inround = 0. At R5, preprim and agegr1 each lose 228 previously observed panel values. The analysis anchors these background variables to the R5 row, so a blank R5 cell can remove a person from reduced-form samples even when their history and earlier cognitive outcomes were observed.</p>",
    "<p>The legacy code first creates preprim and agegr1 as child-level education-history attributes and then merges them onto round-level records. TN63, p. 8 describes setting information missing for nonparticipating rounds and distinguishes household from participant interviews. The data pattern is consistent with that availability rule; without the updated code, its precise implementation remains uncertain.</p>",
    tab(read("education_missingness_by_interview.csv")),
    "<p>Restoring preprim/agegr1 changes reduced-form/first-stage fits in the controlled comparisons. School-type missingness is real, but changing entype alone has no independent effect on any of the 49 fitted models in any of the eight matched factorial pairs tested; those records are already excluded by other requirements.</p>",
    "<h2>4. One concrete-block household changes four younger-cohort fits</h2>", tab(wealth),
    "<p>The affected record is a younger-cohort Addis Ababa household in R3. Its original wall code 25 means concrete blocks. Counting that wall as good quality raises hq_new by 0.25 and wi_new by 1/12, exactly as the updated guide's Ethiopian wall-material revision predicts.</p>",
    "<p>After restoring the four education/status blocks, this one record is the remaining source of differences in iv5aa_yc, iv6aa_yc, iv5aa_yc_rf and iv6aa_yc_rf. Restoring its wealth/housing inputs reproduces all 49 models and all four analysis datasets. It does not affect the main employment models because those use the older cohort.</p>",
    "<h2>5. Controlled comparison results</h2>",
    "<p>The audit estimates 19 scenarios: a fresh legacy reference; all 16 combinations of the schooling, early-school-background, school-type and death-status blocks; and two additional wealth-restoration comparisons. In a factorial mask, TRUE means retain the updated block and FALSE means restore its legacy values. Other updated inputs remain in place. Model comparisons use tolerance 1e-10; both the fresh baseline and fully updated endpoints match the previously validated snapshots.</p>",
    tab(summary),
    "<h3>Restoring each block inside the otherwise updated analysis</h3>", tab(marginal),
    "<p>Block effects overlap and interact, so their model counts are not additive. The optional Shapley CSV averages each Table 2 coefficient/sample-size increment over every ordering of the four education/status changes; it is a mechanical data-change decomposition, not a new causal effect estimate.</p>",
    "<h3>Final closure check: restore all five blocks</h3>", tab(read("restored_analysis_data_matches.csv")),
    "<p>With all five blocks restored, all selected analysis fields and membership agree, as do all 49 coefficient/covariance/sample-size snapshots, both Table 3 fits and the descriptive results. This closes the attribution for the current pipeline; it does not imply that every field in the two raw constructed releases is identical.</p>",
    "<h3>Table 3 association-model changes</h3>", tab(channel_summary),
    "<h2>Implications for the migration</h2>",
    "<ol><li>Keep the verified R3 electricity reconstruction.</li><li>Choose a schooling measure deliberately. Rebuilding the broad legacy measure requires the relevant education-history sources; a simple hghschoolgrade rename does not reproduce its meaning.</li><li>Decide whether historical preprim/agegr1 should be defined as fixed background attributes using earlier observed information, with an explicit source and availability rule rather than automatically reading only the R5 cell.</li><li>Review the revised status classifications, and retain the documented concrete-block correction when adopting the updated wealth definition. Exact legacy replication should be kept as a reference, not used to select preferred results.</li></ol>",
    "<p>No production input, analysis CSV, manuscript table or Overleaf file was changed. The manuscript's existing specification/inference limitations and Tables 4-5 generator mismatch remain separate questions.</p>",
    "<h2>Sources</h2><ul>",
    "<li><a href='../../rawdata/legacy_constructed_r1_r5/code/stata/ethiopia_constructed.do'>Legacy construction do-file</a>: lines 1228-1255 (historical background), 1347-1370 (R5 schooling), 1455-1460 (round merge), and 3741-3754 (external death list).</li>",
    "<li><a href='../../rawdata/yl_const_r1_r7/UKDA-9543-stata/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf#page=8'>Updated guide, p. 8</a> (participation and missingness), <a href='../../rawdata/yl_const_r1_r7/UKDA-9543-stata/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf#page=21'>p. 21</a> (schooling definition), and <a href='../../rawdata/yl_const_r1_r7/UKDA-9543-stata/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf#page=39'>p. 39</a> (concrete blocks).</li>",
    "<li>Original R3 younger-cohort household data: wallr3; original R5 older/younger child interview records and older-cohort ACTR5 activity data. Paths are in the project raw-data catalog and the scripts.</li></ul>",
    "<h2>Reproduction and aggregate evidence</h2>",
    "<pre>Rscript code/auxiliary/attribute_constructed_differences.R\nRscript code/auxiliary/write_constructed_attribution_report.R</pre><ul>",
    sprintf("<li><a href='%s'>%s</a></li>", links, links), "</ul></body></html>"
  )
  writeLines(enc2utf8(report), file.path(out, "remaining_differences_report.html"), useBytes = TRUE)
  message("Created ", normalizePath(file.path(out, "remaining_differences_report.html")))
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  write_constructed_attribution_report(if (length(args)) args[1L] else "output/constructed_difference_attribution")
}
