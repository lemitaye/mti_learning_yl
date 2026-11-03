# Run from the project root:
# Rscript code/auxiliary/investigate_constructed_electricity.R
# Read-only investigation. Saves aggregate evidence, not participant records.
local({
  stopifnot(requireNamespace("haven", quietly = TRUE))
  source("code/helpers/raw_data_catalog.R", local = TRUE)
  out <- "temp/electricity_investigation"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  old_path <- raw_data_files[["constructed_legacy"]]
  new_path <- raw_data_files[["constructed_r1_r7"]]
  old <- haven::read_dta(old_path)
  latest <- haven::read_dta(new_path)
  original_path <- "rawdata/legacy_constructed_r1_r5/stata/stata11/ethiopia_constructed.dta"
  do_path <- "rawdata/legacy_constructed_r1_r5/code/stata/ethiopia_constructed.do"
  raw_do_path <- "rawdata/yl_r3/UKDA-6853-stata/code/stata/et_wealth_r3.do"
  input_paths <- c(old_path, new_path, original_path, do_path, raw_do_path)
  input_hash <- tools::md5sum(input_paths)
  stopifnot(input_hash[[old_path]] == input_hash[[original_path]])
  csv <- function(d, name) write.csv(d, file.path(out, name), row.names = FALSE)
  availability <- do.call(rbind, lapply(1:7, function(r) data.frame(
    round = r, legacy_records = sum(old$round == r),
    legacy_electricity_observed = sum(!is.na(old$elecq_new[old$round == r])),
    updated_records = sum(latest$round == r),
    updated_electricity_observed = sum(!is.na(latest$elecq[latest$round == r])))))
  csv(availability, "electricity_availability.csv")
  key <- function(d) paste(d$childid, d$round, sep = ":")
  new <- latest[match(key(old), key(latest)), ]
  stopifnot(identical(key(old), key(new)))
  # TN43, p. 10: services = mean of electricity, water, sanitation and cooking.
  implied <- 4 * new$sv - new$drwaterq - new$toiletq - new$cookingq
  residual_check <- do.call(rbind, lapply(1:5, function(r) {
    i <- old$round == r
    valid <- i & !is.na(implied) & !is.na(old$elecq_new)
    data.frame(round = r, legacy_observed = sum(i & !is.na(old$elecq_new)),
      electricity_implied_by_updated_services = sum(i & !is.na(implied)),
      jointly_observed = sum(valid), mismatches = sum(abs(implied[valid] - old$elecq_new[valid]) > 1e-6),
      nonbinary_residuals = sum(i & !is.na(implied) & pmin(abs(implied), abs(implied - 1)) > 1e-6))
  }))
  csv(residual_check, "services_index_residual_check.csv")
  stopifnot(all(residual_check$mismatches == 0L), all(residual_check$nonbinary_residuals == 0L))
  components <- c(sv_new = "sv", drwaterq_new = "drwaterq", toiletq_new = "toiletq", cookingq_new = "cookingq")
  component_check <- do.call(rbind, lapply(names(components), function(v) {
    x <- old[[v]]; y <- new[[components[[v]]]]
    valid <- !is.na(x) & !is.na(y)
    data.frame(legacy_variable = v, updated_variable = components[[v]],
      jointly_observed = sum(valid), value_mismatches = sum(abs(x[valid] - y[valid]) > 1e-6),
      observed_to_missing = sum(!is.na(x) & is.na(y)),
      missing_to_observed = sum(is.na(x) & !is.na(y)))
  }))
  csv(component_check, "services_components_comparison.csv")
  raw_checks <- list()
  raw_joined <- list()
  for (cohort in c("oc", "yc")) {
    path <- file.path("rawdata/yl_r3/UKDA-6853-stata/stata/stata13",
      paste0("r3_", cohort), "ethiopia", paste0("et_", cohort, "_householdlevel.dta"))
    input_hash <- c(input_hash, tools::md5sum(path))
    d <- haven::read_dta(path)
    stopifnot(!anyDuplicated(d$childid))
    old_r3 <- old[old$round == 3, ]
    x <- old_r3$elecq_new[match(d$childid, old_r3$childid)]
    valid <- !is.na(x) & !is.na(d$elecq)
    raw_checks[[cohort]] <- data.frame(cohort = cohort, raw_household_records = nrow(d),
      raw_electricity_observed = sum(!is.na(d$elecq)),
      jointly_observed_with_legacy = sum(valid),
      legacy_value_mismatches = sum(x[valid] != d$elecq[valid]),
      legacy_missingness_mismatches = sum(is.na(x) != is.na(d$elecq)),
      raw_question_vs_indicator_mismatches = sum(d$elecr3 != d$elecq, na.rm = TRUE))
    raw_joined[[cohort]] <- data.frame(childid = d$childid, electricity = d$elecq)
  }
  raw_checks <- do.call(rbind, raw_checks)
  csv(raw_checks, "round3_raw_validation.csv")
  raw <- do.call(rbind, raw_joined)
  stopifnot(!anyDuplicated(raw$childid))
  old_r3 <- old[old$round == 3, ]
  raw_r3 <- raw$electricity[match(old_r3$childid, raw$childid)]
  equal <- (is.na(raw_r3) & is.na(old_r3$elecq_new)) |
    (!is.na(raw_r3) & !is.na(old_r3$elecq_new) & raw_r3 == old_r3$elecq_new)
  stopifnot(all(equal))
  # Country comparisons identify whether the issue is a universal rename.
  countries <- do.call(rbind, lapply(c("ethiopia", "india", "peru", "vietnam"), function(country) {
    suffix <- if (country == "vietnam") "r1r6" else "r1r7"
    path <- file.path("rawdata/yl_const_r1_r7/UKDA-9543-stata/stata/stata13",
      paste0(country, "_constructed_", suffix, ".dta"))
    d <- haven::read_dta(path)
    v <- grep("elecq", names(d), value = TRUE)
    stopifnot(length(v) == 1L)
    data.frame(country = country, variable = v, R1_R5_observed = sum(!is.na(d[[v]][d$round <= 5])),
      R6_observed = sum(!is.na(d[[v]][d$round == 6])),
      R7_observed = sum(!is.na(d[[v]][d$round == 7])))
  }))
  csv(countries, "country_release_check.csv")
  stopifnot(identical(unname(input_hash), unname(tools::md5sum(names(input_hash)))))
  csv(data.frame(path = names(input_hash), md5 = unname(input_hash)), "input_checksums.csv")
  writeLines(capture.output(sessionInfo()), file.path(out, "session.txt"))
  esc <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  tab <- function(d) paste0("<div class='scroll'><table><thead><tr><th>",
    paste(esc(names(d)), collapse = "</th><th>"), "</th></tr></thead><tbody>",
    paste(apply(d, 1L, function(r) paste0("<tr><td>", paste(esc(r), collapse = "</td><td>"), "</td></tr>")), collapse = "\n"),
    "</tbody></table></div>")
  sources <- c(
    "<li>TN48, pp. 21-22 and Appendix: <a href='https://www.younglives.org.uk/sites/www.younglives.org.uk/files/YL%20Technical%20Note%2048%20A%20Guide%20to%20R1%20to%205%20Constructed%20Files.pdf'>legacy constructed guide</a>.</li>",
    "<li>TN63, pp. 39-40, Table 24: <a href='https://doc.ukdataservice.ac.uk/doc/9543/mrdoc/pdf/9543_tn63_constructed_dataset_final.pdf#page=39'>updated constructed guide</a>.</li>",
    "<li>TN43, p. 10, section 3.2: <a href='https://www.younglives.org.uk/sites/default/files/migrated/YL-TN43_0.pdf#page=10'>wealth index methodology</a>.</li>",
    "<li>Legacy construction code: <code>rawdata/legacy_constructed_r1_r5/code/stata/ethiopia_constructed.do</code>, lines 2301-2323.</li>",
    "<li>Round 3 source: <code>rawdata/yl_r3/UKDA-6853-stata/stata/stata13/r3_oc/ethiopia/et_oc_householdlevel.dta</code> and corresponding <code>r3_yc/ethiopia/et_yc_householdlevel.dta</code>; fields childid, elecr3 and elecq. Archived <code>code/stata/et_wealth_r3.do</code>, lines 45-49 and 118-122, includes electricity in the services index.</li>",
    "<li>UKDS SN9543 archive dictionary: <code>mrdoc/ukda_data_dictionaries.zip</code>; electricity is variable 247 in the Ethiopian dictionary. This documents the variable name but does not diagnose historical missingness.</li>"
  )
  report <- c(
    "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1'><title>Electricity variable investigation</title>",
    "<style>body{font:16px/1.55 system-ui,sans-serif;max-width:1200px;margin:36px auto;padding:0 24px;color:#223044}h1{line-height:1.2}h2{margin-top:32px}a{color:#175d8b}.finding{background:#edf4f8;padding:16px 22px;border-left:4px solid #507b98}.scroll{overflow:auto}table{border-collapse:collapse;font-size:14px;width:100%}td,th{border:1px solid #dbe3eb;padding:8px;text-align:left}th{background:#e7eff6}code{overflow-wrap:anywhere}pre{overflow:auto;background:#f3f6f9;padding:16px}</style></head><body>",
    "<h1>Why is historical electricity missing in the updated constructed file?</h1>",
    "<div class='finding'><p><strong>The evidence points to an omission in assembling or retaining the historical standalone electricity field, rather than a documented change in the electricity definition.</strong> This is a diagnosis from the supplied data and documentation; the exact programming cause has not been confirmed by the depositors.</p></div>",
    "<h2>1. The name changes, but the intended concept remains</h2>",
    "<p>The legacy file uses <code>elecq_new</code>; the updated Ethiopian file uses <code>elecq</code>. TN48 Table 21 and TN63 Table 24 both describe household access to electricity. The legacy Stata construction script imports the recalculated wealth components from <code>et_wi_r1.dta</code> through <code>et_wi_r5.dta</code> and adds <code>_new</code> to every variable from wi through cookingq. Therefore the suffix is a naming convention for that imported component set, not evidence of a different electricity question.</p>",
    "<p>TN43, p. 10 defines a binary household electricity-access indicator and describes electricity connections and generation sources. TN63 documents revisions to Ethiopian wall-material treatment for housing quality/wealth and changes in water/sanitation treatment during phone calls. It does not document deleting electricity from earlier rounds. In fact Table 24 lists elecq for Rounds 1-7 and Calls 2 and 5.</p>",
    "<h2>2. Observed availability contradicts the expected historical coverage</h2>", tab(availability),
    "<p>There is no second electricity-named field in the updated Ethiopian file containing the missing history. The selected legacy file is byte-identical to its original archived Stata copy. The electricity problem is therefore not caused by selecting a locally altered legacy file or by comparing unmatched panel rows.</p>",
    "<h2>3. Electricity information is still embedded in the updated services index</h2>",
    "<p>TN43 defines the services index as the equal-weighted mean of electricity, drinking water, sanitation and cooking-fuel access. Rearranging it gives:</p>",
    "<pre>electricity_implied = 4 * sv - drwaterq - toiletq - cookingq</pre>",
    tab(residual_check),
    sprintf("<p>This yields %s binary historical values, all matching the legacy electricity indicator within tolerance 1e-6. The remaining %d legacy-observed values cannot be recovered by this identity because at least one required updated component is missing. No component missingness was replaced with zero.</p>",
      format(sum(residual_check$jointly_observed), big.mark = ","),
      sum(residual_check$legacy_observed - residual_check$electricity_implied_by_updated_services)),
    tab(component_check),
    "<p>The components and services index match numerically wherever both releases have observations; their remaining differences are losses of observations. This is strong evidence that historical electricity was used to form the service index but is not populated as a separate column in this release.</p>",
    "<h2>4. The original Round 3 electricity values remain available</h2>", tab(raw_checks),
    "<p>The two original Round 3 household files contain 2,859 household records, with 2,858 observed electricity values and one missing value. The archived raw electricity indicator and yes/no electricity question agree on every observed response. A left join onto all 2,999 historical Round 3 panel keys reproduces legacy electricity values and missingness exactly, including the 140 absent household rows. Thus the manuscript's R3 electricity control can be reconstructed from the original source data without importing values from the legacy constructed file.</p>",
    "<p>This investigation validates the reconstruction but does not write reconstructed participant data or modify the extraction pipeline. For implementation, preserve source missingness and join explicitly on childid; retain round and cohort checks. A missing response is not a no-electricity response.</p>",
    "<h2>5. This is not a uniform change across all countries</h2>", tab(countries),
    "<p>India and Peru retain historical electricity under elecq; Vietnam retains it under elecq_new. No country file in the supplied release has observed R6 electricity under its electricity field, although the new guide lists some phone-call coverage. This wider documentation/data discrepancy warrants clarification, but it does not establish its cause.</p>",
    "<h2>What is established and what remains uncertain</h2>",
    "<ul><li><strong>Established:</strong> the intended historical electricity concept remains in the guides; the supplied Ethiopian standalone field has no R1-R6 values; historical electricity can be recovered from updated services components for 14,117 rows; original R3 data reproduce the entire legacy R3 indicator.</li><li><strong>Likely:</strong> a failure to retain, rename, append or merge the historical standalone variable in the updated assembly. These are possible mechanisms, not identified lines of code.</li><li><strong>Not established:</strong> the exact programming step or an official correction. No updated construction do-file was present in the supplied SN9543 directory. Searches of official Young Lives and UKDS material found no explanation for this specific historical omission during this investigation; this is not proof that no correction exists.</li></ul>",
    "<p><strong>Recommended next step:</strong> use the original R3 household electricity indicator when implementing the new-release pipeline, retaining its missingness. Ask the data producers to confirm the SN9543 historical electricity omission and provide the relevant construction script or corrected file. Resolving electricity does not resolve the separate schooling-definition and sample changes identified in the replication audit.</p>",
    "<h2>Sources and reproducibility</h2><ul>", sources, "</ul>",
    "<p>Run <code>Rscript code/auxiliary/investigate_constructed_electricity.R</code> from the project root. The script generates this report and the aggregate CSV evidence. No raw datasets or current analysis outputs are changed.</p><ul>",
    sprintf("<li><a href='%s'>%s</a></li>",
      c("electricity_availability.csv", "services_index_residual_check.csv", "services_components_comparison.csv", "round3_raw_validation.csv", "country_release_check.csv", "input_checksums.csv", "session.txt"),
      c("Availability by round", "Services-index identity check", "Other service components", "Round 3 source validation", "Country comparison", "Input checksums", "R session")),
    "</ul></body></html>"
  )
  writeLines(enc2utf8(report), file.path(out, "electricity_investigation.html"), useBytes = TRUE)
  message("Created: ", normalizePath(file.path(out, "electricity_investigation.html")))
})
