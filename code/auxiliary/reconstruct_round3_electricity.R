# Apply to an Ethiopian constructed panel without modifying any source files.
# Uses the original R3 household indicator, never the legacy constructed file.
# The compatibility column elecq_new is updated for R3 only; elecq is untouched.
reconstruct_round3_electricity <- function(panel, project_root = ".") {
  stopifnot(requireNamespace("haven", quietly = TRUE),
    all(c("childid", "round", "yc") %in% names(panel)))
  ids <- as.character(panel$childid)
  r3 <- which(panel$round == 3 & !is.na(panel$round))
  if (!length(r3) || anyNA(ids[r3]) || anyDuplicated(ids[r3])) {
    stop("Round 3 must contain nonmissing, unique child identifiers.")
  }
  paths <- setNames(file.path(project_root,
    "rawdata/yl_r3/UKDA-6853-stata/stata/stata13",
    c("r3_oc/ethiopia/et_oc_householdlevel.dta", "r3_yc/ethiopia/et_yc_householdlevel.dta")),
    c("oc", "yc"))
  lookup <- do.call(rbind, lapply(names(paths), function(cohort) {
    d <- haven::read_dta(paths[[cohort]])
    stopifnot(all(c("childid", "elecq") %in% names(d)),
      !anyNA(d$childid), !anyDuplicated(d$childid))
    # as.double removes labels, preserving NA; no equality-to-one recoding.
    electricity <- as.double(d$elecq)
    if (any(!is.na(electricity) & !electricity %in% c(0, 1))) {
      stop("Unexpected electricity codes in ", paths[[cohort]], "; review before recoding.")
    }
    data.frame(childid = as.character(d$childid), yc = as.integer(cohort == "yc"),
      electricity = electricity, stringsAsFactors = FALSE)
  }))
  stopifnot(!anyDuplicated(lookup$childid))
  index <- match(ids[r3], lookup$childid)
  matched <- !is.na(index)
  if (anyNA(panel$yc[r3][matched]) ||
      any(panel$yc[r3][matched] != lookup$yc[index[matched]])) {
    stop("Cohort mismatch in the Round 3 electricity join.")
  }
  original <- panel
  if (!"elecq_new" %in% names(panel)) panel$elecq_new <- NA_real_
  panel$elecq_new[r3] <- lookup$electricity[index]
  # Unmatched households remain NA; source NA also remains NA.
  stopifnot(nrow(panel) == nrow(original), identical(panel$childid, original$childid),
    all(is.na(panel$elecq_new[r3][!matched])),
    identical(is.na(panel$elecq_new[r3][matched]), is.na(lookup$electricity[index[matched]])))
  for (v in setdiff(names(original), "elecq_new")) stopifnot(identical(panel[[v]], original[[v]]))
  if ("elecq_new" %in% names(original)) {
    stopifnot(identical(panel$elecq_new[-r3], original$elecq_new[-r3]))
  }
  audit <- do.call(rbind, lapply(c(0L, 1L), function(yc) {
    j <- which(panel$yc[r3] == yc)
    source_missing <- matched[j] & is.na(lookup$electricity[index[j]])
    data.frame(cohort = if (yc == 1L) "younger" else "older", panel_records = length(j),
      matched_household_records = sum(matched[j]),
      unmatched_household_records = sum(!matched[j]),
      observed_electricity = sum(!is.na(panel$elecq_new[r3][j])),
      source_response_missing = sum(source_missing),
      total_missing = sum(is.na(panel$elecq_new[r3][j])))
  }))
  list(data = panel, audit = audit,
    sources = data.frame(path = normalizePath(unname(paths)), md5 = unname(tools::md5sum(paths))))
}
