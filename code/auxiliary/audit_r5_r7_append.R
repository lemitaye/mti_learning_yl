# Called by 11_prepare_r5_r7_analysis.R with prepared data in memory.
stopifnot(
  identical(names(r5_analysis), names(r7_analysis)),
  !anyDuplicated(paste(combined$child_id, combined$round)),
  nrow(r7_analysis) == 2038L,
  sum(r7_analysis$yc == 1L) == 1406L,
  identical(legacy_hashes, tools::md5sum(legacy_files))
)
for (variable in shared_variables) {
  stopifnot(isTRUE(all.equal(
    as.vector(r5_analysis[[variable]]), as.vector(r5[[variable]]),
    check.attributes = FALSE
  )))
}
counts <- combined |>
  group_by(round, cohort) |>
  summarise(
    rows = n(),
    salary_observed = sum(!is.na(wage_employII)),
    wage_observed = sum(!is.na(wage_employ)),
    .groups = "drop"
  )
write_csv(counts, file.path(audit_dir, "sample_counts.csv"))
writeLines(c(
  "Employment extension: shared names and original activity exclusions.",
  "R7 includes every in-person respondent; no phone respondents are added.",
  "R5 inputs are preserved verbatim; R7 current resources use old definitions.",
  "R5 household controls were measured in R3, R7 controls in R7.",
  "foodsec_r3, policy region and childhood controls remain historical.",
  "IMTI remains frozen at R5; YC exposure requires review before estimation.",
  "No R7 grade/qualification measure is equated with legacy schooling years.",
  "R4 scores have explicit _r4 names; no fresh cognitive outcomes are implied.",
  "YC R5 employment is structurally missing in the existing lookup.",
  "Key: child_id plus round; repeated participants are retained across rounds.",
  "No language-score or complete-case gate is applied to these master files.",
  "For legacy OC comparisons apply the original R4 language-score gate.",
  "A pooled analysis must account for repeated IDs and policy-region inference."
), file.path(audit_dir, "README.txt"))
print(counts)
