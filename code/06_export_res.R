# =============================================================================
# Young Lives Ethiopia: regression-table exports
# Project: Mother Tongue Instruction, Learning and Employment
# Authors: Teferi Mergo and Lemi Daba
#
# Dependency: run 05_estimation.R immediately beforehand in the SAME R session.
# Outputs: legacy tableII--VI.tex plus the revised Table 2 and diagnostics from
# 07_table_main.R. Tables remain in output/tables/ and mirror to the revision
# project unless MTI_OVERLEAF_DIR is empty; CSV diagnostics stay local.
#
# Model order, printed labels and positional LaTeX edits are retained for
# replication. Historical heading/specification issues are noted below and in
# README.md; this presentation cleanup does not revise their interpretation.
# =============================================================================

# ---- Shared formatting ------------------------------------------------------
regression_note <- paste0(
  "Standard errors are in parentheses. *** Significant at 1\\%, ",
  "** Significant at 5\\%, * Significant at 10\\%."
)
combine_panels <- function(first, second, panel_names) {
  star_panel(first, second, same.summary.stats = TRUE,
             panel.label.fontface = "bold", panel.names = panel_names)
}
format_regression_table <- function(table, rows, insert_after = 10) {
  star_insert_row(table, rows, insert.after = insert_after) %>%
    star_notes_tex(note.type = "threeparttable", note = regression_note)
}
employment_header <- " & Z-score & Z-score & Employed & Employed \\\\"

# ---- Legacy non-Addis table: first stages and 2SLS ---------------------------
non_aa_first_stage_panel <- stargazer(
  iv8$stage1, iv9$stage1, iv6$stage1, iv1$stage1,
  keep = "E_is", keep.stat = "n",
  column.labels = c("Maths", "Language", "Salary", "Wage"),
  dep.var.labels.include = FALSE, dep.var.caption = "", model.names = FALSE,
  covariate.labels = "$E_{is}$", title = "Main Results", label = "tab:mainreg"
)
non_aa_iv_panel <- stargazer(
  iv8, iv9, iv6, iv1, keep = "IMTI", keep.stat = "n", covariate.labels = "IMTI"
)
non_aa_table <- combine_panels(non_aa_first_stage_panel, non_aa_iv_panel,
                               c("First Stage", "2SLS")) %>%
  format_regression_table(employment_header)
star_tex_write(non_aa_table, file = "output/tables/tableII.tex")

# ---- Addis Ababa: reduced forms and 2SLS ------------------------------------
aa_reduced_form_panel <- stargazer(
  rf6aa, rf7aa, rf5aa, rf1aa,
  keep = "E_is", keep.stat = "n",
  column.labels = c("Maths", "Language", "Salary", "Wage"),
  dep.var.labels.include = FALSE, dep.var.caption = "", model.names = FALSE,
  covariate.labels = "$E_{is}$", title = "Estimates for Addis Ababa", label = "tab:aareg"
)
aa_iv_panel <- stargazer(
  iv8aa, iv9aa, iv6aa, iv1aa, keep = "IMTI", keep.stat = "n", covariate.labels = "IMTI"
)
aa_table <- combine_panels(aa_reduced_form_panel, aa_iv_panel, c("Reduced Form", "2SLS")) %>%
  format_regression_table(paste0(employment_header, " "))
star_tex_write(aa_table, file = "output/tables/tableIII.tex")

# ---- Younger-cohort Addis: Young Lives and school-survey outcomes ------------
# Reduced forms use standardized outcomes, while 2SLS uses raw scores. The
# inherited repeated Z-score heading is retained pending a substantive revision.
younger_reduced_form_panel <- stargazer(
  iv5aa_yc_rf, iv6aa_yc_rf, iv3aa_yc_rf, iv4aa_yc_rf,
  keep = "E_is", keep.stat = "n",
  dep.var.labels = c("Maths", "Language", "Maths", "Literacy"), dep.var.caption = "",
  covariate.labels = "$E_{is}$", title = "Estimates for Addis Ababa: Younger Cohort",
  label = "tab:ycaa"
)
younger_iv_panel <- stargazer(
  iv5aa_yc, iv6aa_yc, iv3aa_yc, iv4aa_yc,
  keep = "IMTI", keep.stat = "n", covariate.labels = "IMTI"
)
younger_table <- combine_panels(younger_reduced_form_panel, younger_iv_panel,
                                c("Reduced Form", "2SLS")) %>%
  format_regression_table(
    c(" & \\multicolumn{2}{c}{\\textbf{YL Cognitive}} & \\multicolumn{2}{c}{\\textbf{School Survey}} \\\\ ",
      "\\cline{2-3} \\cline{4-5} ", "\\\\[-1.8ex]",
      " & Z-score & Z-score & Z-score & Z-score \\\\"),
    insert_after = c(9, 9, 9, 10)
  )
star_tex_write(younger_table, file = "output/tables/tableIV.tex")

# ---- Tigray/Oromia comparisons ----------------------------------------------
# Legacy columns 3/4 are salary/wage models although the headings say Wage/Salary.
tigray_oromia_panel <- stargazer(
  rf6ot, rf7ot, rf5ot, rf1ot, keep = "Tigray", keep.stat = c("n", "rsq"),
  dep.var.labels = c("Maths", "Language", "Wage", "Salary"), dep.var.caption = "",
  model.names = FALSE, title = "Comparing Estimate for Tigray and Oromia", label = "tab:oromtig"
)
tigray_oromia_table <- format_regression_table(tigray_oromia_panel, employment_header)
star_tex_write(tigray_oromia_table, file = "output/tables/tableV.tex")

# ---- Non-Addis excluding Amhara: first stages and 2SLS -----------------------
non_amhara_first_stage_panel <- stargazer(
  iv8_nonAm$stage1, iv9_nonAm$stage1, iv6_nonAm$stage1, iv1_nonAm$stage1,
  keep = "E_is", keep.stat = "n",
  column.labels = c("Maths", "Language", "Salary", "Wage"),
  dep.var.labels.include = FALSE, dep.var.caption = "", model.names = FALSE,
  covariate.labels = "$E_{is}$",
  title = "Estimates Excluding Observations in the Amhara Region", label = "tab:nonAm"
)
non_amhara_iv_panel <- stargazer(
  iv8_nonAm, iv9_nonAm, iv6_nonAm, iv1_nonAm,
  keep = "IMTI", keep.stat = "n", covariate.labels = "IMTI"
)
non_amhara_table <- combine_panels(non_amhara_first_stage_panel, non_amhara_iv_panel,
                                   c("First Stage", "2SLS")) %>%
  format_regression_table(employment_header)
star_tex_write(non_amhara_table, file = "output/tables/tableVI.tex")

# ---- Revised manuscript Table 2: dedicated inference and diagnostics ---------
source("code/07_table_main.R")

# Local tables are complete before the optional external export. The Table 2
# generator handles its own mirror; diagnostic CSVs are never sent to Dropbox.
local({
  source("code/helpers/export_paths.R", local = TRUE)
  mirror_output(file.path("output/tables",
                         paste0("table", c("II", "III", "IV", "V", "VI"), ".tex")), "tables")
})
