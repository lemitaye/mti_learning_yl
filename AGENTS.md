# Repository guidance

## Purpose and current task

This is an empirical economics research project by Teferi Mergo and Lemi Daba,
currently being revised following Applied Economics referee comments. The active
manuscript title is **The effects of mother tongue instruction on employment:
Evidence from Ethiopia**. README.md and older material use **Mother Tongue
Instruction, Learning and Employment: Evidence from Ethiopia**.

The study uses Ethiopian Young Lives rounds 1–5 (2002–2016), with additional
younger-cohort analysis using the 2012–13 School Survey. The principal design
instruments mother-tongue instruction (MTI) exposure using regional policy
intensity and ethnicity. Treat identification and mechanism claims as research
claims to evaluate, especially in light of the referees' concerns.

Start with this file, README.md, the relevant manuscript sections and scripts,
and both reports in `other/referee_comment_Applied_Econ/`. Historical assistant
guidance in the archive is background only; check it against the active code.

## Repository map

| Path | Role |
| --- | --- |
| Dropbox/Overleaf | Manuscript revision and protected original; destinations described below. |
| `code/` | Active R extraction, cleaning, estimation, and export code. |
| `rawdata/` | Original Stata inputs, questionnaires, dictionaries, and survey documentation. Read-only; new files are git-ignored, but some release documentation is already tracked. Do not redistribute survey inputs. |
| `data/` | Four tracked analysis CSVs generated from raw inputs. Changes are substantive research artifacts. |
| `output/tables/`, `output/figures/` | Staging areas for generated results, mirrored to the revision project as described below. |
| `other/referee_comment_Applied_Econ/` | Two referee reports; preserve the originals. |
| `_archive/` | Historical scripts, drafts, reports, notes, and a nested collaboration repository. Consult for provenance when useful; keep inactive and preserve history. |
| `mti_learning_yl.Rproj` | RStudio project; use the repository root as the working directory. |

`main.tex` includes `abstract`, `intro`, `literature`, `data`, `strategy`,
`results`, `robustness`, and `conclusion`. At the initial audit, `literature.tex`
was empty; much of the literature and theory was embedded in `intro.tex`.
Bibliography entries live in `refs.bib`; reuse verified citation keys and retain
LaTeX labels when reorganizing text.

## Referee priorities

Use filenames to identify reports, rather than inventing referee numbers.
These are requests to address, not analyses already completed or methods already
chosen. For revision work, connect each comment to an edit, analysis, or explicit
limitation; a response should identify the evidence and manuscript location.

### `Review_ Article Applied Economics.pdf`

- Shorten the introduction and separate it from background; state the question,
  contribution, approach, principal findings, and a matching roadmap.
- Develop the literature review and define MTI, cognitive ability, wage
  employment, and salary employment clearly.
- Make the theoretical framework explicit in a separate section and connect
  it to the evidence and interpretation.
- Explain the quantitative approach, survey/data collection, and study population.
- Improve findings headings and roadmap sentences; distinguish presentation of
  results from discussion and interpretation, and engage with theory/literature.
- Include recommendations that follow from the findings and their limitations.

### `Review Report.pdf`

- Explain sample selection and statistical power, especially the Addis Ababa
  employment regressions with approximately 40 observations. Consider power
  calculations; an insignificant falsification result does not validate exclusion.
- Report first-stage strength diagnostics for the relevant estimation samples.
- Explain actual standard errors and address inference with only four regional
  policy clusters; investigate suitable few-cluster methods, including the
  suggested wild bootstrap or randomization inference, with their assumptions.
- Clarify outcome definitions, overlap, exclusions, and denominators; provide a
  cross-tabulation where useful.
- Investigate whether survey variables can directly test public/private-sector
  mechanisms. Salary/wage labels alone do not establish sector or language use.
- State the exact Table 3 channel specification, explain its relationship to the
  manuscript equations, and address the requested corresponding first stage.
- Address multiple comparisons across outcomes and subsamples.
- Sharpen the substantive contribution relative to Seid (2022) and Chicoine (2019).
- Address wording and presentation: distinguish an undetected effect from zero;
  clarify the Table 5 estimator and R-squared; add Figure 1 uncertainty or sample
  sizes; check spelling, state/ethnicity terminology, and citation consistency.
  Do not automatically replace ethnic-group names with region names.

## Analysis workflow

Run commands from the repository root. The intended full command is:

```sh
Rscript code/_master.R
```

This **writes data and results** and is not a harmless smoke test. Inspect the
known blockers below and preserve current manuscript tables before running it.

| Order | Script | Output / dependency |
| --- | --- | --- |
| 1 | `code/01_extract_clean.R` | Older cohort: `data/aa_samp.csv`, `data/non_aa_samp.csv`. |
| 2 | `code/02_cleaning_yc.R` | `data/ylsample_yc.csv`, `data/schsur_yl_joined.csv`. |
| 3 | `code/03_descriptives.R` | `output/tables/tableI.tex`, `output/figures/employ_gr.pdf`. |
| 4 | `code/05_estimation.R` | Regression objects in memory. |
| 5 | `code/06_export_res.R` | `output/tables/tableII.tex` through `tableVI.tex`, using step 4 objects; also sources `07_table_main.R` for staged `tableII_mod.tex` and diagnostics CSV. |
| 6 | `code/09_table_rev.R` | Writes `output/tables/table_rev.tex` and mirrors to the revision project. |

`_master.R`, `03_descriptives.R`, and `05_estimation.R` clear workspace objects.
`06_export_res.R` relies on models from `05_estimation.R` in the **same R session** and
on already loaded packages. Standalone script execution generally requires
checking its package and workspace dependencies first.

The master script loads `tidyverse`, `haven`, `scales`, `stargazer`, `AER`, `lfe`,
`starpolishr`, `fastDummies`, and `xtable`. Existing setup instructions obtain
`starpolishr` from `ChandlerLutz/starpolishr` on GitHub. `_master.R` now installs
missing CRAN packages and bootstraps `remotes` for GitHub installation when
needed, then verifies availability before attaching packages. It also checks
project paths and creates generated-output directories. There is no dependency lockfile. Targeted checks live in `temp/tests/`.

### Known reproducibility issues (initial audit: 2026-10-31)

- Resolved during debugging: the unused `rf1oa`–`rf5oa` exploratory block is now
  commented out alongside its inactive `orom.amh` setup. Its formulas also
  targeted Tigray; do not reactivate it without specifying that comparison.
- Resolved during debugging: exploratory plots in `03_descriptives.R` now use
  `non_aa_samp` and the correct ordering column, and within-region score
  standardization uses `mutate()` rather than multirow `summarise()`.
- The repaired master pipeline completed in isolated copies on 2026-10-31.
  Against original result-producing code with broken exploratory blocks skipped,
  all 49 regression coefficient/covariance/sample-size snapshots, Table 1 data,
  Figure 1 layers, and six staged table bodies matched exactly. Rebuilt CSVs
  matched repository copies; Table 3 matched the manuscript body. This does not
  resolve the historical table-generator differences described below.
- Table 2 is now reconstructed by `code/07_table_main.R` (standalone or via
  `06_export_res.R`), preserving original employment estimates/SEs/N and adding
  exact-sample F, partial R-squared, HC1 F, and Anderson–Rubin diagnostics.
  Table notes now reflect the actual controls. Diagnostics are staged in
  `output/tables/tableII_mod_diagnostics.csv`; review before copying the table.
  HC1 diagnostics allow individual heteroskedasticity, not regional dependence.
- Manuscript Tables 4 and 5 retain a December-2023/November-2022 generator
  mismatch. Validate their specifications before replacing them.
- Existing README.md/CLAUDE.md describe Table 3 as previously byte-verified.
  This audit did not rerun it. Its writer emits a timestamp and output path, so
  compare estimates, standard errors, sample sizes, and table body rather than
  expecting identical metadata on a new run.
- Both cleaning scripts contain `chlang == "Amaharic"` in an Oromia treatment
  branch, unlike the usual `"Amharic"` spelling. Check source labels and affected
  records before correcting it; a spelling fix here can change treatment coding.
- At this audit, all seven active R scripts parsed. `AER`, `starpolishr`, and
  `fastDummies` were absent from the local R library. Recheck dependencies when
  executing analysis; do not treat this as a permanent environment description.

## Variables and econometric conventions

- `child_id` is the cross-round child join key, normalized from survey IDs
  (often by removing `ET` and converting to numeric). Preserve the per-round
  selection, `<variable>_r<round>` naming, and join conventions. Check uniqueness
  and row counts at each join, particularly when handling activity records.
- `IMTI` is the endogenous treatment, labelled MTI intake/MTIuse in the paper.
  It uses completed schooling capped at region/language-specific ceilings:
  generally 8, 6, or 4 years, with exceptions and zero outside eligible cells.
- `E_is` is Predicted MTI, taking values 0, 0.5, 0.75, and 1 from region,
  ethnicity, and, in some cells, instruction language. Read the exact
  `case_when()` rules; the simplified policy description is not the full code.
- `lang_primary` uses reported round-4 instruction language when the child is
  still in primary school and instruction is non-English; otherwise it proxies
  with the round-3 math-test language. It affects both treatment and instrument.
- Main analyses use the non-Addis sample; some exclude Amhara. Addis Ababa
  models add `entype_r4` and use `type_activ != 19`. Current scripts also filter
  `raw_lang != 0` before employment estimation. Track all filters and missingness.
- `wage_employII` is the **salary** outcome: activity code 13 is positive.
  `wage_employ` is the broader **wage** outcome: codes 5, 12, and 13 are positive.
  Salary positives therefore also satisfy the broader wage definition; the two
  variables have different exclusions/missingness and analysis denominators.
  Consult the actual recodes and survey documentation before changing taxonomy.
- Main IV models use `lfe::felm`, with `(IMTI ~ E_is)` in its IV formula slot.
  Do not infer clustering from the package or from a separate first-stage model:
  inspect each formula and exported uncertainty calculation.
- `09_table_rev.R` fits `std_lang ~ inst + controls`, with `inst = E_is * IMTI`,
  using `felm` without an IV slot. This is an OLS association, despite its
  comments calling it a channel first stage. It is not a completed 2SLS channel
  analysis. Standardization occurs after the language-score filter and before
  excluding Amhara. Describe the implemented estimator accurately.
- `06_export_res.R` builds tables with `stargazer` and `starpolishr`; positional
  header/row insertions are sensitive to layout changes. `09_table_rev.R` writes
  LaTeX directly.

## Manuscript result mapping and build

Table numbers follow active `main.tex` input order, not filename numbering:

| Paper | Manuscript file | Generator / status |
| --- | --- | --- |
| Table 1 | `tables/tableI.tex` | `03_descriptives.R`; output staged first. |
| Table 2 | `tables/tableII_mod.tex` | `07_table_main.R`, also called by `06_export_res.R`; original estimates reproduced, diagnostics added. |
| Table 3 | `tables/table_rev.tex` | `09_table_rev.R`; local staging and revision export. |
| Table 4 | `tables/tableIII.tex` | December 2023 version; generator mismatch. |
| Table 5 | `tables/tableV.tex` | December 2023 version; generator mismatch. |
| Figure 1 | `figures/employ_gr.pdf` | `03_descriptives.R`; output staged first. |
| Appendix map | `figures/map.png` | Static source image; attribution in `main.tex`. |

`tableII.tex`, `tableIV.tex`, and `tableVI.tex` are commented out in `main.tex`.
Review staged results before copying them into the manuscript.

Build the manuscript from its Dropbox/Overleaf project directory, for example:

```sh
latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex
```

`main.tex` uses `fontspec` and Times New Roman, so it needs XeLaTeX or LuaLaTeX
and the font. The RStudio project's pdfLaTeX setting is not suitable for this
manuscript. The bibliography uses natbib/BibTeX (`apalike`). Check compilation
logs, references, citations, table numbering, and the rendered pages after edits.

## Working and validation practices

- Keep edits scoped to the requested revision. Preserve coauthor changes and
  original referee reports; avoid incidental archive cleanup or result rewrites.
- Change data definitions and specifications transparently. Record the reason,
  affected samples, and impact on estimates; never tune a model to recover stars.
- Check claims against actual tables/code. Do not equate salary with public
  sector, infer causality from Table 3, interpret insignificance as zero, or
  assert that a small falsification sample proves instrument validity.
- Verify new literature and factual claims using original sources; never invent
  citations, diagnostics, results, or completed responses to referees.
- For code changes, first parse affected scripts; then run the relevant analysis
  in a clean session and compare sample sizes, coefficients, standard errors,
  diagnostics, and generated tables. Separate existing failures from new ones.
- For prose-only changes, check citations, cross-references, terminology, and
  rendering as appropriate. A documentation-only task does not require rerunning
  the estimation pipeline.
- Keep raw inputs and participant-level data out of new public artifacts.
  Prefer aggregate diagnostics in reports and logs.
- Report what changed, what was verified, and what remains unresolved. Update
  this guide and README.md if paths, workflow, or reproduction status change.

## Personal results viewer

`temp/results_viewer/` is a separate, nonpublication LaTeX notebook. Its `build.sh`
refreshes a viewer-only employment PDF from the original descriptive plot
expression and compiles `main.pdf`; it does not rerun regressions. It reads staged
tables directly from `output/tables/`, including the channel table. Keep source/model
notes and export-issue annotations current when changing the analysis. The
viewer uses pdfLaTeX, independently of the manuscript's XeLaTeX requirement.

## Table 2 regional inference

`07_table_main.R` sources `cluster_inference.R` locally. Required packages now
include `clubSandwich` (master setup installs missing packages). Panel C provides
CR2/Satterthwaite inference using equivalent AER IV fits and CR2 AR checks. A
separate within-region-orthogonalized AR score sign-flip sensitivity calculation
uses all sign patterns; it changes test moments, not point estimates. Tigray has
zero residual instrument variation in this calculation, so only three/two
regions are informative. Do not claim it is exact finite-sample randomization
inference, or that more draws can fix its resolution. Its conditional validity,
symmetry/CLT and independent-region assumptions are described in the viewer.
Salary CR2 p-values are .172/.194 and no Table 2 coefficient rejects at 5%.
Preserve this qualification when editing the narrative. Full diagnostics and
cluster scores are staged; run `Rscript temp/tests/check_table2_clustering.R` after
changes. The conventional Panel A stars are retained for comparison only.

## Dual output export

Generated LaTeX tables and the employment figure are retained in `output/` and
also mirrored to `tables/` and `figures/` in the Dropbox project
`/Users/user/Library/CloudStorage/Dropbox/Apps/Overleaf/MTI and Employment - Revision (AE)`.
`code/helpers/export_paths.R` centralizes this behavior. Override with
`Sys.setenv(MTI_OVERLEAF_DIR = "/another/project")`; set it to `""` to disable
external exports (also use this for isolated analysis tests). A missing or
unwritable destination produces a warning and retains local results.
Existing changed destination files are backed up by content hash under
`temp/overleaf_backups/` before replacement. This includes legacy exports
whose specifications/headings differ from the historical manuscript tables.
Only generated tables/figures are mirrored; manuscript prose, raw data and CSV
diagnostics are not copied. Table 3 now writes `output/tables/table_rev.tex`; the archived local manuscript
is not updated or required. The personal viewer reads this staged copy. Explicit output-path arguments to
`07_table_main.R` and `09_table_rev.R` do not trigger the Dropbox mirror.
Descriptive figures use the base PDF device to avoid the optional Cairo/XQuartz
system dependency. The default pipeline now needs no local manuscript folder.

## Raw-data release migration (R7)

The active layout now follows the supplied release folders (`yl_r1` through
`yl_r7`, phone/school surveys, and `yl_const_r1_r7`).
`code/helpers/raw_data_catalog.R` is the authoritative path map for active scripts.
The current models retain their original rounds and use
`rawdata/legacy_constructed_r1_r5/ethiopia_constructed.dta`. The new constructed
release changes historical values/missingness and schema: **do not substitute it
for the legacy input or assume the releases are interchangeable**. This choice
was explicitly requested to preserve replication.

Round 7 and the latest constructed release are inventoried separately by
`code/helpers/inventory_round7.R`; this exports aggregate schema/row counts only and
does not fit new models or change employment outcomes. All original raw files,
including documentation not found by filename in the new release, remain in
an ignored recoverable snapshot under `_archive/raw_data_snapshots/`.
Audit reports, the source-to-local SHA-256 manifest, and the pre-migration
analysis baseline are under `temp/raw_data_update_audit/`. The reusable
`temp/audit_raw_release.R` compares matched Stata files; its command-line
arguments specify both roots, a crosswalk CSV, and the report location.
`temp/audit_constructed_release.R` reports historical differences without
exporting participant records. Candidate aliases in that report are diagnostic
suggestions, not automatic recoding rules. Dropbox export was disabled during
migration verification.

Migration verification completed: 1,610 supplied files were copied with SHA-256
checks. Of 1,058 legacy non-constructed Stata files, 1,050 match exactly; seven
Peru files and one Vietnam file differ. No parse errors remain. All current
Ethiopia inputs and all 49 fitted-model snapshots are unchanged; the four
processed CSVs and generated table bodies/diagnostics match the baseline.
See `temp/raw_data_update_audit/README.txt` and `replication_verification.json`.
Latest constructed data have multiple Round 6 calls per child: do not assume
`childid` plus `round` uniquely identifies records throughout R1--R7.

The original Dropbox project `MTI and Employment Outcomes NFR` is protected as
the pre-revision archive. Its eight tables and employment figure were restored
from the backups preceding the first automated export. The helper rejects any
attempt to export there, including through `MTI_OVERLEAF_DIR`. Revision exports
go to `MTI and Employment - Revision (AE)`; version backups are now namespaced
by destination folder under `temp/overleaf_backups/`. Restoration evidence and
a recovery snapshot are in `temp/overleaf_project_recovery/`.

## Folder reorganization verification (2026-10-31)

Active analysis code is in `code/`, survey inputs in `rawdata/`, and personal
validation/viewer files in `temp/tests/` and `temp/results_viewer/`. Run from the
repository root with `Rscript code/_master.R`; the ignored RStudio project file
is optional. Viewer build paths account for the extra `temp/` directory.
The full pipeline completed with Dropbox exports disabled. All 49 model
coefficient/covariance/sample-size snapshots match the verified baseline; all
four analysis CSVs, table bodies and diagnostics are unchanged. Figure text
matches, targeted path/export/clustering checks pass, and the viewer compiles
to 11 pages. Logs and comparison evidence: `temp/layout_cleanup_audit/`.

## Script order and shared helpers

Run `Rscript code/_master.R` from the repository root. Analysis scripts are
listed below in the order first sourced, including nested helpers. Core scripts
retain their existing numbers; shared helpers live in `code/helpers/`:

1. `code/01_extract_clean.R`
2. `code/02_cleaning_yc.R`
3. `code/03_descriptives.R`
4. `code/helpers/export_paths.R` (loaded by descriptive/table exporters)
5. `code/05_estimation.R`
6. `code/06_export_res.R`
7. `code/07_table_main.R` (sourced by the regression exporter)
8. `code/helpers/cluster_inference.R` (loaded by the Table 2 generator)
9. `code/09_table_rev.R`

The raw-data catalog, Round 7 inventory, export-path helper and regional
inference helper live in `code/helpers/`. These are required pipeline code.
Only the two standalone release-audit scripts remain directly in `temp/`;
tests and the personal viewer also remain there. The active pipeline has no
source dependencies in `temp/`. The master loads the catalog during setup
and inventories Round 7 after data extraction.
Numbering describes source order, not standalone execution: regression exports
still require fitted objects from estimation in the same R session.

Session setup lives in `code/helpers/_setup.R`, sourced first by
`code/_master.R`. It clears workspace objects, validates paths, installs missing
packages and attaches dependencies. Setup variables remain private to `local()`.

## Cleaning-script refactor and constructed-release check

The two extraction scripts now share `code/helpers/extraction_helpers.R` for
explicit survey-code lookups and the MTI schooling cap. Intermediate frames
use descriptive names, inactive alternatives are removed, and the school-test
joins specify their survey keys. The cleanup preserves the legacy samples,
recode rules and output column order. All four CSVs and all 49 model snapshots
match the prior results. The historical `Amaharic` spelling and missing-grade
fallback remain unchanged because repairing them would be a separate analysis
change.

`temp/compare_constructed_analysis_inputs.R` compares the R1--R7 constructed
release in an isolated directory with external exports disabled. It documents
provisional aliases, reconstructs the death flag from labelled interview status,
and restricts to R1--R5. The schooling alias changes definition; it is a
sensitivity mapping, not an approved replacement. The latest `elecq` is missing
for every R1--R5 record, leaving zero complete cases for all four Table 2
specifications (legacy N: 437/279/334/192). Other historical fields also change.
Keep legacy constructed data as the production default; do not drop the
electricity control or fill values silently to obtain estimates. Audit evidence
is in `temp/cleaning_refactor_audit/`.

## Descriptive, estimation and export script polish

`code/helpers/analysis_helpers.R` centralizes older-cohort CSV loading, factor
conversion, the zero-language-score filter and available-case standardization.
The descriptive script builds Table 1 through reusable summary functions and
an ordered label map. The estimation script groups related fits using outcome
maps and explicit formula builders; original fitted-model identifiers remain
available to the export script. Intermediate frames have descriptive names.
Unexported exploratory output and inactive alternatives have been removed.
The exporter shares panel formatting and notes while preserving its original
column order and known historical heading issues.

Against a fresh baseline, all 49 coefficient/covariance/N snapshots, Table 1
data, and Figure 1 data/layers match (tolerance 1e-12). Exported table bodies
and diagnostics are unchanged apart from Table 1 trailing whitespace. The
viewer uses the shared loader when refreshing its figure. Validation evidence
is in `temp/analysis_polish_audit/`; external analysis exports were disabled.

## Constructed-release replication audit (2026-11-03)

Run `Rscript code/auxiliary/compare_constructed_releases.R` from the project
root for an isolated legacy/updated comparison; see
`temp/constructed_release_comparison/comparison_report.html`. All four stored
analysis CSVs, 49 prior model snapshots and ten staged table/diagnostic files
match a fresh legacy run. Updated-only inputs, with documented provisional
aliases and R1--R5 restriction, leave zero complete cases for all 49 models
and both Table 3 association fits because historical electricity is absent.
A separately labelled hybrid diagnostic carrying forward only legacy electricity
makes all 49 models estimable, but 32 coefficient vectors and 26 sample sizes
change; it is not updated-release-only replication. Schooling definitions also
change. The production legacy default remains intact. Audit workers disable
Overleaf exports and preserve production data/results; no raw inputs are edited.

Electricity follow-up: `code/auxiliary/investigate_constructed_electricity.R`
writes `temp/electricity_investigation/electricity_investigation.html`.
The updated services-index identity recovers 14,117 historical electricity
values, all matching legacy data. Original R3 OC/YC household `elecq` reproduces
all 2,999 legacy R3 values/missingness (2,858 observed), allowing reconstruction
without importing the legacy constructed control. The supplied updated
construction code is unavailable; an assembly/retention omission is the working
diagnosis, not a confirmed depositor explanation. No production repair is
implemented; schooling-definition and other release differences remain.

## Replication with original R3 electricity (2026-11-03)

`code/auxiliary/reconstruct_round3_electricity.R` provides
`reconstruct_round3_electricity()`, which joins original OC/YC R3 household
`elecq` by `childid`, checks cohort and key uniqueness, and populates the
compatibility field `elecq_new` for R3 only. The updated `elecq` source column
and other rounds are unchanged. It preserves 141 missing values (one source
response plus 140 unmatched households); 2,858 values are observed.

Run `Rscript code/auxiliary/compare_constructed_releases.R temp/constructed_release_comparison_r3_electricity`.
The audit now includes `updated_raw_r3`, reruns the full pipeline and regional
inference in isolation, and saves `raw_r3_replication_report.html` and ten
revised table/diagnostic outputs under `raw_r3_tables/`. All 49 models and both
Table 3 association fits are estimable. All four extracted CSVs and all 49
model snapshots match the prior hybrid diagnostic, using original household
sources rather than legacy constructed electricity. Against the legacy
baseline, 17 coefficient/covariance vectors and 23 model sample sizes match;
32 vectors and 26 sample sizes differ. Main Table 2 N changes from
437/279/334/192 to 442/283/337/194. All four revised CR2 p-values exceed 0.05.
The provisional schooling alias is still a substantive definition difference.
The production catalog, analysis CSVs, staged results and Overleaf exports
remain unchanged; this is an isolated replication check, not a default-input switch.

## Attribution of remaining release differences (2026-11-03)

Run `Rscript code/auxiliary/attribute_constructed_differences.R`, then
`Rscript code/auxiliary/write_constructed_attribution_report.R`.
The report is `temp/constructed_difference_attribution/remaining_differences_report.html`.
Nineteen isolated scenarios identify the differences in the current pipeline.
Diagnostic restoration of five blocks (`hghgrade`; `preprim`/`agegr1`;
`entype`; `deceased`; `wi_new`/`hq_new`) reproduces all four analysis datasets'
values, all 49 coefficient/covariance/N snapshots, both Table 3 fits and
Table 1/Figure 1 at tolerance 1e-10. These restorations attribute changes;
they are not approved harmonization or a production-input switch.

Main employment differences are fully accounted for by schooling and death
status: no legacy complete cases are lost; schooling supplies 4/3/2/1 new
complete cases and the revised death status adds one case to each column.
IMTI also changes on 34/13/32/11 shared complete cases, while their outcomes
and E_is are unchanged. Seven death-flag child-round changes affect five
participants. The older-cohort re-entry is in Oromia with original R5 salary
activity 13; both legacy-deceased/updated-interviewed R5 participants have raw
R5 child interview records, although exact death timing is not established.

All newly missing `preprim`, `agegr1`, and `entype` observations have inround=0.
Early-school background missingness affects six fitted models in the updated
comparison; entype has no independent effect across the eight matched
factorial pairs. The remaining four younger-cohort fits reflect one Addis
Ababa R3 concrete-block household (wallr3=25): hq_new rises .60 to .85 and
wi_new .733333 to .816667, consistent with TN63's documented revision.
Production data, staged outputs and Overleaf exports remain unchanged.

## Planned R6/R7 separate cross sections (2026-11-03)

User decision: retain the legacy R1--R5 constructed release for the existing
sample and results. Investigate R6 and R7 as separate cross sections; do not
switch the current sample to the updated constructed release or require a
balanced panel. The materials/plan report is
`temp/r6_r7_extension_feasibility/extension_feasibility_report.html`.
Reproduce with `Rscript code/auxiliary/inventory_extension_rounds.R` and
`python3 code/auxiliary/build_extension_feasibility_report.py`.

Fifty Ethiopian raw modules and 68 documentation/archive files are inventoried.
A 16-block crosswalk recommends an older-cohort employment extension using
frozen legacy treatment/instrument/background and a single target call per R6
cross section. Call 2 (2020) is the provisional primary choice; Call 5 (2021)
is a separate sensitivity and has no Tigray records in the inspected raw core.
R6/R7 weekly employment is feasible; the existing annual R5 first-activity
outcome is not exactly equivalent. R7 annual activity-type detail is available
in person, not in the inspected phone core. New OC cognitive outcomes and a
full common contemporary wealth/control battery are unavailable across modes.

Before building samples, reconcile the R6 raw/constructed participation flags:
774 raw Call 2 OC IDs match cohort IDs, but 182 have constructed incall=0
(181 record primary consent=yes). This is an unresolved eligibility discrepancy,
not permission to include all raw rows or discard them automatically. R7 files
have 2,038 in-person plus 193 phone records, no cross-mode duplicate IDs;
report prose says 205 phone while its table agrees with total 2,231. Keep that
12-case discrepancy explicit. Historical-control coverage counts are ceilings,
not final outcome-specific N. No new regressions were estimated or manuscript
outputs changed in this feasibility audit.

## R7-only reassessment: outcomes and current controls (2026-11-03)

User narrowed scope to R7; R6 feasibility is postponed. Earlier-round treatment
and background are allowed; fresh childhood measurements are not prerequisites.
See `temp/r7_extension_reassessment/r7_reassessment_report.html`, generated
by `code/auxiliary/audit_r7_extension.R` and
`code/auxiliary/build_r7_reassessment_report.py`.

R5 questionnaire PDF page 36, section 5.2, confirms activities are ranked by
annual time spent, most important first. This resolves the prior report's
uncertainty about ACTIDR5=1 versus R7's annual main-activity selection rule.
R7 OC in-person: 451 annual workers all have annual activity types; 181 report
no annual work. The primary feasible extension is annual in-person employment
with historic treatment/background plus contemporaneous household controls.
Candidate non-Addis N: salary/wage 358/271 with frozen controls; 352/266 when
updating household size, wealth/housing/CD/electricity (also 352/266 if updating
food situation). No new model estimates have been fitted.

Use R7 wi/hq (without _new) to align the legacy wall-material definition:
legacy R5 wi_new/hq_new match those newer un-suffixed fields on all 2,613
paired observed R5 records. R7 _new variants adopt concrete blocks as good walls.
Constructed R7 hhsize is entirely missing; reconstruct current livhser7=1 roster
members including index participant, requiring cmplthhr7=1. OC size is available
for 626/632 in-person and 64/64 phone cases under that rule.

Phone annual job types are absent. OC phone constructed indices/electricity
are populated despite no matching housing/assets/electricity questions in the
short raw instruments; their contemporaneous provenance is unverified and
must not be assumed. A both-mode weekly/current-job sensitivity can update
size/food while retaining historic wealth. Linked phone-only non-Addis samples
have one E_is value and one policy region; standalone phone-only IV is not
identified. Preserve raw activity labels: R5/R7 code 19 is housemaid, 20 other
non-agriculture; R7 aliases 21/22 map to self-employed crops/livestock (1/4).
The legacy production sample/results remain unchanged.


## R7 in-person master: both cohorts (2026-11-03)

Run `Rscript code/10_build_r7_cross_section.R`, then
`python3 code/auxiliary/build_r7_cross_section_report.py` from the project root.
The separate `data/r7/r7_inperson_analysis.rds` / `.csv` has one row per
`child_id`: 632 OC and 1,406 YC (2,038 total), in person only. All 193 raw phone
IDs are excluded. Derived participant files are ignored by Git. No legacy
CSV, existing result, main workflow or Overleaf output is replaced.

All YC histories link directly to the existing legacy lookup; one OC history
is recovered in an isolated extraction with the historical deceased filter
relaxed. Existing historical values take precedence and are checked unchanged.
The master retains every in-person respondent; eligibility/completeness are
flags. OC's original language-score gate applies only to the comparison flag,
not the master; YC does not acquire this gate. Historical fields use `_hist`.

Annual/weekly job outcomes preserve the original positive/exclusion rules;
explicit all-respondent alternatives have different denominators. Current
roster size requires completed resident rosters. Raw original-definition
housing, services, durables and wealth reconstructions match the imported
R7 indices on all paired observed cases. Raw missing components propagate;
imported and strictly reconstructed fields remain separate. Original-definition
R7 WI/HQ are sourced from un-suffixed constructed `wi`/`hq`, not `_new`.

YC employment outcomes and the employment-control battery are available or
constructible, with observation-level missingness. Frozen-R5-MTI non-Addis
candidate complete cases are salary/wage 862/604 YC and 352/266 OC; replacing
R3 food situation with R7 gives YC 864/606 and OC 352/266. No regressions fitted.
The common control completeness includes caregiver sex for both cohorts.

**Review YC treatment before estimation:** 1,047 YC have R5 schooling below
eight or missing. The provisional `IMTI_r7_schooling_candidate` differs from
frozen IMTI on 458 paired YC cases; it preserves original language/policy rules
and is not a validated exposure measure. Check qualifications, inconsistent
schooling reports and language/policy timing. Raw R7 YC reading items are
retained (1,138 any / 945 all 12 nonmissing); the score stays missing pending a
validated key and comparability assessment. No common fresh R7 maths battery
is supplied. R6 remains deferred.

Full findings: `temp/r7_cross_section_build/build_report.html`; aggregate
availability, source-code labels, sample counts and integrity audits are
alongside it. Recheck exact-sample first stages and few-region inference when
estimating the new cohort/round specifications.


## Audit locations and R7 coding conventions (2026-11-03)

Audit/report folders, codebooks and Overleaf backup/recovery folders previously
under `output/` now live under `temp/` and are individually ignored by Git.
Keep `output/tables/` and `output/figures/` as the active result staging paths.
Update generators and readers together when moving an audit folder; do not
rewrite archived evidence copies merely to change a historical path.

`code/10_build_r7_cross_section.R` follows the R4DS/tidyverse style and DIME
readability principles: named sections, descriptive objects, explicit keyed
joins, `mutate()`/`case_when()` recodes, and grouped/long-form audit summaries.
`code/helpers/r7_build_helpers.R` contains historical lookup recovery and the
explicit provisional MTI rules; dynamic parsing/evaluation is no longer used.
The standalone script is also readable section-by-section in RStudio.
Validation compared every column (including attributes and missingness) with
the pre-refactor RDS: all 2,038 records and 212 columns match exactly. The dual
export test passes with backups stored under `temp/overleaf_backups/`.


R7 audit extraction: `code/auxiliary/audit_r7_cross_section.R` now holds the
aggregate exports, dictionary, candidate counts and final integrity audit.
The main R7 build sources it after saving the dataset; run the main script
for the complete workflow. Keep R7 main/helper/audit code within 80 columns,
with blank lines between distinct operations and multiline long calls.


## Compatible R5/R7 employment files

Run `Rscript code/11_prepare_r5_r7_analysis.R` after the R7 master build.
It writes labelled tibbles and CSVs under `data/r7/`: `r7_append_ready`,
`r5_append_ready`, and `r5_r7_appended` (same extensions .rds/.csv).
The common employment schema preserves legacy variable names and exclusions.
Round indicators: `round` (5/7), `round7` (0/1); cohort: `yc` and
`cohort_young` (0 old/1 young), plus character `cohort` (old/young).
The appended key is `(child_id, round)`; repeated IDs across rounds are valid.

All 2,904 original R5 lookup rows and 2,038 in-person R7 rows are retained.
Existing R5 YC employment is structurally missing. These files impose no
complete-case or historical language-score gate. Apply legacy gates explicitly
when comparing old estimation samples. Historical R4 scores are named with
`_r4` to avoid presenting them as fresh outcomes. No R7 grade/qualification is
silently substituted for the broader legacy completed-schooling scale.

R7 `hhsize`, `wi_new`, `hq_new`, `cd_new`, `sv_new`, `elecq_new` use current
R7 measures with original definitions; the R5 rows retain their original R3
controls. `household_measurement_round` records this timing. `foodsec_r3`
remains historical. `IMTI` remains frozen at R5 and YC exposure is flagged for
review; this preparation does not validate completed YC exposure or fit IVs.
Aggregate checks/notes live in `temp/r7_append_preparation/`; the check code is
`code/auxiliary/audit_r5_r7_append.R`. Original source CSVs remain unchanged.


## R5/R7 IV comparison

`Rscript code/12_compare_r5_r7_regressions.R` reads only the appended RDS
for fitting. It reproduces Table 2's four OC R5 employment IV coefficients,
SEs, N and first-stage F at tolerance 1e-9, then pools rounds with a round
dummy for OC, and adds an exploratory both-cohort panel with cohort dummy.
YC exposure remains frozen/unvalidated and YC enters employment only in R7.
The OC historical R4 language-score gate is retained in both rounds.
No participant fixed effects are used. Exact samples, models and regional
CR2/AR diagnostics are in `temp/r5_r7_regression_comparison/`.
`code/auxiliary/write_r5_r7_comparison.R` writes the new
`output/tables/table_r5_r7_comparison.tex`, mirrored to the revision project.
The revision main.tex includes it after Table 2, with prior source backed up
under temp/overleaf_backups. Existing manuscript tables are preserved.


Latest comparison specification: cohort dummy removed from the pooled model;
Panel A is R5 OC replication and Panel B pools R5 OC with R7 both cohorts,
controlling for round only in addition to the original covariates. R5
replication remains verified. Both panels retain conventional SEs/stars and
ordinary N. Regional diagnostics remain in the temp comparison audit.

Run `Rscript code/13_pooled_summary_statistics.R` for pooled Table 1,
`output/tables/tableI_pooled.tex`, mirrored to the revision project and used
by its main.tex instead of the preserved legacy Table 1. Available-case
statistics use R5 OC plus R7 both cohorts, the original OC language-score
gate, no new YC score gate, and no regression complete-case restriction.
The table adds old/young cohort and R7 shares. Scores and highest grade are
explicitly historical R4/R5 measures; household-control timing is documented.
Audit data: `temp/pooled_summary_statistics/`. Both updated tables compile
without overflow in the two-page preview there.


## Pooled-only robustness and wild IV bootstrap

Run `Rscript code/14_pooled_robustness.R`. The separate table compares baseline,
cohort control, excluding Amhara, and relaxed OC historical language-score
gate, all pooling R5/R7 both cohorts with round dummy and original controls.
Conventional SEs/stars remain. Additional rows report region-cluster CR2,
Wang--Zhang (2024) W-B-S null-imposed IV bootstrap p-values, unrestricted-centred
bootstrap coefficient dispersion (not a calibrated small-G SE correction),
and the existing local weak-IV-robust wild AR score sensitivity. Enumerate all
16/8 region sign vectors. This is not an automatic cure for only 4/3 regions.
The method assumptions and limitations are in
`temp/pooled_robustness/method_notes.txt`. The helper is
`code/helpers/wild_iv_few_regions.R`; the auxiliary table writer is
`code/auxiliary/write_pooled_robustness.R`. An independent AER/HC0 check of 24
bootstrap samples passes. Eight bootstrap p-values are .25--.375. The new
`table_pooled_robustness.tex` is mirrored to and included in revision main.tex.
Previous tables remain. YC frozen exposure remains explicitly exploratory.


Latest pooled robustness update: cohort dummy is included in all eight models.
Column 1 omits the round dummy; columns 2--4 include it. Columns 1 and 2 use
identical observations. Column 3 excludes Amhara, and column 4 relaxes OC's
historical score gate. All bootstrap/CR2 diagnostics have been re-estimated.
The previous cohort-plus-round specification (column 2) is reproduced.


Employment missingness audit: run
`Rscript code/auxiliary/audit_employment_missingness.R`.
Findings and aggregate counts are in `temp/employment_missingness/`.
Table 1 missing outcomes are fully attributed to legacy activity exclusions
and R5 nonresponse: non-Addis wage/salary 1,127/548 missing, including 92
nonrespondents each; Addis 89/80 missing, including 28 nonrespondents each.
All 120 unknowns have legacy R5 inround=0 and no raw activity record.
All R7 missing employment outcomes are deliberate activity exclusions.
These means use outcome-specific comparison groups, not all respondents.
No recode or regression was changed by the audit.


Complete source-file codebooks: run
`Rscript code/auxiliary/create_raw_source_codebooks.R` and open
`temp/codebooks/raw_sources/index.html`. Nineteen HTML codebooks cover all
7,571 source variables, including 95 entirely missing fields. Sources include
both constructed releases, raw outcome/cognitive/household files for both
cohorts, and school-survey inputs. Supplementary phone-exclusion, electricity
reconstruction and school-context inputs are marked explicitly. Each codebook
retains source variable/value labels and missingness. Combined/per-file
metadata CSVs and checksummed source manifests accompany the HTML index.
No participant records or string response values are exported. Scope is all
Stata inputs to the current local construction, plus marked validation/context
sources; provider-internal upstream constructed-data inputs and R6 are outside
this collection. Coverage and all index links were verified.


ACTIDR5 clarification: `code/auxiliary/investigate_actidr5.R` and findings
under `temp/actidr5_investigation/`. Questionnaire PDF p36 orders three paid
or unpaid activity slots by importance in time spent, but separately selects
the main activity in Q10 using CAPI time. Raw ACTIDR5=4 has three records
outside the three-slot questionnaire; provenance is undocumented. Dictionary
PDF p89 calls ACTIDR5 Paid Activities without numeric value definitions.
ACTMSTR5 in olderchild (dictionary p48) stores selected activity-type codes;
48/641 paired values differ from ACTR5 at ACTIDR5=1. This qualifies earlier
statements equating first-listed R5 activity with final main activity. Existing
outcome definitions and results remain unchanged pending a separate audit.


## November 4: user-revised R5 activity schema

The extraction now retains four R5 activity slots and supplies salary_employ,
wage_employ, self_agri, wage_agri and self_nonag. Salary is code13 in ANY
slot; wage is code12/19 in ANY slot. These are substantive user changes,
not aliases of the former first-activity outcomes or their exclusions.

Master dependencies now use salary_employ in descriptives, estimation and
Table 2; self_nonag replaces the old nonfarm outcome name. Per the user's
explicit choice, `prepare_employment_outcomes()` in analysis_helpers.R
reconstructs combined `self_employ` from self_agri OR self_nonag. It preserves
unknowns unless one component is a known positive. Existing numeric model
IDs are retained so 06_export_res.R consumes the correct model objects.
Addis's existing first-activity exclusion now references type_activ_1.

05_estimation.R retains the user's direct RDS sample loading (no automatic
R4 score gate). 07_table_main.R now uses the same RDS input and sample rule.
03_descriptives.R and 09_table_rev.R retain their existing R4 score gates;
their filters were not changed as part of the naming repair. Table 2 notes
now describe the any-activity taxonomy and no longer hardcode older regional
inference conclusions. The R7 historical-recovery helper now saves OC
analysis_data_final, matching the user's renamed extraction object.

Full _master.R completed in temp/variable_schema_update/master_run/ with
MTI_OVERLEAF_DIR='' (see master.log). All active R scripts parse; combined
self-employment matches codes1-4/8-11 in any raw slot and NA logic was checked.
All 32 pre-existing live data/results files retain their checksums. Only
isolated validation artifacts were regenerated; no live/Overleaf outputs
were overwritten. Existing standalone R7/pooled files retain the previous
first/main-activity definitions; updating those is a separate harmonization
step, because renaming would not make the new R5 definitions equivalent.
