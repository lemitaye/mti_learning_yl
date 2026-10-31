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
Rscript code/master.R
```

This **writes data and results** and is not a harmless smoke test. Inspect the
known blockers below and preserve current manuscript tables before running it.

| Order | Script | Output / dependency |
| --- | --- | --- |
| 1 | `code/extract_clean.R` | Older cohort: `data/aa_samp.csv`, `data/non_aa_samp.csv`. |
| 2 | `code/cleaning_yc.R` | `data/ylsample_yc.csv`, `data/schsur_yl_joined.csv`. |
| 3 | `code/descriptives.R` | `output/tables/tableI.tex`, `output/figures/employ_gr.pdf`. |
| 4 | `code/estimation.R` | Regression objects in memory. |
| 5 | `code/export_res.R` | `output/tables/tableII.tex` through `tableVI.tex`, using step 4 objects; also sources `table_main.R` for staged `tableII_mod.tex` and diagnostics CSV. |
| 6 | `code/table_rev.R` | Writes `output/tables/table_rev.tex` and mirrors to the revision project. |

`master.R`, `descriptives.R`, and `estimation.R` clear workspace objects.
`export_res.R` relies on models from `estimation.R` in the **same R session** and
on already loaded packages. Standalone script execution generally requires
checking its package and workspace dependencies first.

The master script loads `tidyverse`, `haven`, `scales`, `stargazer`, `AER`, `lfe`,
`starpolishr`, `fastDummies`, and `xtable`. Existing setup instructions obtain
`starpolishr` from `ChandlerLutz/starpolishr` on GitHub. `master.R` now installs
missing CRAN packages and bootstraps `remotes` for GitHub installation when
needed, then verifies availability before attaching packages. It also checks
project paths and creates generated-output directories. There is no dependency lockfile. Targeted checks live in `temp/tests/`.

### Known reproducibility issues (initial audit: 2026-10-31)

- Resolved during debugging: the unused `rf1oa`–`rf5oa` exploratory block is now
  commented out alongside its inactive `orom.amh` setup. Its formulas also
  targeted Tigray; do not reactivate it without specifying that comparison.
- Resolved during debugging: exploratory plots in `descriptives.R` now use
  `non_aa_samp` and the correct ordering column, and within-region score
  standardization uses `mutate()` rather than multirow `summarise()`.
- The repaired master pipeline completed in isolated copies on 2026-10-31.
  Against original result-producing code with broken exploratory blocks skipped,
  all 49 regression coefficient/covariance/sample-size snapshots, Table 1 data,
  Figure 1 layers, and six staged table bodies matched exactly. Rebuilt CSVs
  matched repository copies; Table 3 matched the manuscript body. This does not
  resolve the historical table-generator differences described below.
- Table 2 is now reconstructed by `code/table_main.R` (standalone or via
  `export_res.R`), preserving original employment estimates/SEs/N and adding
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
- `table_rev.R` fits `std_lang ~ inst + controls`, with `inst = E_is * IMTI`,
  using `felm` without an IV slot. This is an OLS association, despite its
  comments calling it a channel first stage. It is not a completed 2SLS channel
  analysis. Standardization occurs after the language-score filter and before
  excluding Amhara. Describe the implemented estimator accurately.
- `export_res.R` builds tables with `stargazer` and `starpolishr`; positional
  header/row insertions are sensitive to layout changes. `table_rev.R` writes
  LaTeX directly.

## Manuscript result mapping and build

Table numbers follow active `main.tex` input order, not filename numbering:

| Paper | Manuscript file | Generator / status |
| --- | --- | --- |
| Table 1 | `tables/tableI.tex` | `descriptives.R`; output staged first. |
| Table 2 | `tables/tableII_mod.tex` | `table_main.R`, also called by `export_res.R`; original estimates reproduced, diagnostics added. |
| Table 3 | `tables/table_rev.tex` | `table_rev.R`; local staging and revision export. |
| Table 4 | `tables/tableIII.tex` | December 2023 version; generator mismatch. |
| Table 5 | `tables/tableV.tex` | December 2023 version; generator mismatch. |
| Figure 1 | `figures/employ_gr.pdf` | `descriptives.R`; output staged first. |
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

`table_main.R` sources `cluster_inference.R` locally. Required packages now
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
`code/export_paths.R` centralizes this behavior. Override with
`Sys.setenv(MTI_OVERLEAF_DIR = "/another/project")`; set it to `""` to disable
external exports (also use this for isolated analysis tests). A missing or
unwritable destination produces a warning and retains local results.
Existing changed destination files are backed up by content hash under
`output/overleaf_backups/` before replacement. This includes legacy exports
whose specifications/headings differ from the historical manuscript tables.
Only generated tables/figures are mirrored; manuscript prose, raw data and CSV
diagnostics are not copied. Table 3 now writes `output/tables/table_rev.tex`; the archived local manuscript
is not updated or required. The personal viewer reads this staged copy. Explicit output-path arguments to
`table_main.R` and `table_rev.R` do not trigger the Dropbox mirror.
Descriptive figures use the base PDF device to avoid the optional Cairo/XQuartz
system dependency. The default pipeline now needs no local manuscript folder.

## Raw-data release migration (R7)

The active layout now follows the supplied release folders (`yl_r1` through
`yl_r7`, phone/school surveys, and `yl_const_r1_r7`).
`code/raw_data_catalog.R` is the authoritative path map for active scripts.
The current models retain their original rounds and use
`rawdata/legacy_constructed_r1_r5/ethiopia_constructed.dta`. The new constructed
release changes historical values/missingness and schema: **do not substitute it
for the legacy input or assume the releases are interchangeable**. This choice
was explicitly requested to preserve replication.

Round 7 and the latest constructed release are inventoried separately by
`code/inventory_round7.R`; this exports aggregate schema/row counts only and
does not fit new models or change employment outcomes. All original raw files,
including documentation not found by filename in the new release, remain in
an ignored recoverable snapshot under `_archive/raw_data_snapshots/`.
Audit reports, the source-to-local SHA-256 manifest, and the pre-migration
analysis baseline are under `output/raw_data_update_audit/`. The reusable
`code/audit_raw_release.R` compares matched Stata files; its command-line
arguments specify both roots, a crosswalk CSV, and the report location.
`code/audit_constructed_release.R` reports historical differences without
exporting participant records. Candidate aliases in that report are diagnostic
suggestions, not automatic recoding rules. Dropbox export was disabled during
migration verification.

Migration verification completed: 1,610 supplied files were copied with SHA-256
checks. Of 1,058 legacy non-constructed Stata files, 1,050 match exactly; seven
Peru files and one Vietnam file differ. No parse errors remain. All current
Ethiopia inputs and all 49 fitted-model snapshots are unchanged; the four
processed CSVs and generated table bodies/diagnostics match the baseline.
See `output/raw_data_update_audit/README.txt` and `replication_verification.json`.
Latest constructed data have multiple Round 6 calls per child: do not assume
`childid` plus `round` uniquely identifies records throughout R1--R7.

The original Dropbox project `MTI and Employment Outcomes NFR` is protected as
the pre-revision archive. Its eight tables and employment figure were restored
from the backups preceding the first automated export. The helper rejects any
attempt to export there, including through `MTI_OVERLEAF_DIR`. Revision exports
go to `MTI and Employment - Revision (AE)`; version backups are now namespaced
by destination folder under `output/overleaf_backups/`. Restoration evidence and
a recovery snapshot are in `output/overleaf_project_recovery/`.

## Folder reorganization verification (2026-10-31)

Active analysis code is in `code/`, survey inputs in `rawdata/`, and personal
validation/viewer files in `temp/tests/` and `temp/results_viewer/`. Run from the
repository root with `Rscript code/master.R`; the ignored RStudio project file
is optional. Viewer build paths account for the extra `temp/` directory.
The full pipeline completed with Dropbox exports disabled. All 49 model
coefficient/covariance/sample-size snapshots match the verified baseline; all
four analysis CSVs, table bodies and diagnostics are unchanged. Figure text
matches, targeted path/export/clustering checks pass, and the viewer compiles
to 11 pages. Logs and comparison evidence: `output/layout_cleanup_audit/`.
