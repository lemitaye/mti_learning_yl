# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An empirical economics paper — *"Mother Tongue Instruction, Learning and Employment: Evidence from Ethiopia"* (Teferi Mergo, U. Waterloo; Lemi Daba, SPIA CGIAR). It estimates the causal effect of mother-tongue instruction (MTI) on cognitive test scores and employment using the **Young Lives** survey of Ethiopia (rounds 1–5) plus the 2012–13 School Survey. Identification is **IV-2SLS**. There is no application to build or test suite — the deliverables are the intermediate data files, the LaTeX tables/figures, and the compiled PDF. `README.md` is the human-facing reproduction guide; read it for the full pipeline and table mapping.

## Layout (after the 2026 cleanup)

```
scripts/            Analysis pipeline (run via scripts/master.R). Nothing else lives here.
raw_data/           Young Lives raw Stata .dta inputs. Read-only; git-ignored; large.
data/               Processed analysis CSVs, built by extract_clean.R + cleaning_yc.R.
output/             Staging for generated tables/figures (review, then copy into the manuscript).
MTI__learning_and_job_outcomes/   The live manuscript (LaTeX; an Overleaf project). Self-contained.
Archive/            Superseded drafts, exploratory scripts, notes. NOT used by the pipeline — do not wire anything here back in.
```

`Archive/` holds the previous mess: the old `tex/` draft, the `Young-Lives---Collaboration/` repo (an older full copy of the paper, its own git history), exploratory `data_extract_r*.R` / `revision.R`, preliminary `.Rmd` reports, `.docx` notes, and session junk. Treat it as write-once history.

## Running the analysis

RStudio project (`mti_learning_yl.Rproj`). **All script paths are relative to the project root**, so set the working directory there. `scripts/master.R` is the working orchestrator — it loads packages once, then sources the pipeline in order. Required packages: `tidyverse`, `haven`, `scales`, `stargazer`, `AER`, `lfe`, `fastDummies`, `xtable`, and `starpolishr` (GitHub: `remotes::install_github("ChandlerLutz/starpolishr")`).

Pipeline order (encoded in `master.R`):

1. `extract_clean.R` → `data/aa_samp.csv`, `data/non_aa_samp.csv` (older cohort)
2. `cleaning_yc.R` → `data/ylsample_yc.csv`, `data/schsur_yl_joined.csv` (younger cohort; read by `estimation.R`)
3. `descriptives.R` → `output/tables/tableI.tex`, `output/figures/employ_gr.pdf`
4. `estimation.R` → fits all models into the workspace
5. `export_res.R` → `output/tables/tableII…VI.tex`
6. `table_rev.R` → writes Table 3 straight into `MTI__learning_and_job_outcomes/tables/table_rev.tex`

**Workspace gotcha (unchanged):** `descriptives.R` and `estimation.R` each begin with `rm(list = ls())` (each reloads its data). `export_res.R` has **no** `rm()` / `read_csv` / `library` — it consumes the model objects `estimation.R` leaves in memory, so it must run in the same session immediately after it. `master.R` preserves this order.

Generated tables/figures land in `output/`; copy the ones the manuscript uses into `MTI__learning_and_job_outcomes/tables|figures` and compile `main.tex`.

## Manuscript ↔ source mapping (filename ≠ table number)

Table numbers follow the `\input` order in `MTI__learning_and_job_outcomes/main.tex`, which comments out and reorders files. Currently: Table 1 = `tableI.tex` (`descriptives.R`), Table 2 = `tableII_mod.tex`, **Table 3 = `table_rev.tex` (`scripts/table_rev.R`)**, Table 4 = `tableIII.tex`, Table 5 = `tableV.tex`; Figure 1 = `figures/employ_gr.pdf`, appendix map = `figures/map.png` (static). `tableII/IV/VI.tex` exist but are commented out.

**Reproduction caveat:** only `table_rev.R` is byte-verified against the current manuscript. Table 2/4/5 (`tableII_mod`, `tableIII`, `tableV`) were generated on 3 Dec 2023 on a Windows machine; the on-disk `export_res.R` is an earlier Nov-2022 vintage that produces different, earlier versions. To reproduce those exactly, the Dec-2023 generator must be recovered/reconstructed (as was done for `table_rev.R`). See README "Reproduction caveat".

## Identification strategy (needed to read the code)

The design lives in the `mutate()` blocks at the bottom of `extract_clean.R` (mirrored in `cleaning_yc.R`). Two constructed variables are the whole ballgame:

- **`IMTI`** — Intensity of Mother-Tongue Instruction, the *endogenous treatment*: years of schooling (`hghgrade_final_num`) top-coded (via `top_code()`) at a region/language-specific ceiling (e.g. 8 in Tigray/Oromia, 6 in Amhara, 4 in SNNP); 0 outside MTI cells. In the manuscript this is labelled **MTIuse**.
- **`E_is`** — the *instrument*, "assigned by nature": a 0/0.5/0.75/1 score for whether the child's ethnicity (`chethnic`) is the region's titular nationality and the primary-school language matches. Drives the first stage `IMTI ~ E_is`.

The sample is **split in two** because Addis Ababa has a different linguistic regime: `aa_samp` (Addis, controls add `entype_r4`, subset `type_activ != 19`) and `non_aa_samp` (Tigray, Oromia, Amhara, SNNP — the main sample). Estimation uses `lfe::felm` for 2SLS (`y ~ controls | 0 | (IMTI ~ E_is)`) and first stages (`.$stage1`).

`table_rev.R` (Table 3, "Likely Channels") is a first-stage regression of the standardized language ("Verbal") score on the interaction `E_is * IMTI` ("`E_is*MTIuse`"), over the whole non-Addis sample and excluding Amhara — testing whether MTI raises cognitive ability.

`lang_primary` (language of primary instruction) is itself constructed by proxy (round-4 reported instruction language if still in primary and non-English, else the round-3 math-test language) and feeds both `IMTI` and `E_is` — changing it changes the identification.

## Conventions

- Stata categorical codes are decoded to factors with large `case_when()` blocks (regions, languages, ethnicities, activity types). New round/variable → follow the `<var>_r<round>` naming and the "select per round, then `left_join` on `child_id`" pattern in `extract_clean.R`.
- `child_id` is the join key everywhere, derived as `str_remove(childid, "ET") %>% as.numeric()`.
- Employment outcomes: `wage_employII` = **salaried** employment, `wage_employ` = **wage** employment (a recent commit "corrected wage-salary confusion" — don't reintroduce it).
- Regression tables are built with `stargazer` + `starpolishr` (`star_panel`, `star_insert_row`, `star_notes_tex`, `star_tex_write`) in `export_res.R`; multi-panel header rows are inserted positionally (`insert.after`), so row offsets matter. `table_rev.R` instead writes its LaTeX directly (no `starpolishr` dependency) — a good template if reconstructing the other generators.
