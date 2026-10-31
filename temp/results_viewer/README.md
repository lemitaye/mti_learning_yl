# Local results viewer

Open `main.pdf` for the compiled personal results notebook, or edit `main.tex`
in your LaTeX editor. This project is independent of the manuscript bibliography.
It displays eight tables and the employment figure, with source and model notes.

From the repository root:

```sh
./temp/results_viewer/build.sh
```

The script works from any starting directory. It requires R with tidyverse and
scales, plus a LaTeX distribution with latexmk and standard packages listed in
main.tex. It uses pdfLaTeX and Latin Modern, with no system font dependency.

The build refreshes only the viewer's employment PDF from existing processed
data, reusing the `non_aa_samp` and `employ_gr` expressions in descriptives.R.
It uses the base PDF device because Cairo/XQuartz is unavailable locally.
It does not run regressions, rewrite manuscript files, or copy staged results
into the manuscript. Tables are included directly from their source files and
must already exist. Table 3 is included from output/tables/table_rev.tex. Keep this folder within the repository; it is not a
standalone copy of the results.

To refresh analysis first, run `Rscript code/master.R` from the repository
root, then rebuild the viewer. That analysis command overwrites data and outputs,
including manuscript Table 3. To update only Table 2, run
`Rscript code/table_main.R`, then rebuild.

Each section identifies the source scripts, sample, estimator, controls, and
known heading/unit inconsistencies. Viewer numbering is independent of manuscript
numbering. Compilation time does not certify a common estimation run: older
staged files may remain until their analysis is rerun.
