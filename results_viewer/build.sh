#!/bin/sh
# Run from any directory; compile the viewer without rerunning regressions.
set -eu
cd "$(dirname "$0")/.."
Rscript results_viewer/refresh_figure.R
cd results_viewer
latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
