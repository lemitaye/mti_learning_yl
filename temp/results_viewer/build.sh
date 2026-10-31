#!/bin/sh
# Run from any directory; compile the viewer without rerunning regressions.
set -eu
cd "$(dirname "$0")/../.."
Rscript temp/results_viewer/refresh_figure.R
cd temp/results_viewer
latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
