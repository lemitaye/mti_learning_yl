# Reuse the original descriptive plot expression without rerunning the analysis.
# Use base PDF because the local Cairo device may require unavailable XQuartz.
suppressPackageStartupMessages({library(tidyverse); library(scales)})
expressions <- parse('code/descriptives.R')
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name('<-')) && is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% c('non_aa_samp', 'employ_gr')) eval(expr)
}
stopifnot(exists('employ_gr'))
dir.create('temp/results_viewer/assets', showWarnings = FALSE)
ggsave('temp/results_viewer/assets/employ_gr.pdf', plot = employ_gr,
       device = grDevices::pdf, width = 200, height = 120, units = 'mm')
