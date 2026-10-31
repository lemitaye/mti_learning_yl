# Validate availability and inventory the new Ethiopian resources without changing
# the existing R1--R5 analysis. No participant-level output is written.
local({
  source('scripts/raw_data_catalog.R', local = TRUE)
  paths <- c(list.files(raw_data_files[['round7_ethiopia']],
                       pattern = '[.]dta$', full.names = TRUE),
             raw_data_files[['constructed_r1_r7']])
  if (length(paths) < 2) stop('Round 7 modules or constructed release are missing.')
  inventory <- list(); schemas <- list()
  dir.create('output/raw_data_update_audit', recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(paths)) {
    dat <- haven::read_dta(paths[i])
    inventory[[i]] <- data.frame(file = paths[i], rows = nrow(dat), columns = ncol(dat))
    schemas[[i]] <- data.frame(file = paths[i], variable = names(dat),
      storage = vapply(dat, typeof, ''),
      label = vapply(dat, function(x) {a <- attr(x,'label', exact = TRUE);if(is.null(a)) '' else paste(a, collapse = ' | ')}, ''))
    if (identical(paths[i], unname(raw_data_files[['constructed_r1_r7']]))) {
      stopifnot(all(1:7 %in% dat$round))
      write.csv(as.data.frame(table(round = dat$round)),
                'output/raw_data_update_audit/constructed_r1_r7_round_counts.csv', row.names = FALSE)
    }
  }
  write.csv(do.call(rbind, inventory), 'output/raw_data_update_audit/round7_inventory.csv', row.names = FALSE)
  write.csv(do.call(rbind, schemas), 'output/raw_data_update_audit/round7_schema.csv', row.names = FALSE)
  message('Round 7 resources inventoried; existing estimation rounds are unchanged.')
})
