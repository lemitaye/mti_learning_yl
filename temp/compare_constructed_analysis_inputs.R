# Diagnostic only: run from the repository root after the replication pipeline.
# Keep the legacy default intact. The latest release is tested in an isolated
# directory with Dropbox exports disabled; no raw inputs are edited.
# The schooling alias changes meaning, so this is a sensitivity exercise rather
# than an approved harmonization or an alternative manuscript specification.
local({
  root <- normalizePath('.')
  source('code/helpers/raw_data_catalog.R', local = TRUE)
  audit <- file.path(root, 'output/cleaning_refactor_audit')
  dir.create(audit, recursive = TRUE, showWarnings = FALSE)
  sandbox <- tempfile('latest_release_sensitivity-', tmpdir = audit)
  dir.create(sandbox, recursive = TRUE)
  stopifnot(file.copy('code', sandbox, recursive = TRUE),
            file.symlink(file.path(root, 'rawdata'), file.path(sandbox, 'rawdata')),
            file.copy('README.md', sandbox))
  dir.create(file.path(sandbox, 'data'))
  dir.create(file.path(sandbox, 'output'))

  legacy <- haven::read_dta(raw_data_files[['constructed_legacy']])
  latest <- haven::read_dta(raw_data_files[['constructed_r1_r7']])
  availability <- do.call(rbind, lapply(sort(unique(latest$round)), function(rd)
    data.frame(round = rd, rows = sum(latest$round == rd),
               observed_electricity = sum(!is.na(latest$elecq[latest$round == rd])))))
  write.csv(availability, file.path(audit, 'latest_electricity_availability.csv'), row.names = FALSE)
  latest <- latest[latest$round %in% 1:5, ]
  key <- function(x) paste(x$childid, x$round, sep = ':')
  stopifnot(!anyDuplicated(key(legacy)), !anyDuplicated(key(latest)),
            setequal(key(legacy), key(latest)))
  latest <- latest[match(key(legacy), key(latest)), ]

  # Record why a direct replacement cannot execute unchanged.
  required_legacy <- c('deceased', 'hghgrade', 'underweight', 'stunting', 'thinness',
                      'sv_new', 'cd_new', 'drwaterq_new', 'toiletq_new',
                      'elecq_new', 'cookingq_new')
  missing <- setdiff(required_legacy, names(latest))
  writeLines(c('Direct substitution is incompatible with the existing schema.',
               paste('Missing fields:', paste(missing, collapse = ', '))),
             file.path(audit, 'latest_direct_substitution.txt'))

  # Candidate aliases only: retain the new release's values and missingness.
  aliases <- c(underweight = 'underweight_child', stunting = 'stunting_child',
               thinness = 'thinness_child', hghgrade = 'hghschoolgrade',
               sv_new = 'sv', cd_new = 'cd', drwaterq_new = 'drwaterq',
               toiletq_new = 'toiletq', elecq_new = 'elecq', cookingq_new = 'cookingq')
  stopifnot(all(unname(aliases) %in% names(latest)),
            unname(attr(latest$status, 'labels', exact = TRUE)['Died']) == 2)
  latest$deceased <- ifelse(latest$status == 2, 1, NA_real_)
  for (v in names(aliases)) latest[[v]] <- latest[[aliases[[v]]]]
  write.csv(data.frame(
    legacy_name = c('deceased', names(aliases)),
    latest_source = c('status == 2; other statuses map to missing', unname(aliases)),
    note = c('Death flag reconstructed; interview statuses are not otherwise filtered.',
             ifelse(names(aliases) == 'hghgrade',
                    'School grade completed differs from old highest grade achieved.',
                    'Candidate alias; retain latest values, including missingness.'))),
    file.path(audit, 'latest_sensitivity_mapping.csv'), row.names = FALSE)
  saveRDS(latest, file.path(sandbox, 'constructed_candidate.rds'))
  for (script in c('01_extract_clean.R', '02_cleaning_yc.R')) {
    target <- file.path(sandbox, 'code', script)
    text <- readLines(target)
    text <- gsub('read_dta(raw_data_files[["constructed_legacy"]])',
                 'readRDS("constructed_candidate.rds")', text, fixed = TRUE)
    writeLines(text, target)
  }
  # Extract candidate inputs before attempting any regressions. This allows a
  # complete-case audit even when a required covariate is absent throughout.
  runner <- c('Sys.setenv(MTI_OVERLEAF_DIR = "")', 'options(warn = 1)',
              'source("code/helpers/_setup.R")',
              'source("code/01_extract_clean.R")',
              'source("code/02_cleaning_yc.R")')
  writeLines(runner, file.path(sandbox, 'extract_candidate.R'))
  setwd(sandbox)
  on.exit(setwd(root))
  exit_status <- system2(file.path(R.home('bin'), 'Rscript'), 'extract_candidate.R',
                         stdout = 'extraction.log', stderr = 'extraction.log')
  if (exit_status != 0) stop('Candidate extraction failed: ', sandbox)

  changed_fields <- list()
  for (file in c('aa_samp.csv', 'non_aa_samp.csv', 'ylsample_yc.csv', 'schsur_yl_joined.csv')) {
    old <- read.csv(file.path(root, 'data', file))
    new <- read.csv(file.path(sandbox, 'data', file))
    id <- if ('child_id' %in% names(old)) 'child_id' else 'childid'
    stopifnot(!anyDuplicated(old[[id]]), !anyDuplicated(new[[id]]))
    matched <- match(old[[id]], new[[id]])
    keep <- !is.na(matched)
    for (v in intersect(names(old), names(new))) {
      x <- old[[v]][keep]; y <- new[[v]][matched[keep]]
      valid <- !is.na(x) & !is.na(y)
      same <- is.na(x) & is.na(y)
      same[valid] <- x[valid] == y[valid]
      changed_fields[[length(changed_fields) + 1]] <- data.frame(
        dataset = file, variable = v, legacy_rows = nrow(old), latest_rows = nrow(new),
        common_rows = sum(keep), legacy_only = sum(!keep),
        latest_only = sum(!new[[id]] %in% old[[id]]),
        changed = sum(!same), missingness_changed = sum(is.na(x) != is.na(y)))
    }
  }
  write.csv(do.call(rbind, changed_fields), file.path(audit, 'latest_analysis_data_changes.csv'),
            row.names = FALSE)

  controls <- c('chsex', 'zbfa', 'stunting', 'caredu_r1', 'careage_r1', 'caresex_r1',
                'hhsize', 'wi_new', 'hq_new', 'cd_new', 'elecq_new', 'ownlandhse_r1', 'foodsec_r3')
  old <- read.csv(file.path(root, 'data/non_aa_samp.csv'))
  new <- read.csv('data/non_aa_samp.csv')
  missingness <- do.call(rbind, lapply(c('IMTI', 'E_is', controls), function(v)
    data.frame(variable = v, legacy_missing = sum(is.na(old[[v]])),
               latest_missing = sum(is.na(new[[v]])),
               legacy_rows = nrow(old), latest_rows = nrow(new))))
  write.csv(missingness, file.path(audit, 'latest_main_sample_missingness.csv'), row.names = FALSE)
  sample_size <- function(d, outcome, excluding_Amhara) {
    d <- d[!is.na(d$raw_lang) & d$raw_lang != 0, ]
    if (excluding_Amhara) d <- d[!is.na(d$region) & d$region != 'Amhara', ]
    sum(complete.cases(d[, c(outcome, 'IMTI', 'E_is', controls)]))
  }
  samples <- do.call(rbind, lapply(1:4, function(i) {
    outcome <- c('wage_employII', 'wage_employ')[(i - 1) %% 2 + 1]
    data.frame(column = i, outcome = outcome, excluding_Amhara = i > 2,
               legacy_n = sample_size(old, outcome, i > 2),
               latest_n = sample_size(new, outcome, i > 2))
  }))
  write.csv(samples, file.path(audit, 'latest_table2_complete_cases.csv'), row.names = FALSE)
  if (any(samples$latest_n == 0)) {
    writeLines(c('Latest-release-only regressions cannot reproduce the current specification.',
                 'At least one Table 2 specification has zero complete observations.',
                 'No covariates were dropped or filled from legacy data.',
                 paste('Isolated candidate directory:', sandbox)),
               file.path(audit, 'latest_sensitivity_status.txt'))
    message('Comparison completed: latest inputs have no complete Table 2 cases; legacy default retained.')
  } else {
    stop('Candidate has estimable samples. Review mapping definitions before extending the model comparison.')
  }
})
