# Full-variable codebooks for all Stata sources used by current construction.
# Run from the project root; no survey records or raw strings are exported.
library(tidyverse)
library(haven)
source("code/helpers/raw_data_catalog.R")
out <- "temp/codebooks/raw_sources"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
sources <- enframe(raw_data_files, name = "source", value = "path") |>
  filter(source != "round7_ethiopia") |>
  mutate(role = case_when(
    source == "school_schools_w1" ~ "Catalogued context; not read by cleaning",
    str_starts(source, "school_") ~ "YC school-survey learning extension",
    TRUE ~ "Current outcome/covariate construction"
  ))
for (co in c("oc", "yc")) {
  for (module in c("ch_arch", "hh_arch", "ch_phone_arch")) {
    sources <- bind_rows(sources, tibble(
      source = paste("r7", co, module, sep = "_"),
      path = file.path(raw_data_files[["round7_ethiopia"]],
        paste0("et_r7_", co, module, ".dta")),
      role = if (module == "ch_phone_arch") {
        "Phone-ID exclusion validation only; no phone records retained"
      } else "R7 outcomes, household controls and reading responses"
    ))
  }
  sources <- bind_rows(sources, tibble(
    source = paste0("r3_", co, "_household"),
    path = file.path("rawdata/yl_r3/UKDA-6853-stata/stata/stata13",
      paste0("r3_", co), "ethiopia", paste0("et_", co,
        "_householdlevel.dta")),
    role = "Raw electricity reconstruction audit; not production replacement"
  ))
}
stopifnot(all(file.exists(sources$path)), !anyDuplicated(sources$path))
sources <- sources |>
  mutate(rows = NA_integer_, variables = NA_integer_,
         all_missing_variables = NA_integer_)
metadata <- list()
for (i in seq_len(nrow(sources))) {
  source <- sources$source[i]
  message("Codebook ", i, "/", nrow(sources), ": ", source)
  d <- read_dta(sources$path[i])
  dictionary <- map_dfr(names(d), function(variable) {
    x <- d[[variable]]
    labels <- attr(x, "labels")
    tibble(
      source = source, position = match(variable, names(d)),
      variable = variable,
      variable_label = paste(as.character(attr(x, "label") %||% ""),
                             collapse = "; "),
      storage_class = paste(class(x), collapse = "/"),
      stata_format = attr(x, "format.stata") %||% "",
      rows = nrow(d), observed = sum(!is.na(x)),
      missing = sum(is.na(x)),
      value_labels = paste(paste0(unname(labels), " = ", names(labels)),
                           collapse = "; ")
    )
  })
  metadata[[source]] <- dictionary
  write_csv(dictionary, file.path(out, paste0(source, "_variables.csv")))
  entirely_missing <- vapply(d, function(x) all(is.na(x)), logical(1))
  display <- d
  for (variable in names(display)) {
    label <- attr(display[[variable]], "label")
    if (!is.null(label)) {
      attr(display[[variable]], "label") <-
        paste(as.character(label), collapse = "; ")
    }
  }
  book <- sjPlot::view_df(
    display[, !entirely_missing, drop = FALSE], show.type = TRUE,
    show.string.values = FALSE, show.na = TRUE,
    max.len = max(15L, lengths(lapply(d, attr, "labels"))),
    use.viewer = FALSE, verbose = FALSE
  )
  # sjPlot omits entirely missing fields, so document them separately.
  missing_fields <- dictionary |> filter(observed == 0)
  supplement <- if (nrow(missing_fields)) paste0(
    "<h2>Entirely missing variables (retained in this codebook)</h2>",
    "<table><tr><th>Variable</th><th>Label</th><th>Class</th>",
    "<th>Defined value labels</th><th>Missing</th></tr>",
    paste0("<tr><td>", escape(missing_fields$variable), "</td><td>",
      escape(missing_fields$variable_label), "</td><td>",
      escape(missing_fields$storage_class), "</td><td>",
      escape(missing_fields$value_labels), "</td><td>",
      missing_fields$missing, "</td></tr>", collapse = ""), "</table>"
  ) else ""
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'>", book$page.style,
    "<style>body{margin:30px;font:15px system-ui}table{max-width:100%}",
    "td{overflow-wrap:anywhere}p{max-width:1000px}</style></head><body>",
    "<a href='index.html'>All source files</a><h1>", source, "</h1><p>",
    escape(sources$path[i]), "</p><p>", escape(sources$role[i]),
    ". Records: ", nrow(d), "; variables: ", ncol(d), ".</p>",
    "<p>Complete source file, with no filtering or recoding. Numeric survey",
    " special codes remain numeric codes; missing counts use Stata missing",
    " values imported by haven. String response values are suppressed.</p>",
    "<p><a href='", source, "_variables.csv'>Complete variable metadata</a>",
    " (including all defined value labels and entirely missing fields).</p>",
    book$page.content, supplement, "</body></html>"
  )
  writeLines(html, file.path(out, paste0(source, ".html")))
  sources$rows[i] <- nrow(d)
  sources$variables[i] <- ncol(d)
  sources$all_missing_variables[i] <- sum(entirely_missing)
}
sources$md5 <- unname(tools::md5sum(sources$path))
write_csv(sources, file.path(out, "source_manifest.csv"))
write_csv(bind_rows(metadata), file.path(out, "all_source_variables.csv"))
links <- paste0("<tr><td><a href='", sources$source, ".html'>",
  sources$source, "</a></td><td>", escape(sources$role), "</td><td>",
  sources$rows, "</td><td>", sources$variables, "</td></tr>")
writeLines(paste0(
  "<!doctype html><html><head><meta charset='utf-8'><title>Source ",
  "codebooks</title>",
  "<style>body{font:16px system-ui;margin:40px;max-width:1300px}",
  "td,th{padding:10px;border-bottom:1px solid #ddd;text-align:left}</style>",
  "</head><body><h1>Young Lives: complete source-file codebooks</h1>",
  "<p>All Stata inputs to R1-R5/R7 construction and the YC school-survey",
  " extension, plus clearly marked validation/context sources. Both legacy",
  " and updated constructed releases are included as direct source inputs.",
  " Each codebook covers every variable in the original file, not just",
  " those selected for estimation. Records may repeat IDs across rounds",
  " or roster members. Metadata only; no participant records exported.</p>",
  "<p>Scope follows code/01_extract_clean.R, 02_cleaning_yc.R and",
  " 10_build_r7_cross_section.R. Underlying files used by the data provider",
  " to produce released constructed variables are not recursively treated",
  " as local pipeline inputs. R6 is outside the current construction.</p>",
  "<p><a href='all_source_variables.csv'>All variable metadata</a> | ",
  "<a href='source_manifest.csv'>Source manifest and checksums</a></p>",
  "<table><tr><th>Codebook</th><th>Use</th><th>Rows</th><th>Variables</th>",
  "</tr>", paste(links, collapse = ""), "</table></body></html>"
), file.path(out, "index.html"))
writeLines(capture.output(sessionInfo()), file.path(out, "session.txt"))
message("Created ", nrow(sources), " full-file codebooks; ",
        sum(sources$variables), " variable entries.")
