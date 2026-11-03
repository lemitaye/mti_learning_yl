# Run from the project root:
# Rscript code/01_create_constructed_codebook.R [input.dta] [output.html]
# Requires haven and sjPlot; the source dataset is read without recoding.

args <- commandArgs(trailingOnly = TRUE)
relative_source <- file.path(
  "rawdata", "yl_const_r1_r7", "UKDA-9543-stata", "stata", "stata13",
  "ethiopia_constructed_r1r7.dta"
)
source_file <- if (length(args) >= 1L) args[1L] else relative_source
if (!file.exists(source_file) && length(args) == 0L) {
  source_file <- file.path(
    path.expand("~/Library/CloudStorage/GoogleDrive-tayelemi@gmail.com/My Drive/Research/mti_learning_yl"),
    relative_source
  )
}
output_file <- if (length(args) >= 2L) args[2L] else file.path(
  "output", "codebooks", "ethiopia_constructed_r1r7_codebook.html"
)
for (package in c("haven", "sjPlot")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is not installed: ", package)
  }
}
if (!file.exists(source_file)) stop("Dataset not found: ", source_file)

ethiopia_constructed_r1r7 <- haven::read_dta(source_file)
stopifnot(nrow(ethiopia_constructed_r1r7) > 0L)
# view_df omits entirely missing variables; guard against an incomplete codebook.
all_missing <- vapply(ethiopia_constructed_r1r7, function(x) all(is.na(x)), logical(1))
if (any(all_missing)) {
  stop("Entirely missing variables require separate documentation: ",
       paste(names(ethiopia_constructed_r1r7)[all_missing], collapse = ", "))
}
max_labels <- max(15L, vapply(
  ethiopia_constructed_r1r7, function(x) length(attr(x, "labels")), integer(1)
))

codebook <- sjPlot::view_df(
  ethiopia_constructed_r1r7,
  show.id = TRUE, show.type = TRUE, show.values = TRUE, show.labels = TRUE,
  show.na = TRUE, show.string.values = FALSE,
  max.len = max_labels, sort.by.name = FALSE, encoding = "UTF-8"
)
# Write the returned HTML directly so batch runs do not launch a browser.
intro <- sprintf(
  paste0(
    "<h1>Young Lives Ethiopia: constructed data, Rounds 1–7</h1>\n",
    "<p>Source: ethiopia_constructed_r1r7.dta. %s records; %s variables.</p>\n",
    "<p>Generated with sjPlot %s and haven %s. Variables retain source order, ",
    "variable labels and value labels. All defined value labels are displayed.</p>\n",
    "<p>No filters, weighting or recoding were applied. Negative survey codes ",
    "remain as supplied and are not counted as NA in the missings column. ",
    "String values are omitted. Numeric ranges use sjPlot's display precision. ",
    "Records include repeated rounds and phone calls, not unique children.</p>\n"
  ),
  format(nrow(ethiopia_constructed_r1r7), big.mark = ","),
  ncol(ethiopia_constructed_r1r7),
  as.character(utils::packageVersion("sjPlot")),
  as.character(utils::packageVersion("haven"))
)
html <- sub("<body>", paste0("<body>\n", intro), codebook$page.complete, fixed = TRUE)
html <- sub("<head>", paste0(
  "<head>\n<title>Ethiopia Rounds 1–7 constructed data codebook</title>"
), html, fixed = TRUE)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
writeLines(enc2utf8(html), output_file, useBytes = TRUE)
writeLines(c(
  paste("Source:", normalizePath(source_file)),
  paste("Source MD5:", unname(tools::md5sum(source_file))),
  paste("Records:", nrow(ethiopia_constructed_r1r7)),
  paste("Variables:", ncol(ethiopia_constructed_r1r7)),
  "", capture.output(sessionInfo())
), sub("\\.html$", "_session.txt", output_file))
message("Created: ", normalizePath(output_file))
