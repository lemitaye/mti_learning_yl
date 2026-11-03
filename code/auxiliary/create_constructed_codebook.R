# Run from the project root:
# Rscript code/auxiliary/create_constructed_codebook.R [input.dta] [output.html]
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

# Explicit crosswalk to TN63 (December 2025), sections 3-6, Tables 1-24.
# Only variables present in the Ethiopian file are listed. Source names and
# source column IDs are retained; assignments based on labels are noted below.
section <- function(chapter, title, reference, variables, note = "") {
  list(chapter = chapter, title = title, reference = reference,
       variables = strsplit(variables, "[[:space:]]+")[[1L]], note = note)
}
sections <- list(
  identifiers = section("3. Unique identifiers and panel structure",
    "Unique identifiers and panel structure", "pp. 8-9; Table 1",
    "childid yc round call inround incall row panel status in_phone_r7 dint",
    "The dataset's additional variable row is grouped here by its label, Round; it is not listed in Table 1."),
  location = section("4. Location variables", "Location variables",
    "pp. 9-10; Table 2 and Household region of residence",
    "clustid commid typesite region"),
  background = section("5. Participant's characteristics", "Background characteristics",
    "p. 11; Table 3", "chsex chlang chethnic chldrel agemon ageyea",
    "The guide also describes chsex_correct, which is absent from this Ethiopian file."),
  family = section("5. Participant's characteristics", "Family formation and fertility",
    "pp. 11-12; Table 4", "marital_status marrcohab marrcohab_age birth birth_age pregnant"),
  anthropometrics = section("5. Participant's characteristics", "Anthropometrics information",
    "pp. 13-14; Table 5",
    paste("chweight chheight bmi zwfa zhfa zbfa zwfl fwfa fhfa fbfa fwfl",
      "underweight_child stunting_child thinness_child bmi_cat overweight_adult underweight_adult obese_adult"),
    "bmi_cat is grouped here by its dataset label, BMI - Categories; it is not listed in Table 5. The file uses thinness_child where the guide prints Thinnes_child."),
  birth = section("5. Participant's characteristics", "Birth and immunisations",
    "pp. 14-15; Table 6", "bwght bwdoc numante tetanus delivery bcg measles polio dpt hib",
    "This subsection appears in the guide's body but is omitted from its contents page."),
  physical_health = section("5. Participant's characteristics", "Physical health and disabilities",
    "pp. 15-17; Tables 7-8",
    "chillness chinjury chhprob chmightdie chdisability chdisscale chhrel chhealth"),
  mental_health = section("5. Participant's characteristics", "Mental health and subjective well-being",
    "pp. 17-18; Table 9", "cladder gad7_score gad7_cat phq8_score phq8_cat phq2_score trust"),
  health_behaviours = section("5. Participant's characteristics",
    "Smoking and drinking habits and reproductive health knowledge", "pp. 18-19; Table 10",
    "chsmoke chalcohol chrephealth1 chrephealth2 chrephealth3 chrephealth4"),
  time_use = section("5. Participant's characteristics", "Time use",
    "pp. 20-21; Table 11", "hsleep hcare hchore htask hwork hschool hstudy hplay commwork commsch"),
  education = section("5. Participant's characteristics", "Education and learning outcomes",
    "pp. 21-22; Table 12", "preprim agegr1 enrol engrade entype hghschoolgrade timesch"),
  learning = section("5. Participant's characteristics", "Learning outcomes",
    "pp. 22-24; Table 13", "levlread levlwrit literate"),
  labour = section("5. Participant's characteristics", "Labour market outcomes",
    "pp. 24-26; Table 14",
    "work_12 work_week no_work_job econ_sector type_act who_work agri typemp written_contract",
    "The file's work_12 (label: Worked last year) corresponds to the guide's Work_lastyear; its name has not been changed."),
  parents = section("6. Household and parental characteristics", "Parent characteristics",
    "p. 27; Table 15",
    "dadid dadage dadlive dadyrdied dadcantread dadedu momid momage momlive momyrdied momcantread momedu"),
  caregivers = section("6. Household and parental characteristics", "Caregivers' characteristics",
    "pp. 27-28; Table 16",
    "careid careage caresex carehead carerel carecantread caredu careladder careldr4yrs"),
  household_head = section("6. Household and parental characteristics", "Household head characteristics",
    "p. 28; Table 17", "headid headage headsex headedu headrel"),
  composition = section("6. Household and parental characteristics", "Household size and composition",
    "pp. 29-30; Table 18",
    paste("childloc male05 male612 male1317 male1860 male61",
      "female05 female612 female1317 female1860 female61 hhm05 hhm612 hhm1317 hhm1860 hhm61 hhsize"),
    "childloc is grouped here by its label, Child currently lives in the household; it is not listed in Table 18."),
  assets = section("6. Household and parental characteristics", "Livestock, land and house ownership variables",
    "pp. 30-31; Table 19",
    paste("aniany animilk anidrau anirumi anispec anicowm anicowt anicalv anibufm anibuft aniheif",
      "anibull anihebu anidonk aniybul anioxen anicaml anishee anigoat anipigs anipoul anirabb anibeeh aniothr ownlandhse ownhouse")),
  food_security = section("6. Household and parental characteristics", "Credit and food security",
    "pp. 31-34; Tables 20-21", "credit foodsec hfias_score hfias_cat hfias_score_6 hfias_cat_6"),
  programmes = section("6. Household and parental characteristics", "Public programmes",
    "pp. 34-36; Table 22 (Ethiopia)", "psnp_pw psnp_ds othprog hep resettled eap"),
  shocks = section("6. Household and parental characteristics", "Household shocks",
    "pp. 36-39; Table 23",
    paste("shcrime1 shcrime2 shcrime3 shcrime4 shcrime5 shcrime6 shcrime7 shcrime8",
      "shregul1 shregul2 shregul3 shregul4 shregul5",
      "shecon1 shecon2 shecon3 shecon4 shecon5 shecon6 shecon7 shecon8 shecon9 shecon10 shecon11 shecon12 shecon13 shecon14",
      "shenv1 shenv2 shenv3 shenv4 shenv5 shenv6 shenv7 shenv8 shenv9 shhouse1 shhouse2 shhouse3",
      "shfam1 shfam2 shfam3 shfam4 shfam5 shfam6 shfam7 shfam8 shfam9 shfam10 shfam12 shfam13 shfam14 shfam18 shother")),
  wealth = section("6. Household and parental characteristics", "Wealth index",
    "pp. 39-40; Table 24 and accompanying text",
    "wi wi_new hq hq_new sv cd elecq toiletq drwaterq cookingq")
)
assigned <- unlist(lapply(sections, `[[`, "variables"), use.names = FALSE)
if (anyDuplicated(assigned) || !setequal(assigned, names(ethiopia_constructed_r1r7))) {
  stop("Section mapping must cover every variable exactly once. Unmapped: ",
    paste(setdiff(names(ethiopia_constructed_r1r7), assigned), collapse = ", "),
    "; absent: ", paste(setdiff(assigned, names(ethiopia_constructed_r1r7)), collapse = ", "))
}

codebook <- sjPlot::view_df(
  ethiopia_constructed_r1r7,
  show.id = TRUE, show.type = TRUE, show.values = TRUE, show.labels = TRUE,
  show.na = TRUE, show.string.values = FALSE,
  max.len = max_labels, sort.by.name = FALSE, encoding = "UTF-8"
)
# Split the sjPlot table into sections without changing its cells or source IDs.
rows <- regmatches(codebook$page.content,
  gregexpr("(?s)<tr>.*?</tr>", codebook$page.content, perl = TRUE))[[1L]]
stopifnot(length(rows) == ncol(ethiopia_constructed_r1r7) + 1L)
variable_rows <- setNames(rows[-1L], names(ethiopia_constructed_r1r7))
escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
toc <- character()
body <- character()
previous_chapter <- ""
for (key in names(sections)) {
  s <- sections[[key]]
  if (s$chapter != previous_chapter) {
    toc <- c(toc, sprintf("<li class='chapter'>%s</li>", escape_html(s$chapter)))
    body <- c(body, sprintf("<h2>%s</h2>", escape_html(s$chapter)))
    previous_chapter <- s$chapter
  }
  # Preserve original column order within each thematic section.
  variables <- names(ethiopia_constructed_r1r7)[names(ethiopia_constructed_r1r7) %in% s$variables]
  toc <- c(toc, sprintf("<li><a href='#%s'>%s</a> <span>(%d)</span></li>",
    key, escape_html(s$title), length(variables)))
  body <- c(body, sprintf("<section id='%s'><h3>%s</h3>", key, escape_html(s$title)),
    sprintf("<p class='reference'>Guide: %s. %d variables.</p>", s$reference, length(variables)))
  if (nzchar(s$note)) body <- c(body, paste0("<p class='note'>", escape_html(s$note), "</p>"))
  body <- c(body, "<div class='table-scroll'><table>",
    sprintf("<caption>%s</caption>", escape_html(s$title)),
    paste0("<thead>", rows[1L], "</thead><tbody>"),
    variable_rows[variables], "</tbody></table></div>",
    "<p class='back'><a href='#contents'>Back to contents</a></p></section>")
}
# Write HTML directly so batch runs do not launch a browser.
intro <- sprintf(
  paste0(
    "<h1>Young Lives Ethiopia: constructed data, Rounds 1–7</h1>\n",
    "<p>Source: ethiopia_constructed_r1r7.dta. %s records; %s variables.</p>\n",
    "<p>Generated with sjPlot %s and haven %s. Variables are grouped using ",
    "Young Lives Technical Note 63, <cite>A Guide to Young Lives Constructed Datasets: ",
    "Rounds 1 to 7</cite> (December 2025), sections 3-6. Guide page and table ",
    "references appear under each heading. Assignments inferred from dataset labels ",
    "and differences in variable names are noted in the relevant sections.</p>\n",
    "<p>IDs are original source column positions; source order is retained within ",
    "each section. Source variable labels, value labels and all defined value codes ",
    "are retained. Only variables present in the Ethiopian file are included.</p>\n",
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
style <- paste0(
  "<style>body{font-family:system-ui,sans-serif;color:#203044;max-width:1400px;",
  "margin:32px auto;padding:0 24px;line-height:1.5}h1{line-height:1.2}h2{border-bottom:2px solid #b8cbdc;",
  "padding-top:24px}h3{font-size:1.25rem;margin-bottom:4px}a{color:#175987}",
  "nav{background:#f2f6fa;padding:20px 28px;border-radius:8px}nav ul{list-style:none;padding:0;",
  "columns:2;column-gap:40px}nav li{break-inside:avoid;margin:4px 0}",
  "nav .chapter{font-weight:700;margin-top:14px}nav span,.reference,.back{color:#526479;font-size:.9rem}",
  ".note{border-left:3px solid #9bb7cc;padding:8px 12px;background:#f7f9fb}",
  ".table-scroll{overflow-x:auto}table{width:100%;font-size:.9rem}caption{text-align:left;",
  "padding:8px 0}th{background:#e6eef5}td,th{border-bottom:1px solid #dfe6ed}",
  "tbody tr:nth-child(even) td{background:#f5f7fa}td.arc{background:transparent}",
  "section{scroll-margin-top:20px}@media(max-width:700px){nav ul{columns:1}body{padding:0 12px}}",
  "@media print{nav,.back{display:none}h2,h3{break-after:avoid}tr{break-inside:avoid}}</style>"
)
html <- paste(c(
  "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>Ethiopia Rounds 1-7 constructed data codebook</title>",
  codebook$page.style, style, "</head><body>", intro,
  "<nav id='contents' aria-label='Variable sections'><h2>Contents</h2><ul>", toc,
  "</ul></nav><main>", body, "</main></body></html>"
), collapse = "\n")
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
writeLines(enc2utf8(html), output_file, useBytes = TRUE)
writeLines(c(
  paste("Source:", normalizePath(source_file)),
  paste("Source MD5:", unname(tools::md5sum(source_file))),
  paste("Records:", nrow(ethiopia_constructed_r1r7)),
  paste("Variables:", ncol(ethiopia_constructed_r1r7)),
  paste("Thematic sections:", length(sections)),
  "Grouping source: Young Lives TN63 (December 2025), sections 3-6, Tables 1-24.",
  "", capture.output(sessionInfo())
), sub("\\.html$", "_session.txt", output_file))
message("Created: ", normalizePath(output_file))
