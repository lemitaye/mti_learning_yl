# Shared code labels for the Ethiopia Rounds 1--5 extraction scripts.
# Unknown codes remain missing; factor() retains alphabetical observed levels.
survey_code_labels <- list(
  region = c(`1` = "Tigray", `2` = "Afar", `3` = "Amhara", `4` = "Oromia",
             `5` = "Somali", `7` = "SNNP", `14` = "Addis Ababa"),
  language_r4 = c(`1` = "English", `2` = "Amharic", `3` = "Hadiya",
                  `4` = "Afaan Oromo", `5` = "Wolayta", `6` = "Sidamigna",
                  `7` = "Tigrigna"),
  language_r3 = c(`2` = "Amharic", `7` = "Hadiya", `11` = "Afaan Oromo",
                  `12` = "Sidamigna", `15` = "Tigrigna", `16` = "Wolayta"),
  home_language = c(`1` = "Afar", `2` = "Amharic", `6` = "Gurage", `7` = "Hadiya",
                    `11` = "Afaan Oromo", `12` = "Sidamigna", `13` = "Siltigna",
                    `15` = "Tigrigna", `16` = "Wolayta", `20` = "Other"),
  ethnicity = c(`10` = "Other", `12` = "Amhara", `13` = "Gurage", `14` = "Hadiya",
                `16` = "Oromo", `17` = "Sidama", `18` = "Tigrian", `19` = "Wolayta"),
  sex = c(`1` = "Male", `2` = "Female")
)

label_survey_codes <- function(x, labels) {
  unname(labels[match(as.character(as.numeric(x)), names(labels))])
}

# Preserve missing values when capping schooling-based MTI exposure.
top_code <- function(x, ceiling) if_else(x > ceiling, ceiling, x)
