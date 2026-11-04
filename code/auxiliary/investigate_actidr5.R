# ACTIDR5: activity roster slot, not occupation or payment category.
# Documentation: R5 questionnaire PDF p36; dictionary PDF pp89 and48.
library(haven)
library(dplyr)
p <- paste0("rawdata/yl_r5/UKDA-8357-stata/stata/stata13/",
            "ethiopia_r5/etoc_ch_anon/")
a <- read_dta(paste0(p, "et_r5_occh_activity.dta"))
b <- read_dta(paste0(p, "et_r5_occh_olderchild.dta"))
out <- "temp/actidr5_investigation"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(!anyDuplicated(paste(a$CHILDCODE, a$ACTIDR5)))
write.csv(count(a, ACTIDR5), file.path(out, "slot_counts.csv"),
          row.names = FALSE)
x <- a |>
  filter(ACTIDR5 == 1) |>
  select(CHILDCODE, ACTR5) |>
  left_join(select(b, CHILDCODE, ACTMSTR5), by = "CHILDCODE")
comparison <- x |>
  summarise(
    paired = sum(!is.na(ACTR5) & !is.na(ACTMSTR5)),
    different = sum(ACTR5 != ACTMSTR5, na.rm = TRUE)
  )
write.csv(comparison, file.path(out, "first_slot_vs_selected_main.csv"),
          row.names = FALSE)
print(count(a, ACTIDR5))
print(comparison)
