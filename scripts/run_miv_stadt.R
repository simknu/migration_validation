#!/usr/bin/env Rscript
# Run the validation for one dataset and write the report.
#
# Usage: Rscript scripts/run_miv_stadt.R [migrate|derive|both]
#
#   migrate  certifies migrate_published_to_long()  (the step under test)
#   derive   additionally recomputes arbeitstag/kennzahl/ort_richtung_id
#   both     runs each in turn (default)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
source("pipeline/house_shims.R")
source("pipeline/miv_stadt_fct.R")
source("configs/bezeichnung_lookup.R")
source("configs/miv_stadt.R")
source("configs/miv_stadt_derive.R")

args <- commandArgs(trailingOnly = TRUE)
which_run <- if (length(args) == 0L) "both" else args[[1L]]

specs <- switch(
  which_run,
  migrate = list(miv_stadt_config()),
  derive = list(miv_stadt_derive_config()),
  both = list(miv_stadt_config(), miv_stadt_derive_config()),
  stop("argument must be one of: migrate, derive, both")
)

dir.create("reports", showWarnings = FALSE)

for (spec in specs) {
  path <- file.path("reports", paste0(spec$dataset, ".txt"))
  con <- file(path, open = "wt", encoding = "UTF-8")
  sink(con, split = TRUE)
  result <- tryCatch(
    validate_dataset(spec),
    finally = {
      sink()
      close(con)
    }
  )
  cat("\nreport written to ", path, "\n", sep = "")
}
