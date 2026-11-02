#!/usr/bin/env Rscript
# Run the validation for one dataset and write its report.
#
# Usage:
#   Rscript scripts/run_dataset.R <config-file.R> [<config-fn> ...]
#
# Examples:
#   Rscript scripts/run_dataset.R configs/miv_stadt.R
#   Rscript scripts/run_dataset.R configs/miv_stadt.R miv_stadt_config
#   Rscript scripts/run_dataset.R configs/miv_stadt_derive.R miv_stadt_derive_config
#
# With no config function named, every `*_config` function the file defines is
# run in turn. A dataset therefore needs a config, not a script.
#
# A config may declare `sources`: files sourced before it runs, which is how the
# pipeline under test and its shims are loaded. They are sourced after the
# config function is called, not before, because `transform` is a closure and is
# only invoked once validation starts.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  stop("usage: run_dataset.R <config-file.R> [<config-fn> ...]")
}
config_file <- args[[1L]]
if (!file.exists(config_file)) {
  stop("config file not found: ", config_file)
}

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  source(f)
}

# Sourcing into an environment lets us discover the config functions a file
# defines without assuming a naming convention beyond the `_config` suffix.
config_env <- new.env(parent = globalenv())
source(config_file, local = config_env)

config_fns <- if (length(args) > 1L) {
  args[-1L]
} else {
  sort(grep("_config$", ls(config_env), value = TRUE))
}
if (length(config_fns) == 0L) {
  stop("no `*_config` function found in ", config_file, "; name one explicitly")
}

missing <- config_fns[!vapply(
  config_fns,
  function(nm) is.function(mget(nm, envir = config_env, ifnotfound = list(NULL))[[1L]]),
  logical(1)
)]
if (length(missing) > 0L) {
  stop(
    "not a function in ", config_file, ": ", paste(missing, collapse = ", ")
  )
}

dir.create("reports", showWarnings = FALSE)

for (fn in config_fns) {
  spec <- get(fn, envir = config_env)()

  for (dep in spec[["sources"]] %||% character(0)) {
    if (!file.exists(dep)) {
      stop("config ", fn, " declares a missing source: ", dep)
    }
    source(dep)
  }

  path <- file.path("reports", paste0(spec[["dataset"]], ".txt"))
  con <- file(path, open = "wt", encoding = "UTF-8")
  sink(con, split = TRUE)
  tryCatch(
    validate_dataset(spec),
    finally = {
      sink()
      close(con)
    }
  )
  cat("\nreport written to ", path, "\n", sep = "")
}
