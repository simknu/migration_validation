#!/usr/bin/env Rscript
# Container-only bridge: typed TSV + manifest -> frozen .RDS snapshot.
#
# On the user's machine the harness reads Parquet directly with `arrow`
# (see read_parquet_source() in R/fetch.R). This container cannot build
# `arrow`, so scripts/parquet_to_tsv.py renders each Parquet file to a typed
# TSV and checksums every column. This script re-reads those strings, verifies
# each column's SHA-256 against the manifest, casts to the declared R type and
# freezes the result to .RDS. A bridge that altered a single value would fail
# the checksum here, so no finding in the report can be an artefact of it.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("usage: tsv_to_rds.R <in.tsv> <in.manifest.tsv> <out.rds>")
}
tsv_path <- args[[1L]]
manifest_path <- args[[2L]]
rds_path <- args[[3L]]

na_string <- "\\N"

manifest <- readr::read_tsv(
  manifest_path,
  col_types = readr::cols(.default = readr::col_character())
)

raw_tbl <- readr::read_tsv(
  tsv_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = character(0),          # keep the sentinel literal; cast step handles NA
  quote = "",                 # fields are never quoted by the writer
  progress = FALSE
)

if (!identical(colnames(raw_tbl), manifest$name)) {
  stop(
    "manifest columns do not match the TSV header:\n  tsv:      ",
    paste(colnames(raw_tbl), collapse = ", "),
    "\n  manifest: ", paste(manifest$name, collapse = ", ")
  )
}

expected_rows <- as.integer(manifest$n_rows[[1L]])
if (nrow(raw_tbl) != expected_rows) {
  stop(
    "row count mismatch: TSV has ", nrow(raw_tbl),
    " rows, manifest declares ", expected_rows
  )
}

# Verify the bridge before trusting anything downstream.
for (i in seq_len(nrow(manifest))) {
  nm <- manifest$name[[i]]
  got <- digest::digest(
    paste(raw_tbl[[nm]], collapse = "\x1f"),
    algo = "sha256",
    serialize = FALSE
  )
  if (!identical(got, manifest$sha256[[i]])) {
    stop("checksum mismatch for column ", nm, ": TSV content is not faithful")
  }
}

cast_column <- function(x, kind) {
  x[x == na_string] <- NA_character_
  switch(
    kind,
    integer = as.integer(x),
    double = as.numeric(x),
    logical = x == "TRUE",
    Date = as.Date(x, format = "%Y-%m-%d"),
    POSIXct = as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S"),
    character = x,
    stop("unknown column kind: ", kind)
  )
}

out <- tibble::as_tibble(
  stats::setNames(
    lapply(
      seq_len(nrow(manifest)),
      function(i) cast_column(raw_tbl[[manifest$name[[i]]]], manifest$kind[[i]])
    ),
    manifest$name
  )
)

attr(out, "miv_source") <- tsv_path
attr(out, "miv_frozen_at") <- format(Sys.time(), tz = "Europe/Zurich")

saveRDS(out, rds_path)

cat(sprintf(
  "%s -> %s (%d rows, %d cols, %d column checksums verified)\n",
  basename(tsv_path), basename(rds_path), nrow(out), ncol(out), nrow(manifest)
))
