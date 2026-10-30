# Dataset config: MIV traffic counts, Stadt St.Gallen -- RAW .TXT.
#
# This is the real certification case. `data/MIV_2024_open_data.txt` is the raw
# export that produced `miv_transformed.parquet`, so unlike the other two
# configs -- which reconcile the OLD published wide dataset against the
# known-final -- this one compares the pipeline's output against the ground
# truth derived from the same source, and can actually reach PASS.
#
# Nothing is stubbed: the vendored house files supply the real `load_txt_utf8()`
# and `load_csv_utf8()`, so the FILE READING AND ENCODING DETECTION are covered.
# That matters more than it looks: the encoding damage in the old published
# dataset is exactly what the harness is being asked to adjudicate, and a
# stand-in reader would have left the interesting part untested.
#
# `standort` needs a geometry-aware normalizer, because the two sides disagree
# on representation rather than position: the portal export carries WKB binary
# while the join CSV carries "lat, lon" text. `type = "geopoint"` decodes both
# to one canonical "lon,lat" rendering. Calibrated against this data, every
# precision from 6 to 10 decimals agrees exactly, so 8 sits mid-margin.

miv_stadt_raw_config <- function() {
  raw_file <- "data/MIV_2024_open_data.txt"
  join_file <- "data/detailplane-und-geokoordinaten-miv-zahlstellen-stadt-stgallen.csv"

  list(
    dataset = "miv_stadt_raw",
    cache_dir = "snapshots",

    sources = c(
      "pipeline/00_input_check_fct.R",
      "pipeline/import_export_utf8_fct.R",
      "pipeline/00_helper_fct.R",
      "pipeline/miv_stadt_fct.R"
    ),

    # The pipeline owns the parsing, so the config hands the harness a reader
    # rather than a format. This resolves the original for the report's row
    # counts and schema diff; the transform below re-reads from the path
    # because `process_miv()` takes a path, and running it from the path is
    # what covers the real entry point rather than a hand-assembled chain.
    original = list(
      kind = "reader",
      path = raw_file,
      label = "miv_2024_raw",
      reader = function(path) load_miv_file(path, sep = "auto")
    ),

    known_final = list(
      kind = "parquet",
      path = "data/miv_transformed.parquet",
      label = "miv_transformed"
    ),

    # `process_miv()` is the pipeline's own entry point: shape detection, read,
    # transform, join, pivot, sort and its duplicate assertion. Only the
    # tracking columns are added here, exactly as `publish_miv()` does, since
    # `process_miv()` deliberately produces none.
    transform = function(original) {
      process_miv(raw_file, join_file = join_file, sep = "auto") |>
        dplyr::mutate(
          erstpublikation = miv_run_date(),
          letzte_aktualisierung = miv_run_date()
        ) |>
        dplyr::select(dplyr::all_of(miv_schema()$published))
    },

    # `lnr` is kept OUT of the grain and compared as an attribute instead. It
    # comes straight from this raw file and matches exactly, so it is certified
    # either way -- but as an attribute a divergence would be reported as a
    # value difference rather than hidden inside a roster gap.
    grain_key = c("ort_id", "datum", "richtung", "stunde"),

    sides = list(known_final = list(), new_output = list()),

    normalize = list(
      standort = list(type = "geopoint", digits = 8L, order = "latlon"),
      lnr = list(type = "number"),
      richtung = list(type = "number"),
      stunde = list(type = "number"),
      anzahl_fahrzeuge = list(type = "number"),
      datum = list(type = "date"),
      erstpublikation = list(type = "date"),
      letzte_aktualisierung = list(type = "date")
    ),
    na_string = TRUE,

    lineage = tibble::tribble(
      ~column,                  ~class,
      "lnr",                    "unchanged",
      "bezeichnung",            "unchanged",
      "standort",               "unchanged",
      "wochentag",              "unchanged",
      "arbeitstag",             "unchanged",
      "kennzahl",               "unchanged",
      "ort_richtung_id",        "unchanged",
      "anzahl_fahrzeuge",       "unchanged",
      "erstpublikation",        "new",
      "letzte_aktualisierung",  "new"
    ),

    examples_cap = 10L,

    limits = c(
      "Exact multiset equivalence for `unchanged` columns is a true guarantee.",
      "Row order is never checked: bag semantics by design, as accepted.",
      "Values normalizing to empty are dropped before comparison, both sides alike.",
      paste(
        "`standort` is compared across representations: WKB on the known-final",
        "side against \"lat, lon\" text from the join CSV, both decoded and",
        "matched to 8 decimals (~1 mm). It is a positional match, not a",
        "byte-for-byte one."
      ),
      paste(
        "`erstpublikation`/`letzte_aktualisierung` are stamped at run time, not",
        "derived; agreement is coincidence, not evidence."
      ),
      paste(
        "`wochentag` is an input column, so a wrong weekday in the source",
        "propagates into `arbeitstag` and `kennzahl` unchecked by this harness."
      ),
      paste(
        "This covers the 2024 export only. A value absent from this data is",
        "uncovered, however the pipeline would treat it."
      ),
      paste(
        "`publish_huwise()` and `download_dataset()` are never reached: the",
        "harness compares frozen snapshots and does not publish."
      )
    )
  )
}
