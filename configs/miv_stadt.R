# Dataset config: MIV traffic counts, Stadt St.Gallen.
#
# Validates the step under test: the published wide dataset migrated to the long
# published schema by `migrate_published_to_long()`. A plain R list, as the
# handoff specifies; there is no registry and no loop.

miv_stadt_config <- function() {
  hours <- as.character(1:24)

  list(
    dataset = "miv_stadt",
    cache_dir = "snapshots",

    # Pre-transform input: the published wide dataset. Parquet is the real
    # input; `resolve_source()` reads the frozen snapshot when one exists, so
    # `arrow` is needed only on the first run of a fresh clone.
    original = list(
      kind = "parquet",
      path = "data/miv_original.parquet",
      label = "miv_original"
    ),

    # Ground truth: the old WYSIWYG transformation's own output. The old
    # pipeline left no code, so this object is its only ground truth.
    known_final = list(
      kind = "parquet",
      path = "data/miv_transformed.parquet",
      label = "miv_transformed"
    ),

    # The new transformation under test, called verbatim from the vendored
    # pipeline. `datum` is handed over as ISO character because that is what
    # `download_dataset()` yields and what `parse_miv_date()` requires.
    transform = function(original) {
      original |>
        dplyr::mutate(datum = format(.data$datum, "%Y-%m-%d")) |>
        migrate_published_to_long(run_date = miv_run_date())
    },

    # `lnr` is deliberately NOT part of the grain key, although
    # `miv_schema()$key` includes it: it is a running number, not an identity.
    # In the published wide data it is a per-station counter (4018 distinct
    # values over 54478 rows, unique within `ort_id`); in the known-final it is
    # a global 0..53157 sequence. Keying on it would match almost nothing and
    # every column would appear to fail. `(ort_id, datum, richtung)` is unique
    # in both, and `stunde` completes the long grain.
    grain_key = c("ort_id", "datum", "richtung", "stunde"),

    # Both sides are already long here: the wide-to-long pivot is inside the
    # pipeline and is itself part of what is under test.
    sides = list(
      known_final = list(),
      new_output = list()
    ),

    # Per-attribute normalization. `standort` is a WKB geo-point carried as
    # lowercase hex, never compared as text.
    normalize = list(
      standort = list(type = "hex"),
      lnr = list(type = "number"),
      richtung = list(type = "number"),
      stunde = list(type = "number"),
      anzahl_fahrzeuge = list(type = "number"),
      datum = list(type = "date"),
      erstpublikation = list(type = "date"),
      letzte_aktualisierung = list(type = "date")
    ),

    # Literal "NA" -> NA is on by default. No MIV column uses "NA" as a
    # legitimate category, and the report states how many cells it converted.
    na_string = TRUE,

    # Frozen lineage spec. Classified by INTENT -- what the pipeline is meant
    # to do with each column -- so a column meant to be carried through
    # verbatim fails loudly if it was not. Review this once; see the report's
    # draft spec for what content-matching proposes.
    lineage = tibble::tribble(
      ~column,                  ~class,
      "bezeichnung",            "unchanged",
      "standort",               "unchanged",
      "wochentag",              "unchanged",
      "arbeitstag",             "unchanged",
      "kennzahl",               "unchanged",
      "ort_richtung_id",        "unchanged",
      "anzahl_fahrzeuge",       "unchanged",
      "lnr",                    "changed",
      "erstpublikation",        "new",
      "letzte_aktualisierung",  "new"
    ),

    # No lookup table was supplied for any `changed` column, so nothing
    # asserts the `lnr` renumbering. The report says so rather than implying
    # the pairs certify themselves.
    lookup_reviewed = list(lnr = FALSE),

    examples_cap = 10L,

    limits = c(
      "Exact multiset equivalence for `unchanged` columns is a true guarantee.",
      "Row order is never checked: bag semantics by design, as accepted.",
      "Values normalizing to empty are dropped before comparison, both sides alike.",
      "`lnr` is classed `changed` and no lookup was supplied: it is asserted by nothing.",
      paste(
        "`erstpublikation`/`letzte_aktualisierung` are stamped at run time, not",
        "derived. They can only agree because the known-final happens to carry",
        "the same run date; this is coincidence, not evidence."
      ),
      paste(
        "This run certifies `migrate_published_to_long()` only. It does NOT",
        "cover `derive_miv_cols()`: the published input already carries",
        "arbeitstag/kennzahl/ort_richtung_id, so they are passed through, not",
        "recomputed. Use configs/miv_stadt_derive.R for that."
      ),
      paste(
        "Snapshots were bridged from Parquet to .RDS in this container (no",
        "`arrow` available); all 49 column checksums verified, so the bridge",
        "cannot be the source of any finding."
      ),
      "An input value absent from this data is uncovered, however the pipeline would treat it.",
      sprintf("Hourly columns melted by the pipeline: %s.", paste(range(hours), collapse = "..."))
    )
  )
}
