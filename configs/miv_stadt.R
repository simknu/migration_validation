# Dataset config: MIV traffic counts, Stadt St.Gallen.
#
# Two configs over the same dataset, in one file so neither depends on the
# other being sourced first:
#
#   miv_stadt_config()         the step under test -- the published wide dataset
#                              migrated to the long schema by
#                              `migrate_published_to_long()`
#   miv_stadt_derive_config()  the same, but recomputing the derived columns
#                              rather than passing them through
#
# Plain R lists, as the handoff specifies; there is no registry and no loop.
# Run both with:  Rscript scripts/run_dataset.R configs/miv_stadt.R

miv_stadt_config <- function() {
  hours <- as.character(1:24)

  list(
    dataset = "miv_stadt",
    cache_dir = "snapshots",

    # Sourced by scripts/run_dataset.R before this config runs, so the runner
    # needs no knowledge of this dataset. These are the real house files, not
    # stand-ins, so the read path is covered too.
    sources = c(
      "pipeline/00_input_check_fct.R",
      "pipeline/import_export_utf8_fct.R",
      "pipeline/00_helper_fct.R",
      "pipeline/miv_stadt_fct.R"
    ),

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
      "bezeichnung",            "changed",
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

    # A reviewed lookup is what turns a `changed` column from "asserted by
    # nothing" into "asserted by you". Neither is reviewed yet, so the report
    # says so rather than implying the pairs certify themselves.
    #
    # configs/bezeichnung_lookup.R holds the 10 observed `bezeichnung` pairs,
    # UNREVIEWED. Once you have confirmed which side is authoritative, prune it
    # and switch both lines below on -- deliberately left off, because wiring in
    # an unreviewed table would make the report claim an assertion nobody made.
    #
    #   lookup = list(bezeichnung = bezeichnung_lookup()),
    #   lookup_reviewed = list(bezeichnung = TRUE, lnr = FALSE),
    lookup_reviewed = list(bezeichnung = FALSE, lnr = FALSE),

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
        "`bezeichnung` is classed `changed` and its lookup is NOT wired in, so",
        "its 10 differing pairs are asserted by nothing. See",
        "configs/bezeichnung_lookup.R."
      ),
      paste(
        "The two files carry different station rosters, which the pipeline",
        "cannot have caused (it emitted exactly 24 rows per input row, losing",
        "none). 1'368 station-days were never compared: the guarantee covers",
        "1'275'216 of the 1'307'472 rows produced, i.e. 97.5%."
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


# ---------------------------------------------------------------------------
# The primary config certifies `migrate_published_to_long()`, which passes
# `arbeitstag`, `kennzahl` and `ort_richtung_id` through untouched because the
# published input already carries them. That leaves the pipeline's own
# derivation logic uncovered -- precisely the kind of gap this harness exists
# to surface.
#
# This config closes it: it recomputes those columns from the original with
# `derive_miv_cols()` and compares the result against the same ground truth. A
# divergence here means a future run from raw .TXT would publish values that
# differ from what is published today, even though the migration step itself is
# clean.

miv_stadt_derive_config <- function() {
  base <- miv_stadt_config()

  base$dataset <- "miv_stadt_derive"

  base$transform <- function(original) {
    original |>
      dplyr::mutate(
        datum = format(.data$datum, "%Y-%m-%d"),
        richtung = as.character(.data$richtung)
      ) |>
      derive_miv_cols() |>
      dplyr::select(-dplyr::any_of("tagestotal")) |>
      pivot_miv_long() |>
      dplyr::mutate(
        erstpublikation = miv_run_date(),
        letzte_aktualisierung = miv_run_date()
      ) |>
      dplyr::select(dplyr::all_of(miv_schema()$published))
  }

  # These three are now recomputed rather than carried, so the intent for them
  # is "the new derivation reproduces the published value".
  base$limits <- c(
    "Exact multiset equivalence for `unchanged` columns is a true guarantee.",
    "Row order is never checked: bag semantics by design, as accepted.",
    "Values normalizing to empty are dropped before comparison, both sides alike.",
    paste(
      "`arbeitstag`, `kennzahl` and `ort_richtung_id` are RECOMPUTED here by",
      "`derive_miv_cols()`, not passed through. That is the point of this",
      "variant: it tests the derivation logic the primary config cannot reach."
    ),
    paste(
      "`erstpublikation`/`letzte_aktualisierung` are stamped at run time, not",
      "derived; agreement is coincidence, not evidence."
    ),
    "`lnr` is classed `changed` and no lookup was supplied: it is asserted by nothing.",
    paste(
      "`wochentag` is an input column, so a wrong weekday in the source",
      "propagates into `arbeitstag` and `kennzahl` unchecked by this harness."
    ),
    "An input value absent from this data is uncovered, however the pipeline would treat it."
  )

  base
}
