# Dataset config: MIV traffic counts, Stadt St.Gallen -- derivation variant.
#
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
