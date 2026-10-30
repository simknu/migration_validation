# One entry point per dataset: resolve -> canonicalize -> verdict -> report.
# No master registry and no loop: the user runs one dataset, reads the report,
# moves on.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Validate that a new transformation reproduces a known-final output
#'
#' Resolves both sources, runs the new transformation over the original,
#' reduces both sides to canonical triples, compares them as multisets and
#' builds the coverage ledger. Runs over the full data, locally, with no
#' external calls.
#'
#' @param spec A dataset config: a plain R list, see `configs/`.
#' @param print Logical. Print the report as well as returning it.
#' @return Invisibly, a list carrying the comparison, ledger, draft spec and
#'   verdict.
#' @export
validate_dataset <- function(spec, print = TRUE) {
  examples_cap <- spec[["examples_cap"]] %||% 10L

  original <- resolve_source(spec[["original"]], spec[["cache_dir"]] %||% "snapshots",
                             paste0(spec[["dataset"]], "_original"))
  known_final <- resolve_source(spec[["known_final"]], spec[["cache_dir"]] %||% "snapshots",
                                paste0(spec[["dataset"]], "_known_final"))

  # The new transformation under test. The old pipeline left no code, so its
  # only ground truth is `known_final`; everything compared here is produced by
  # the new R code from the same original.
  new_output <- spec[["transform"]](original)

  left <- canonicalize(known_final, spec[["sides"]]$known_final %||% list(), spec, "known_final")
  right <- canonicalize(new_output, spec[["sides"]]$new_output %||% list(), spec, "new_output")

  grain_check <- dplyr::bind_rows(
    check_grain_unique(left, "known_final"),
    check_grain_unique(right, "new_output")
  )

  # A unique grain admits the cheaper set-based comparison, which is provably
  # identical to the counted one when each (entity_key, value) occurs at most
  # once. `compare_canonical()` falls back to counting when it does not hold.
  unique_grain <- all(grain_check$unique)
  comparison <- compare_canonical(left, right, examples_cap, unique_grain)
  draft_spec <- draft_lineage_spec(left, right, comparison$keys$shared)

  lineage <- spec[["lineage"]] %||% dplyr::select(draft_spec, "column", "class")
  ledger <- coverage_ledger(comparison, lineage, left$diagnostics, right$diagnostics)

  # Value pairs are tabulated for every `changed` column AND for any column
  # that failed: on a failure the distinct pairs are what explains it.
  changed <- dplyr::filter(lineage, .data$class == "changed")$column
  failing <- dplyr::filter(comparison$per_attribute, !isTRUE(.data$equal))$attribute
  pair_cols <- intersect(union(changed, failing), names(left$attributes))
  lookups <- spec[["lookup"]] %||% list()
  changed_pairs <- lapply(
    stats::setNames(pair_cols, pair_cols),
    function(nm) annotate_with_lookup(
      changed_value_pairs(left, right, nm),
      lookups[[nm]]
    )
  )

  result <- list(
    dataset = spec[["dataset"]],
    run_at = format(Sys.time(), tz = "Europe/Zurich", usetz = TRUE),
    grain_key = spec[["grain_key"]],
    sources = list(
      known_final = spec[["known_final"]]$path %||% spec[["known_final"]]$dataset_id %||% "<object>",
      original = spec[["original"]]$path %||% spec[["original"]]$dataset_id %||% "<object>"
    ),
    n_rows = list(
      original = nrow(original),
      new_output = nrow(new_output),
      known_final = nrow(known_final)
    ),
    grain_check = grain_check,
    unique_grain = unique_grain,
    comparison = comparison,
    ledger = ledger,
    draft_spec = draft_spec,
    lineage = lineage,
    changed_pairs = changed_pairs,
    lookup_reviewed = spec[["lookup_reviewed"]] %||% list(),
    schema_residue = diff_schemas(colnames(known_final), colnames(new_output)),
    examples_cap = examples_cap,
    limits = spec[["limits"]] %||% default_limits()
  )

  if (isTRUE(print)) {
    print_report(result)
  }
  invisible(result)
}

default_limits <- function() {
  c(
    "Exact multiset equivalence for `renamed`/`unchanged` columns is a true guarantee.",
    "Exhaustive over mapping inputs present in the data; an unseen future value is uncovered.",
    "`changed` columns are test-strength only, bounded by the reviewed lookup table.",
    "A reshape is covered only where both sides reduce to the same triple grain.",
    "Row order is never checked: the comparison is bag semantics by design.",
    "Values that normalize to empty are dropped before comparison, on both sides alike."
  )
}
