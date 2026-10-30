# Spec bootstrap: propose a lineage class per output column by matching columns
# on their value content, not their names. Emits a draft for the user to review
# and freeze into the dataset config. This is the only per-dataset manual step.

#' Fingerprint an attribute's canonical bag
#'
#' Shape- and order-independent: the same values in a different row order, or
#' reached through a different reshape, fingerprint identically.
#'
#' @param tbl A `tibble(entity_key, value)` from `canonicalize()`.
#' @param with_key Logical. `TRUE` fingerprints `(entity_key, value)` pairs;
#'   `FALSE` fingerprints the value bag alone, which detects a column whose
#'   content survived but was re-keyed.
#' @return Character scalar hash.
#' @export
bag_fingerprint <- function(tbl, with_key = TRUE) {
  payload <- if (with_key) paste0(tbl$entity_key, "\x1f", tbl$value) else tbl$value
  # Radix sort, not the default: it is far faster on large character vectors
  # and locale-independent, so the fingerprint is reproducible across machines.
  digest::digest(sort(payload, method = "radix"), algo = "xxhash64")
}

#' Draft a lineage spec by matching columns on value content
#'
#' Classifies each output column as `unchanged`, `renamed`, `changed`, `new` or
#' `dropped`. The draft is a starting point, never an assertion: the user
#' reviews it once and freezes it into the dataset config.
#'
#' @param left A `canonicalize()` result for the known-final side.
#' @param right A `canonicalize()` result for the new-output side.
#' @param shared Character vector of entity keys present on both sides. The
#'   draft is restricted to these, because a roster difference would otherwise
#'   make every column's bag differ and classify all of them `changed` --
#'   drowning the real signal. The roster difference is reported separately.
#' @return A tibble with `column`, `class`, `matched_to` and `evidence`.
#' @export
draft_lineage_spec <- function(left, right, shared = NULL) {
  if (!is.null(shared)) {
    left <- restrict_to_keys(left, shared)
    right <- restrict_to_keys(right, shared)
  }

  lf <- vapply(left$attributes, bag_fingerprint, character(1), with_key = TRUE)
  rf <- vapply(right$attributes, bag_fingerprint, character(1), with_key = TRUE)

  # The value-only fingerprint distinguishes "content survived but was
  # re-keyed" from "content changed". It is needed only where the keyed
  # fingerprints already disagree, so it is computed on demand.
  values_match <- function(nm) {
    identical(
      bag_fingerprint(left$attributes[[nm]], with_key = FALSE),
      bag_fingerprint(right$attributes[[nm]], with_key = FALSE)
    )
  }

  rows <- list()
  claimed_left <- character(0)

  for (nm in names(right$attributes)) {
    if (nm %in% names(left$attributes)) {
      if (identical(rf[[nm]], lf[[nm]])) {
        rows[[nm]] <- c(nm, "unchanged", nm, "same name, identical bag on shared keys")
      } else if (values_match(nm)) {
        rows[[nm]] <- c(nm, "changed", nm, "same name and value bag, but re-keyed")
      } else {
        rows[[nm]] <- c(nm, "changed", nm, "same name, differing bag on shared keys")
      }
      claimed_left <- c(claimed_left, nm)
      next
    }

    hit <- names(lf)[lf == rf[[nm]]]
    hit <- setdiff(hit, claimed_left)
    if (length(hit) > 0L) {
      rows[[nm]] <- c(nm, "renamed", hit[[1L]], "identical bag on shared keys under another name")
      claimed_left <- c(claimed_left, hit[[1L]])
    } else {
      rows[[nm]] <- c(nm, "new", NA_character_, "no content match on the known-final side")
    }
  }

  for (nm in setdiff(names(left$attributes), claimed_left)) {
    rows[[paste0("__dropped__", nm)]] <- c(
      nm, "dropped", NA_character_, "present in known-final, absent from new output"
    )
  }

  out <- do.call(rbind, rows)
  tibble::tibble(
    column = unname(out[, 1L]),
    class = unname(out[, 2L]),
    matched_to = unname(out[, 3L]),
    evidence = unname(out[, 4L])
  )
}

#' Diff the raw schemas of the two sides
#'
#' Reports the residue the canonical comparison cannot see: columns that exist
#' on only one side before any reshape.
#'
#' @param left_cols,right_cols Character vectors of column names.
#' @return A tibble with `column` and `side`.
#' @export
diff_schemas <- function(left_cols, right_cols) {
  tibble::tibble(
    column = c(setdiff(left_cols, right_cols), setdiff(right_cols, left_cols)),
    side = c(
      rep("known_final only", length(setdiff(left_cols, right_cols))),
      rep("new_output only", length(setdiff(right_cols, left_cols)))
    )
  )
}

#' Collect the distinct input to output pairs for a `changed` column
#'
#' Thousands of rows collapse to the few mappings actually present. The draft
#' is derived from the pipeline, so checking it against the pipeline is
#' circular: the printed pairs exist for a human to compare against the lookup
#' table, and that one-time review is the actual assertion of intent.
#'
#' @param left A `canonicalize()` result for the known-final side.
#' @param right A `canonicalize()` result for the new-output side.
#' @param attribute Character scalar naming the attribute to tabulate.
#' @return A tibble of distinct `known_final` / `new_output` value pairs with
#'   the number of entity keys exhibiting each pair.
#' @export
changed_value_pairs <- function(left, right, attribute) {
  empty <- tibble::tibble(
    known_final = character(0),
    new_output = character(0),
    n_keys = integer(0)
  )
  l <- left$attributes[[attribute]]
  r <- right$attributes[[attribute]]
  if (is.null(l) || is.null(r)) {
    return(empty)
  }

  # Align by entity key with `match()` rather than a join: on a 1.3M-row side a
  # join dominates the whole run, and only the paired values are wanted.
  idx <- match(l$entity_key, r$entity_key)
  ok <- !is.na(idx)
  if (!any(ok)) {
    return(empty)
  }

  pair <- paste0(l$value[ok], "\x1f", r$value[idx[ok]])
  distinct_pairs <- unique(pair)
  counts <- tabulate(match(pair, distinct_pairs), nbins = length(distinct_pairs))

  parts <- strsplit(distinct_pairs, "\x1f", fixed = TRUE)
  out <- tibble::tibble(
    known_final = vapply(parts, `[`, character(1), 1L),
    new_output = vapply(parts, `[`, character(1), 2L),
    n_keys = as.integer(counts)
  )

  # Pairs where the two sides agree are not what a reviewer needs to see; the
  # count of agreeing keys is carried as an attribute instead.
  agreeing <- dplyr::filter(out, .data$known_final == .data$new_output)
  differing <- out |>
    dplyr::filter(.data$known_final != .data$new_output) |>
    dplyr::arrange(dplyr::desc(.data$n_keys))

  attr(differing, "n_keys_agreeing") <- sum(agreeing$n_keys)
  attr(differing, "n_pairs_agreeing") <- nrow(agreeing)
  differing
}

# Restrict a canonical side to a set of entity keys, so the draft spec compares
# like with like when the two rosters differ.
restrict_to_keys <- function(side, keys) {
  side$attributes <- lapply(
    side$attributes,
    function(tbl) tbl[tbl$entity_key %in% keys, , drop = FALSE]
  )
  side
}
