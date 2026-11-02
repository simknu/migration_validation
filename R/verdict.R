# The guarantee: a deterministic full-data multiset comparison, computed
# locally with zero external calls. Order is never checked; repeated
# (entity, attribute) values are kept rather than deduplicated, so genuine
# repeats are covered without an occurrence index.

#' Compare two canonical sides as multisets of triples
#'
#' Per attribute, compares the bag of `(entity_key, value)` pairs on both
#' sides. Pass means every attribute's bag is identical and the entity-key
#' rosters agree.
#'
#' @param left A `canonicalize()` result: the known-final (ground truth) side.
#' @param right A `canonicalize()` result: the new transformation's output.
#' @param examples_cap Integer. Maximum diverging triples to retain per
#'   attribute for the report.
#' @param unique_grain Logical. `TRUE` when `check_grain_unique()` found the
#'   grain unique on both sides, which admits the cheaper set-based path.
#' @return A list with `per_attribute`, `keys`, `examples` and `pass`.
#' @export
compare_canonical <- function(left, right, examples_cap = 10L, unique_grain = TRUE) {
  keys <- compare_key_bags(left, right)

  attrs <- union(names(left$attributes), names(right$attributes))
  rows <- list()
  examples <- list()

  for (nm in attrs) {
    l <- left$attributes[[nm]]
    r <- right$attributes[[nm]]

    if (is.null(l) || is.null(r)) {
      rows[[nm]] <- tibble::tibble(
        attribute = nm,
        present_both = FALSE,
        n_left = if (is.null(l)) NA_integer_ else nrow(l),
        n_right = if (is.null(r)) NA_integer_ else nrow(r),
        n_left_only = NA_integer_,
        n_right_only = NA_integer_,
        equal = NA,
        equal_on_shared_keys = NA
      )
      next
    }

    diff <- bag_difference(l, r, unique_grain, examples_cap)
    equal <- diff$n_left_only == 0L && diff$n_right_only == 0L

    # Restricting both sides to the same key subset cannot break an equality,
    # so a passing attribute needs no second pass.
    shared_equal <- if (equal) {
      TRUE
    } else {
      bag_equal_on_keys(l, r, keys$shared, unique_grain)
    }

    rows[[nm]] <- tibble::tibble(
      attribute = nm,
      present_both = TRUE,
      n_left = nrow(l),
      n_right = nrow(r),
      n_left_only = diff$n_left_only,
      n_right_only = diff$n_right_only,
      equal = equal,
      equal_on_shared_keys = shared_equal
    )

    if (nrow(diff$examples) > 0L) {
      examples[[nm]] <- dplyr::mutate(diff$examples, attribute = nm)
    }
  }

  per_attribute <- dplyr::bind_rows(rows)

  list(
    per_attribute = per_attribute,
    keys = keys,
    examples = dplyr::bind_rows(examples),
    pass = isTRUE(all(per_attribute$equal)) && isTRUE(keys$equal)
  )
}

# Pack a triple into one string so a bag of triples is a vector of strings.
pack_triples <- function(tbl) {
  paste0(tbl$entity_key, "\x1f", tbl$value)
}

#' @noRd
bag_difference <- function(l, r, unique_grain, examples_cap = 10L) {
  lk <- pack_triples(l)
  rk <- pack_triples(r)

  if (!unique_grain) {
    return(bag_difference_counted(l, r, lk, rk, examples_cap))
  }

  # With a unique grain each (entity_key, value) pair occurs at most once per
  # attribute, so the multiset equals the set and `match()` answers the whole
  # question -- including which rows diverge, by index, with no string surgery.
  only_left <- which(is.na(match(lk, rk)))
  only_right <- which(is.na(match(rk, lk)))

  list(
    n_left_only = length(only_left),
    n_right_only = length(only_right),
    examples = example_triples(l, only_left, r, only_right, examples_cap)
  )
}

# Fallback for a non-unique grain: counts matter, so group and compare counts.
bag_difference_counted <- function(l, r, lk, rk, examples_cap) {
  lc <- dplyr::count(tibble::tibble(triple = lk), .data$triple, name = "n_left")
  rc <- dplyr::count(tibble::tibble(triple = rk), .data$triple, name = "n_right")

  joined <- dplyr::full_join(lc, rc, by = dplyr::join_by("triple")) |>
    mutate(
      n_left = dplyr::coalesce(.data$n_left, 0L),
      n_right = dplyr::coalesce(.data$n_right, 0L)
    )
  bad <- dplyr::filter(joined, .data$n_left != .data$n_right)

  only_left <- which(lk %in% bad$triple[bad$n_left > bad$n_right])
  only_right <- which(rk %in% bad$triple[bad$n_right > bad$n_left])

  list(
    n_left_only = as.integer(sum(pmax(bad$n_left - bad$n_right, 0L))),
    n_right_only = as.integer(sum(pmax(bad$n_right - bad$n_left, 0L))),
    examples = example_triples(l, only_left, r, only_right, examples_cap)
  )
}

example_triples <- function(l, only_left, r, only_right, cap) {
  li <- utils::head(only_left, cap)
  ri <- utils::head(only_right, cap)
  tibble::tibble(
    side = c(
      rep("known_final only", length(li)),
      rep("new_output only", length(ri))
    ),
    entity_key = c(l$entity_key[li], r$entity_key[ri]),
    value = c(l$value[li], r$value[ri])
  )
}

# Are the two bags equal once both sides are restricted to the shared keys?
bag_equal_on_keys <- function(l, r, shared, unique_grain) {
  lk <- pack_triples(l)[l$entity_key %in% shared]
  rk <- pack_triples(r)[r$entity_key %in% shared]

  if (length(lk) != length(rk)) {
    return(FALSE)
  }
  if (unique_grain) {
    return(!anyNA(match(lk, rk)) && !anyNA(match(rk, lk)))
  }
  identical(
    sort(lk, method = "radix"),
    sort(rk, method = "radix")
  )
}

# The entity-key roster is compared on its own, so a roster difference is
# reported once rather than echoing through every attribute's verdict.
compare_key_bags <- function(left, right) {
  lk <- unique(left$entity_key)
  rk <- unique(right$entity_key)
  shared <- intersect(lk, rk)

  list(
    n_left = length(lk),
    n_right = length(rk),
    n_shared = length(shared),
    left_only = setdiff(lk, rk),
    right_only = setdiff(rk, lk),
    shared = shared,
    equal = length(lk) == length(shared) && length(rk) == length(shared)
  )
}

#' Verify the comparison grain is unique before any row-level drill-down
#'
#' A silent many-to-one grain would make row diffs mislead, so drill-down is
#' refused when the grain is not unique. The verdict itself is unaffected: bag
#' semantics keep repeats, so it still runs.
#'
#' @param side A `canonicalize()` result.
#' @param label Character scalar naming the side.
#' @return A tibble with one row: `side`, `n_rows`, `n_distinct_keys`,
#'   `n_duplicated` and `unique`.
#' @export
check_grain_unique <- function(side, label) {
  # Values are computed before the tibble: a column named `side` would
  # otherwise shadow the `side` argument inside the data mask.
  n_rows <- side$n_rows
  n_distinct_keys <- length(unique(side$entity_key))
  tibble::tibble(
    side = label,
    n_rows = n_rows,
    n_distinct_keys = n_distinct_keys,
    n_duplicated = as.integer(n_rows - n_distinct_keys),
    unique = n_rows == n_distinct_keys
  )
}
