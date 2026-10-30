# Key handling beyond a plain rename.
#
# A rename is free: the rename map is applied before the grain key is read, so
# a key that only changed name needs nothing else. A key whose *values* were
# transformed is the dangerous case. If two old key values map to one new one,
# rows silently collapse and every row-level diff below becomes a lie, while
# the bag compare still returns a tidy-looking verdict. So the map is verified
# to be a bijection first, and drill-down is refused when it is not.

#' Verify a declared key-value map is a bijection
#'
#' @param key_map A named list: one entry per grain-key column whose values
#'   were transformed, each a data frame with `from` (the value as it appears
#'   on the known-final side) and `to` (the value on the new-output side).
#' @param left A `canonicalize()`-style side, or `NULL`. When given, the map is
#'   also checked for coverage of the key values actually present.
#' @return A tibble, one row per mapped key column: `key_column`, `n_pairs`,
#'   `n_from_ambiguous`, `n_to_ambiguous`, `n_uncovered`, `bijective`.
#' @export
check_key_bijection <- function(key_map, left = NULL) {
  if (length(key_map) == 0L) {
    return(tibble::tibble(
      key_column = character(0),
      n_pairs = integer(0),
      n_from_ambiguous = integer(0),
      n_to_ambiguous = integer(0),
      n_uncovered = integer(0),
      bijective = logical(0)
    ))
  }

  rows <- lapply(names(key_map), function(nm) {
    map <- validate_key_map(key_map[[nm]], nm)
    pairs <- dplyr::distinct(tibble::tibble(from = map$from, to = map$to))

    # One `from` reaching two `to` values is not a function; two `from` values
    # reaching one `to` is the collapse that invalidates row diffs.
    from_ambiguous <- sum(duplicated(pairs$from))
    to_ambiguous <- sum(duplicated(pairs$to))

    n_uncovered <- NA_integer_
    if (!is.null(left) && nm %in% colnames(left$grain)) {
      present <- unique(left$grain[[nm]])
      n_uncovered <- as.integer(sum(!present %in% pairs$from))
    }

    tibble::tibble(
      key_column = nm,
      n_pairs = nrow(pairs),
      n_from_ambiguous = as.integer(from_ambiguous),
      n_to_ambiguous = as.integer(to_ambiguous),
      n_uncovered = n_uncovered,
      bijective = from_ambiguous == 0L && to_ambiguous == 0L
    )
  })

  dplyr::bind_rows(rows)
}

validate_key_map <- function(map, nm) {
  if (!is.data.frame(map) || !all(c("from", "to") %in% colnames(map))) {
    stop(
      "key_map[[\"", nm, "\"]] must be a data frame with `from` and `to` ",
      "columns; got ", paste(class(map), collapse = "/")
    )
  }
  if (anyNA(map$from) || anyNA(map$to)) {
    stop("key_map[[\"", nm, "\"]] must not contain NA in `from` or `to`")
  }
  map
}

# Translate the mapped key columns of one side into the other's key space.
# Unmapped values are left as they are: they then simply fail to match, which
# the entity-key roster reports, rather than being silently dropped.
apply_key_map <- function(key_values, key_map) {
  for (nm in intersect(names(key_map), names(key_values))) {
    map <- key_map[[nm]]
    hit <- match(key_values[[nm]], as.character(map$from))
    translated <- as.character(map$to)[hit]
    key_values[[nm]] <- ifelse(is.na(hit), key_values[[nm]], translated)
  }
  key_values
}
