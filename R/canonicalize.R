# Reduce a table to the shape-independent canonical form the verdict compares:
# triples (entity_key, attribute, value), stored factored by attribute.
#
# Factoring by attribute is not a weakening. `attribute` is part of every
# triple, so triples carrying different attributes can never collide; the bag
# of all triples is equal iff the bag of (entity_key, value) pairs is equal for
# every attribute. Keeping them factored costs far less memory on a 1.3M-row
# table and yields the per-column verdict the coverage ledger needs anyway.

#' Normalize one column to its canonical character form
#'
#' Removes deterministic representation noise only: see the per-type rules in
#' the package README. Dispatch is on the column's class unless the dataset
#' config overrides `type` for this attribute.
#'
#' @param x A vector to normalize.
#' @param rule A list with optional `type`, `tolerance` and `na_string`.
#' @param na_string Logical. Dataset-level default for mapping `"NA"` to `NA`.
#' @return A list with `value` (character) and `n_na_string` (integer count of
#'   literal `"NA"` strings converted, so an unintended coercion is visible).
#' @export
normalize_column <- function(x, rule = list(), na_string = TRUE) {
  type <- rule$type %||% infer_value_type(x)
  use_na_string <- rule$na_string %||% na_string

  switch(
    type,
    string = normalize_string(x, use_na_string),
    hex = normalize_hex(x),
    geopoint = normalize_geopoint(x, rule),
    number = normalize_number(x, rule$tolerance),
    date = normalize_date(x),
    datetime = normalize_datetime(x),
    logical = list(value = normalize_logical(x), n_na_string = 0L),
    stop("unknown normalization type: ", type)
  )
}

# Class-based default; `blob`/raw-list columns are WKB or similar binary.
infer_value_type <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.numeric(x)) {
    return("number")
  }
  if (is.list(x) || inherits(x, "blob")) {
    return("hex")
  }
  "string"
}

# UTF-8, trimmed, internal whitespace runs collapsed to one space.
normalize_string <- function(x, na_string) {
  out <- enc2utf8(as.character(x))
  out <- trimws(out)
  out <- gsub("[[:space:]]+", " ", out, perl = TRUE)

  n_na_string <- 0L
  if (isTRUE(na_string)) {
    hit <- !is.na(out) & out == "NA"
    n_na_string <- sum(hit)
    out[hit] <- NA_character_
  }

  # Post-trim empty counts as missing and is dropped before comparison.
  out[!is.na(out) & out == ""] <- NA_character_
  list(value = out, n_na_string = as.integer(n_na_string))
}

# Binary (WKB geometry and friends) compared as lowercase hex, never as text.
normalize_hex <- function(x) {
  out <- if (is.list(x)) {
    vapply(
      x,
      function(b) {
        if (is.null(b) || length(b) == 0L) {
          NA_character_
        } else {
          paste(sprintf("%02x", as.integer(b)), collapse = "")
        }
      },
      character(1)
    )
  } else {
    tolower(trimws(as.character(x)))
  }
  out[!is.na(out) & out == ""] <- NA_character_
  list(value = out, n_na_string = 0L)
}

# A geo column reaches the two sides in different representations: a portal
# export carries WKB binary, while the source CSV behind it carries "lat, lon"
# text. Comparing either as a string would report every row as divergent, so
# both are decoded to coordinates and rendered in one canonical order.
#
# Only the distinct values are decoded: a geo column typically holds a handful
# of stations repeated across millions of rows, and per-row decoding would
# dominate the run.
normalize_geopoint <- function(x, rule = list()) {
  digits <- rule$digits %||% 8L
  order <- rule$order %||% "latlon"
  if (!order %in% c("latlon", "lonlat")) {
    stop("`order` for a geopoint must be \"latlon\" or \"lonlat\", not ", order)
  }

  chr <- if (is.list(x)) normalize_hex(x)$value else trimws(as.character(x))
  chr[!is.na(chr) & chr == ""] <- NA_character_

  present <- unique(chr[!is.na(chr)])
  canonical <- vapply(
    present,
    function(v) geopoint_canonical(v, digits, order),
    character(1),
    USE.NAMES = FALSE
  )

  out <- rep(NA_character_, length(chr))
  hit <- !is.na(chr)
  out[hit] <- canonical[match(chr[hit], present)]
  list(value = out, n_na_string = 0L)
}

# Renders as "lon,lat" whatever the input looked like.
geopoint_canonical <- function(v, digits, order) {
  if (grepl("^(00|01)[0-9a-fA-F]{40}$", v)) {
    xy <- parse_wkb_point(v)
    return(sprintf("%.*f,%.*f", digits, xy[[1L]], digits, xy[[2L]]))
  }

  parts <- strsplit(v, ",", fixed = TRUE)[[1L]]
  if (length(parts) == 2L) {
    nums <- suppressWarnings(as.numeric(trimws(parts)))
    if (!anyNA(nums)) {
      lon <- if (order == "latlon") nums[[2L]] else nums[[1L]]
      lat <- if (order == "latlon") nums[[1L]] else nums[[2L]]
      return(sprintf("%.*f,%.*f", digits, lon, digits, lat))
    }
  }

  stop(
    "cannot read as a geopoint: ", substr(v, 1L, 60L),
    ". Expected WKB-point hex or a \"lat, lon\" pair."
  )
}

# Little- or big-endian WKB Point; returns c(x = lon, y = lat).
parse_wkb_point <- function(hex) {
  bytes <- as.raw(strtoi(
    substring(hex, seq(1L, 41L, by = 2L), seq(2L, 42L, by = 2L)),
    base = 16L
  ))
  endian <- if (bytes[[1L]] == as.raw(1L)) "little" else "big"

  geom_type <- readBin(bytes[2:5], "integer", n = 1L, size = 4L, endian = endian)
  if (geom_type != 1L) {
    stop("WKB geometry type ", geom_type, " is not a Point")
  }

  readBin(bytes[6:21], "double", n = 2L, size = 8L, endian = endian)
}

# Compared as numbers, not strings. `sprintf("%.15g")` formats element-wise, so
# a column's rendering never depends on the other values present in it -- which
# `format()` would, and which would make two sides disagree spuriously.
#
# Tolerance is applied by QUANTIZATION, because bag semantics give no pairwise
# row correspondence to compare against: values are snapped to a grid and the
# grid points are compared exactly. The consequence is a boundary effect -- two
# values closer than the tolerance can still land in adjacent buckets (with
# `abs = 1`, 1.4999 and 1.5001 become 1 and 2). Tolerance here means "ignore
# differences below this scale", not "pair up rows and subtract".
normalize_number <- function(x, tolerance = NULL) {
  v <- suppressWarnings(as.numeric(x))
  tol <- parse_tolerance(tolerance)

  if (!is.null(tol$abs)) {
    v <- round(v / tol$abs) * tol$abs
  }

  if (!is.null(tol$rel)) {
    # Quantize in log space, so the bucket width scales with magnitude. Zero
    # has no relative neighbourhood and is left alone.
    step <- log1p(tol$rel)
    nz <- !is.na(v) & v != 0
    v[nz] <- sign(v[nz]) * exp(round(log(abs(v[nz])) / step) * step)
  }

  out <- sprintf("%.15g", v)
  out[is.na(v)] <- NA_character_
  list(value = out, n_na_string = 0L)
}

# A bare number means absolute tolerance, for brevity in the common case.
parse_tolerance <- function(tolerance) {
  if (is.null(tolerance)) {
    return(list(abs = NULL, rel = NULL))
  }

  if (is.numeric(tolerance) && length(tolerance) == 1L) {
    return(list(abs = if (tolerance > 0) tolerance else NULL, rel = NULL))
  }

  if (!is.list(tolerance)) {
    stop("`tolerance` must be a single number or a list with `abs` and/or `rel`")
  }

  unknown <- setdiff(names(tolerance), c("abs", "rel"))
  if (length(unknown) > 0L) {
    stop(
      "unknown `tolerance` field(s): ", paste(unknown, collapse = ", "),
      ". Use `abs` and/or `rel`."
    )
  }
  for (f in c("abs", "rel")) {
    v <- tolerance[[f]]
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0)) {
      stop("`tolerance$", f, "` must be a single non-negative number")
    }
  }

  list(
    abs = if (!is.null(tolerance$abs) && tolerance$abs > 0) tolerance$abs else NULL,
    rel = if (!is.null(tolerance$rel) && tolerance$rel > 0) tolerance$rel else NULL
  )
}

normalize_date <- function(x) {
  d <- if (inherits(x, "Date")) x else as.Date(as.character(x), format = "%Y-%m-%d")
  list(value = format(d, "%Y-%m-%d"), n_na_string = 0L)
}

normalize_datetime <- function(x) {
  tm <- if (inherits(x, "POSIXt")) x else as.POSIXct(as.character(x), tz = "UTC")
  list(
    value = format(tm, "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"),
    n_na_string = 0L
  )
}

normalize_logical <- function(x) {
  out <- ifelse(is.na(x), NA_character_, ifelse(as.logical(x), "TRUE", "FALSE"))
  as.character(out)
}

#' Reduce one side of the comparison to canonical triples
#'
#' Applies the rename map, melts replicated measures so wide and long input
#' reach the same grain, normalizes every value and drops empty ones.
#'
#' @param data A data frame: one side of the comparison.
#' @param side_spec A list with optional `rename` and `reshape`.
#' @param spec The dataset config (see `configs/`).
#' @param side Character scalar naming the side, for error messages.
#' @return A list with `attributes` (named list of `tibble(entity_key, value)`),
#'   `grain` (the normalized key columns), `n_rows`, and `diagnostics`.
#' @export
canonicalize <- function(data, side_spec, spec, side = "side") {
  data <- tibble::as_tibble(data)

  rename <- side_spec[["rename"]]
  if (length(rename) > 0L) {
    present <- intersect(names(rename), colnames(data))
    missing_renames <- setdiff(names(rename), colnames(data))
    if (length(missing_renames) > 0L) {
      warning(
        side, ": rename map names column(s) absent from the data: ",
        paste(missing_renames, collapse = ", ")
      )
    }
    colnames(data)[match(present, colnames(data))] <- unname(rename[present])
  }

  # Melt replicated measures: the wide column name becomes a key dimension
  # (e.g. the 24 hourly columns become `stunde`), not an attribute, because
  # they carry one measure replicated across that dimension.
  reshape <- side_spec[["reshape"]]
  if (!is.null(reshape)) {
    missing_cols <- setdiff(reshape$cols, colnames(data))
    if (length(missing_cols) > 0L) {
      stop(
        side, ": reshape expects column(s) absent from the data: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    data <- tidyr::pivot_longer(
      data,
      cols = all_of(reshape$cols),
      names_to = reshape$names_to,
      values_to = reshape$values_to
    )
  }

  key_cols <- spec[["grain_key"]]
  missing_key <- setdiff(key_cols, colnames(data))
  if (length(missing_key) > 0L) {
    stop(
      side, ": grain key column(s) missing after reshape: ",
      paste(missing_key, collapse = ", ")
    )
  }

  drop_cols <- union(spec[["ignore"]] %||% character(0), side_spec[["ignore"]] %||% character(0))
  attr_cols <- setdiff(colnames(data), union(key_cols, drop_cols))

  rules <- spec[["normalize"]] %||% list()
  na_string <- spec[["na_string"]] %||% TRUE

  # Keys are normalized with the same rules as values, so a Date on one side
  # and an ISO string on the other describe the same entity.
  key_norm <- lapply(
    key_cols,
    function(nm) normalize_column(data[[nm]], rules[[nm]] %||% list(), na_string)$value
  )
  names(key_norm) <- key_cols

  # A key whose VALUES were transformed is translated into the other side's key
  # space here. check_key_bijection() must vouch for the map first; a map that
  # is not one-to-one collapses rows and makes every row-level diff a lie.
  key_map <- side_spec[["key_map"]]
  if (length(key_map) > 0L) {
    key_norm <- apply_key_map(key_norm, key_map)
  }

  entity_key <- do.call(paste, c(key_norm, list(sep = "\x1f")))

  attributes_out <- list()
  diagnostics <- list()
  for (nm in attr_cols) {
    norm <- normalize_column(data[[nm]], rules[[nm]] %||% list(), na_string)
    keep <- !is.na(norm$value)
    attributes_out[[nm]] <- tibble::tibble(
      entity_key = entity_key[keep],
      value = norm$value[keep]
    )
    diagnostics[[nm]] <- tibble::tibble(
      attribute = nm,
      n_total = length(keep),
      n_empty_dropped = as.integer(sum(!keep)),
      n_na_string = norm$n_na_string
    )
  }

  list(
    attributes = attributes_out,
    grain = tibble::as_tibble(key_norm),
    entity_key = entity_key,
    n_rows = nrow(data),
    raw_columns = colnames(data),
    diagnostics = dplyr::bind_rows(diagnostics)
  )
}
