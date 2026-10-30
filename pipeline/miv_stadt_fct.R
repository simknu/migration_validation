#' MIV schema definition (single source of truth)
#'
#' Central definition of every column set the MIV pipeline depends on. Kept in
#' one place so a change to the published schema, the key, or the expected
#' future long-format input is a single edit.
#'
#' @return A named list:
#'   \item{wide_input}{Character. Mandatory header of a wide \code{.TXT} export:
#'     \code{LNR}, \code{ORT-ID}, \code{BEZEICHNUNG}, \code{DATUM},
#'     \code{WOCHENTAG}, \code{RI}, and the 24 hourly columns \code{"1"}--\code{"24"}.}
#'   \item{long_input}{Character. Required columns of a future long-format
#'     \code{.TXT} export (measured columns only; the rest are derived).}
#'   \item{hours}{Character. The 24 hourly labels \code{"1"}--\code{"24"}.}
#'   \item{key}{Character. Row key of the long published data:
#'     \code{lnr}, \code{ort_id}, \code{datum}, \code{richtung}, \code{stunde}.}
#'   \item{tracking}{Character. The two change-tracking columns.}
#'   \item{processed}{Character. Published columns without tracking, in order.}
#'   \item{published}{Character. Full published column order (tracking last).}
#'
#' @noRd
#' @keywords internal
miv_schema <- function() {
  hours <- as.character(1:24)
  processed <- c(
    "lnr",
    "ort_id",
    "bezeichnung",
    "standort",
    "datum",
    "wochentag",
    "arbeitstag",
    "kennzahl",
    "richtung",
    "ort_richtung_id",
    "stunde",
    "anzahl_fahrzeuge"
  )
  tracking <- c("erstpublikation", "letzte_aktualisierung")

  list(
    wide_input = c(
      "LNR",
      "ORT-ID",
      "BEZEICHNUNG",
      "DATUM",
      "WOCHENTAG",
      "RI",
      hours
    ),
    long_input = c(
      "lnr",
      "ort_id",
      "bezeichnung",
      "datum",
      "wochentag",
      "richtung",
      "stunde",
      "anzahl_fahrzeuge"
    ),
    hours = hours,
    key = c("lnr", "ort_id", "datum", "richtung", "stunde"),
    tracking = tracking,
    processed = processed,
    published = c(processed, tracking)
  )
}

#' Run date in ISO 8601, anchored to Europe/Zurich
#'
#' The date the pipeline runs, formatted \code{"YYYY-MM-DD"}. The instant is
#' resolved in \code{Europe/Zurich} so the value is deterministic regardless of
#' the executing machine's system time zone.
#'
#' @return Character scalar, an ISO 8601 date.
#'
#' @noRd
#' @keywords internal
miv_run_date <- function() {
  format(as.Date(Sys.time(), tz = "Europe/Zurich"), "%Y-%m-%d")
}


#' Parse an MIV `datum` column, accepting ISO 8601 or DD.MM.YYYY
#'
#' Classifies each value by its literal shape and parses it strictly, so a
#' wrong-format value is never coerced to a wrong date or silently to \code{NA};
#' \code{"\%Y-\%m-\%d"} and \code{"\%d.\%m.\%Y"} may even be mixed within one
#' column. Any non-empty value matching neither shape, or matching a shape but
#' not a real calendar date (e.g. \code{"31.02.2024"}), is a hard error.
#' \code{NA}/\code{""} pass through as \code{NA}, matching
#' \code{\link{derive_miv_cols}}.
#'
#' @param x Character vector of date strings.
#' @param call Environment. Caller, for error attribution.
#'
#' @return Character vector of ISO 8601 dates (\code{"\%Y-\%m-\%d"}); \code{NA}
#'   or \code{""} in, \code{NA} out.
#'
#' @noRd
#' @keywords internal
parse_miv_date <- function(x, call = rlang::caller_env()) {
  check_character_vector(x, call = call)

  trimmed <- trimws(x)
  is_iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trimmed)
  is_ch <- grepl("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$", trimmed)

  parsed <- as.Date(rep(NA_character_, length(trimmed)))
  parsed[is_iso] <- as.Date(trimmed[is_iso], format = "%Y-%m-%d")
  parsed[is_ch] <- as.Date(trimmed[is_ch], format = "%d.%m.%Y")

  bad <- is.na(parsed) & !is.na(x) & trimmed != ""
  if (any(bad)) {
    cli::cli_abort(
      c(
        "Unparseable {.field datum} value{?s} in the MIV data.",
        "x" = "{cli::qty(sum(bad))}Offending value{?s}: {.val {unique(x[bad])}}.",
        "i" = "Expected {.val YYYY-MM-DD} or {.val DD.MM.YYYY}."
      ),
      class = "Huwise_error_value",
      call = call
    )
  }

  format(parsed, "%Y-%m-%d")
}


#' Read a wide MIV traffic count file
#'
#' Reads a semicolon- or tab-separated wide MIV traffic count text file,
#' skipping metadata rows that precede the header. The header is the first row
#' whose first field equals \code{"LNR"}; it is located via an initial raw read
#' with \code{\link{load_txt_utf8}}, after which the file is re-read from that
#' row onward. UTF-8 normalisation and encoding detection are handled by
#' \code{\link{load_txt_utf8}}. Any unexpected extra columns are dropped so the
#' result carries exactly \code{miv_schema()$wide_input}.
#'
#' @param file Character scalar. Path to the input \code{.TXT} file.
#' @param sep Character scalar. Field separator forwarded to
#'   \code{\link{load_txt_utf8}}; \code{"auto"} lets the reader guess.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A \code{data.frame}/tibble starting at the \code{LNR} header row with
#'   exactly the columns of \code{miv_schema()$wide_input}, UTF-8-normalised by
#'   \code{\link{load_txt_utf8}}.
#'
#' @seealso \code{\link{read_miv_long_file}}, \code{\link{load_txt_utf8}}
#' @noRd
#' @keywords internal
read_miv_file <- function(
  file,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_file(file, call = call)

  mandatory_cols <- miv_schema()$wide_input

  raw_data <- load_txt_utf8(file, sep = sep, skip = 0L)

  if (identical(colnames(raw_data), mandatory_cols)) {
    return(raw_data)
  }

  if (all(mandatory_cols %in% colnames(raw_data))) {
    extra_cols <- setdiff(colnames(raw_data), mandatory_cols)
    if (length(extra_cols) > 0L) {
      cli::cli_warn(c(
        "{cli::qty(length(extra_cols))}Removing unexpected extra column{?s} from {.path {file}}.",
        "i" = "{cli::qty(length(extra_cols))}Column{?s} removed: {.col {extra_cols}}"
      ))
    }
    return(dplyr::select(raw_data, dplyr::all_of(mandatory_cols)))
  }

  header_row <- which(raw_data[[1]] == "LNR")
  if (length(header_row) == 0L) {
    cli::cli_abort(
      c(
        "No row with first field {.val LNR} found in {.path {file}}.",
        "i" = "Verify that the file is a valid wide MIV traffic count export."
      ),
      class = "Huwise_error_schema",
      call = call
    )
  }

  result <- load_txt_utf8(
    file,
    sep = sep,
    skip = header_row[1L] - 1L
  )

  if (all(mandatory_cols %in% colnames(result))) {
    extra_cols <- setdiff(colnames(result), mandatory_cols)
    if (length(extra_cols) > 0L) {
      cli::cli_warn(c(
        "{cli::qty(length(extra_cols))}Removing unexpected extra column{?s} from {.path {file}}.",
        "i" = "{cli::qty(length(extra_cols))}Column{?s} removed: {.col {extra_cols}}"
      ))
    }
    return(dplyr::select(result, dplyr::all_of(mandatory_cols)))
  }

  cli::cli_abort(
    c(
      "Unexpected columns after re-reading {.path {file}}.",
      "i" = "Expected: {.col {mandatory_cols}}",
      "i" = "Got: {.col {colnames(result)}}"
    ),
    class = "Huwise_error_schema",
    call = call
  )
}

#' Read a long-format MIV traffic count file
#'
#' Reads a future long-format \code{.TXT} export (already one row per hour) and
#' validates it against \code{miv_schema()$long_input}. Extra columns are
#' dropped downstream by \code{\link{transform_miv_long}}, so any derived column
#' that happens to be present in the input is ignored and recomputed.
#'
#' @inheritParams read_miv_file
#'
#' @return A \code{data.frame}/tibble containing at least the columns of
#'   \code{miv_schema()$long_input}, UTF-8-normalised by
#'   \code{\link{load_txt_utf8}}.
#'
#' @seealso \code{\link{read_miv_file}}, \code{\link{load_txt_utf8}}
#' @noRd
#' @keywords internal
read_miv_long_file <- function(
  file,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_file(file, call = call)

  required <- miv_schema()$long_input

  result <- load_txt_utf8(file, sep = sep, skip = 0L)
  cols <- colnames(result)

  if (!all(required %in% cols)) {
    missing <- setdiff(required, cols)
    cli::cli_abort(
      c(
        "{.path {file}} is not a valid long-format MIV export.",
        "x" = "{cli::qty(length(missing))}Missing required column{?s}: {.col {missing}}.",
        "i" = "Expected at least: {.col {required}}"
      ),
      class = "Huwise_error_schema",
      call = call
    )
  }

  result
}

#' Detect the shape of an MIV traffic count file
#'
#' Peeks at the header of \code{file} to classify it as wide (24 hourly columns
#' behind an \code{LNR} header) or long (one row per hour). Detection is by
#' schema, never by guessing on content.
#'
#' @inheritParams read_miv_file
#'
#' @return Character scalar, either \code{"wide"} or \code{"long"}.
#'
#' @noRd
#' @keywords internal
detect_miv_shape <- function(
  file,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_file(file, call = call)

  raw <- load_txt_utf8(file, sep = sep, skip = 0L)
  cols <- colnames(raw)
  first_col <- raw[[1]]

  if (all(miv_schema()$long_input %in% cols)) {
    return("long")
  }

  if (
    all(miv_schema()$wide_input %in% cols) ||
      any(first_col == "LNR", na.rm = TRUE)
  ) {
    return("wide")
  }

  cli::cli_abort(
    c(
      "Cannot determine the shape of {.path {file}}.",
      "i" = "Expected a wide export (an {.val LNR} header with columns \\
             {.col {miv_schema()$hours}}) or a long export with columns \\
             {.col {miv_schema()$long_input}}."
    ),
    class = "Huwise_error_schema",
    call = call
  )
}

#' Abort if a data frame holds NA or empty-string values
#'
#' Shared validator for the single-file loaders. Aborts on the first offending
#' category (NA, then empty string).
#'
#' @param data A \code{data.frame} of character columns.
#' @param file Character scalar. Source path, for the error message.
#' @param call Environment. Caller, for error attribution.
#'
#' @return Invisibly, \code{data}. Called for its side effect of aborting.
#'
#' @noRd
#' @keywords internal
check_no_na_empty <- function(data, file, call = rlang::caller_env()) {
  na_cols <- names(which(purrr::map_int(data, \(x) sum(is.na(x))) > 0L))
  if (length(na_cols) > 0L) {
    cli::cli_abort(
      c(
        "{.path {file}} contains {.val NA} values.",
        "i" = "{cli::qty(length(na_cols))}Affected column{?s}: {.col {na_cols}}"
      ),
      class = "Huwise_error_missing_value",
      call = call
    )
  }

  empty_cols <- names(which(
    purrr::map_int(data, \(x) sum(x == "", na.rm = TRUE)) > 0L
  ))
  if (length(empty_cols) > 0L) {
    cli::cli_abort(
      c(
        "{.path {file}} contains empty string values.",
        "i" = "{cli::qty(length(empty_cols))}Affected column{?s}: {.col {empty_cols}}"
      ),
      class = "Huwise_error_value",
      call = call
    )
  }

  invisible(data)
}

#' Load a single wide MIV traffic count file
#'
#' Thin wrapper around \code{\link{read_miv_file}} that additionally rejects
#' \code{NA} and empty-string values.
#'
#' @inheritParams read_miv_file
#'
#' @return A \code{data.frame} of the parsed wide data; aborts if any \code{NA}
#'   or empty-string values are present.
#'
#' @noRd
#' @keywords internal
load_miv_file <- function(
  file,
  sep = "auto",
  call = rlang::caller_env()
) {
  read_miv_file(file, sep = sep, call = call) |>
    check_no_na_empty(file = file, call = call)
}

#' Load a single long-format MIV traffic count file
#'
#' Long-format counterpart to \code{\link{load_miv_file}}: reads with
#' \code{\link{read_miv_long_file}} and rejects \code{NA} and empty-string
#' values.
#'
#' @inheritParams read_miv_file
#'
#' @return A \code{data.frame} of the parsed long data; aborts if any \code{NA}
#'   or empty-string values are present.
#'
#' @noRd
#' @keywords internal
load_miv_long_file <- function(
  file,
  sep = "auto",
  call = rlang::caller_env()
) {
  read_miv_long_file(file, sep = sep, call = call) |>
    check_no_na_empty(file = file, call = call)
}

#' Load and combine all wide MIV traffic count files from a directory
#'
#' Reads every \code{.TXT} file in \code{dir} with \code{\link{read_miv_file}}
#' and row-binds them. Encoding is detected per file.
#'
#' @param dir Character scalar. Directory containing wide \code{.TXT} files.
#' @param sep Character scalar. Field separator; see \code{\link{read_miv_file}}.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A \code{data.frame} with the combined rows of every \code{.TXT} file.
#'
#' @noRd
#' @keywords internal
load_miv_dir <- function(
  dir,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_directory(dir, call = call)

  files <- list.files(
    dir,
    pattern = "\\.TXT$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_abort(
      "No {.val .TXT} files found in directory {.path {dir}}.",
      class = "Huwise_error_no_data",
      call = call
    )
  }

  purrr::map(
    files,
    \(f) read_miv_file(f, sep = sep, call = call)
  ) |>
    purrr::list_rbind()
}

#' Load and combine all long-format MIV traffic count files from a directory
#'
#' Long-format counterpart to \code{\link{load_miv_dir}}: reads every
#' \code{.TXT} file with \code{\link{read_miv_long_file}} and row-binds them.
#'
#' @inheritParams load_miv_dir
#'
#' @return A \code{data.frame} with the combined rows of every \code{.TXT} file.
#'
#' @noRd
#' @keywords internal
load_miv_long_dir <- function(
  dir,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_directory(dir, call = call)

  files <- list.files(
    dir,
    pattern = "\\.TXT$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_abort(
      "No {.val .TXT} files found in directory {.path {dir}}.",
      class = "Huwise_error_no_data",
      call = call
    )
  }

  purrr::map(
    files,
    \(f) read_miv_long_file(f, sep = sep, call = call)
  ) |>
    purrr::list_rbind()
}

#' Load the MIV join dataset (location metadata)
#'
#' Reads the static CSV of detail plans and geo-coordinates for MIV counting
#' stations.
#'
#' @param file Character scalar. Path to the join dataset CSV file.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A \code{data.frame} with character columns \code{ort_id} and
#'   \code{standort}.
#'
#' @noRd
#' @keywords internal
load_join_dataset <- function(
  file = "~/LWP/3_Publikationsreihen/OGD/Datensätze OGD/OGD Stadt St.Gallen/TBA - Tiefbauamt/MIV - Motorisierter Individualverkehr/Join_datasets/detailplane-und-geokoordinaten-miv-zahlstellen-stadt-stgallen.csv",
  call = rlang::caller_env()
) {
  check_file(file, call = call)

  load_csv_utf8(file) |>
    dplyr::select(
      ort_id = "Ort-ID",
      standort = "Standort"
    ) |>
    # keep the join key character to match derive_miv_cols()'s ort_id
    dplyr::mutate(ort_id = as.character(.data$ort_id))
}


#' Derive the shared MIV columns
#'
#' Normalises \code{datum} to ISO 8601 and derives \code{arbeitstag},
#' \code{kennzahl}, and \code{ort_richtung_id}. Applied identically to wide and
#' long input so downstream code never branches on source format. \code{datum}
#' is parsed with \code{\link{parse_miv_date}}, which accepts \code{DD.MM.YYYY}
#' or \code{YYYY-MM-DD} and aborts rather than coercing a bad value to
#' \code{NA}.
#'
#' Time-zone assumption: \code{datum} and \code{stunde} are civil-time labels in
#' the source's own clock and are passed through WITHOUT any time-zone
#' conversion — only the reformat to ISO, which does not shift the day. The
#' source clock is assumed to be a fixed-offset, DST-free 24-hour clock (the
#' fixed 24 hourly columns of the wide format admit no 23-/25-hour transition
#' days), consistent across every input AND the already-published data.
#' Consistency is what keeps keys aligned; a silent change of source zone over
#' time would misalign near-midnight keys (see \code{\link{process_miv}}). Only
#' \code{erstpublikation}/\code{letzte_aktualisierung} are absolute dates,
#' stamped in \code{Europe/Zurich} by \code{\link{miv_run_date}}.
#'
#' @param data A \code{data.frame} with character columns \code{datum} (input
#'   format \code{DD.MM.YYYY} or \code{YYYY-MM-DD}), \code{wochentag},
#'   \code{ort_id}, \code{richtung}.
#' @param call Environment. Caller, for error attribution.
#'
#' @return \code{data} with \code{datum} in ISO 8601 and the columns
#'   \code{arbeitstag}, \code{kennzahl}, \code{ort_richtung_id} added.
#'
#' @importFrom rlang .data
#' @noRd
#' @keywords internal
derive_miv_cols <- function(data, call = rlang::caller_env()) {
  data |>
    dplyr::mutate(
      datum = parse_miv_date(.data$datum, call = call),
      arbeitstag = dplyr::if_else(
        .data$wochentag %in% c("Samstag", "Sonntag"),
        "Wochenende",
        "Werktage"
      ),
      kennzahl = dplyr::if_else(
        .data$arbeitstag == "Wochenende",
        "Durchschnittlicher Tagesverkehr (DTV)",
        "Durchschnittlicher Tagesverkehr (DTV),Durchschnittlicher Werktagsverkehr (DWV)"
      ),
      ort_richtung_id = stringr::str_c(.data$ort_id, .data$richtung)
    )
}


#' Transform raw wide MIV traffic count columns
#'
#' Renames raw wide columns to snake_case and derives the shared columns via
#' \code{\link{derive_miv_cols}}. No daily total is computed: the total is not
#' part of the long published schema.
#'
#' @param miv_table A \code{data.frame} from \code{\link{load_miv_file}} or
#'   \code{\link{load_miv_dir}}.
#'
#' @return A wide \code{data.frame} with metadata columns plus the 24 hourly
#'   columns, ready for \code{\link{join_miv}} and \code{\link{pivot_miv_long}}.
#'
#' @noRd
#' @keywords internal
transform_miv <- function(miv_table) {
  miv_table |>
    dplyr::rename(
      lnr = "LNR",
      ort_id = "ORT-ID",
      bezeichnung = "BEZEICHNUNG",
      datum = "DATUM",
      wochentag = "WOCHENTAG",
      richtung = "RI"
    ) |>
    derive_miv_cols() |>
    dplyr::select(
      "lnr",
      "ort_id",
      "bezeichnung",
      "datum",
      "wochentag",
      "arbeitstag",
      "kennzahl",
      "richtung",
      "ort_richtung_id",
      dplyr::all_of(miv_schema()$hours)
    )
}

#' Transform raw long-format MIV traffic count columns
#'
#' Long-format counterpart to \code{\link{transform_miv}}. The input is already
#' one row per hour; only the shared columns are derived (via
#' \code{\link{derive_miv_cols}}), any extra input columns are dropped, and no
#' pivot is applied.
#'
#' @param miv_table A \code{data.frame} from \code{\link{load_miv_long_file}} or
#'   \code{\link{load_miv_long_dir}}.
#'
#' @return A long \code{data.frame} with the same columns as a pivoted wide
#'   input except \code{standort}, ready for \code{\link{join_miv}}.
#'
#' @noRd
#' @keywords internal
transform_miv_long <- function(miv_table) {
  miv_table |>
    derive_miv_cols() |>
    dplyr::mutate(
      stunde = as.character(.data$stunde),
      anzahl_fahrzeuge = as.character(.data$anzahl_fahrzeuge)
    ) |>
    dplyr::select(
      "lnr",
      "ort_id",
      "bezeichnung",
      "datum",
      "wochentag",
      "arbeitstag",
      "kennzahl",
      "richtung",
      "ort_richtung_id",
      "stunde",
      "anzahl_fahrzeuge"
    )
}

#' Join location metadata to transformed MIV data
#'
#' Left-joins the location metadata on \code{ort_id} and relocates
#' \code{standort} after \code{bezeichnung}. Works on both wide (pre-pivot) and
#' long data.
#'
#' @param data A \code{data.frame} from \code{\link{transform_miv}} or
#'   \code{\link{transform_miv_long}}.
#' @param join A \code{data.frame} from \code{\link{load_join_dataset}}.
#'
#' @return \code{data} with \code{standort} inserted after \code{bezeichnung}.
#'
#' @noRd
#' @keywords internal
join_miv <- function(data, join) {
  dplyr::left_join(
    x = data,
    y = join,
    by = dplyr::join_by("ort_id")
  ) |>
    dplyr::relocate(
      "standort",
      .after = "bezeichnung"
    )
}

#' Pivot wide MIV data to the long published shape
#'
#' Collapses the 24 hourly columns into \code{stunde} (hour label \code{"1"}--
#' \code{"24"}) and \code{anzahl_fahrzeuge} (that hour's count). No daily total
#' is produced. Each input row becomes exactly 24 long rows.
#'
#' @param data A wide, joined \code{data.frame} carrying the columns
#'   \code{miv_schema()$hours}.
#'
#' @return A long \code{data.frame} with \code{stunde} and
#'   \code{anzahl_fahrzeuge} (both character) in place of the hourly columns.
#'
#' @noRd
#' @keywords internal
pivot_miv_long <- function(data) {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(miv_schema()$hours),
      names_to = "stunde",
      values_to = "anzahl_fahrzeuge"
    ) |>
    dplyr::mutate(
      stunde = as.character(.data$stunde),
      anzahl_fahrzeuge = as.character(.data$anzahl_fahrzeuge)
    )
}

#' Process a single MIV traffic count file end-to-end
#'
#' Detects the file shape and normalises it to the long published schema: wide
#' input goes through the wide reader, \code{\link{transform_miv}},
#' \code{\link{join_miv}}, and \code{\link{pivot_miv_long}}; long input goes
#' through the long reader, \code{\link{transform_miv_long}}, and
#' \code{\link{join_miv}} (no pivot). The join dataset is supplied by the caller
#' so it is loaded only once.
#'
#' @param file Character scalar. Path to the \code{.TXT} file.
#' @param join A \code{data.frame} from \code{\link{load_join_dataset}}.
#' @param sep Character scalar. Field separator; see \code{\link{read_miv_file}}.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A long, joined \code{data.frame} (no tracking columns).
#'
#' @noRd
#' @keywords internal
process_miv_file <- function(
  file,
  join,
  sep = "auto",
  call = rlang::caller_env()
) {
  shape <- detect_miv_shape(file, sep = sep, call = call)

  if (shape == "wide") {
    load_miv_file(file, sep = sep, call = call) |>
      transform_miv() |>
      join_miv(join) |>
      pivot_miv_long()
  } else {
    load_miv_long_file(file, sep = sep, call = call) |>
      transform_miv_long() |>
      join_miv(join)
  }
}

#' Process all MIV traffic count files in a directory end-to-end
#'
#' Detects the shape of the directory's \code{.TXT} files (which must all share
#' one shape) and normalises them to the long published schema, mirroring
#' \code{\link{process_miv_file}} at directory scale.
#'
#' @param dir Character scalar. Directory of \code{.TXT} files.
#' @param join A \code{data.frame} from \code{\link{load_join_dataset}}.
#' @param sep Character scalar. Field separator; see \code{\link{read_miv_file}}.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A long, joined \code{data.frame} combining all files (no tracking
#'   columns).
#'
#' @noRd
#' @keywords internal
process_miv_dir <- function(
  dir,
  join,
  sep = "auto",
  call = rlang::caller_env()
) {
  check_directory(dir, call = call)

  files <- list.files(
    dir,
    pattern = "\\.TXT$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_abort(
      "No {.val .TXT} files found in directory {.path {dir}}.",
      class = "Huwise_error_no_data",
      call = call
    )
  }

  shapes <- purrr::map_chr(files, \(f) {
    detect_miv_shape(f, sep = sep, call = call)
  })
  if (dplyr::n_distinct(shapes) > 1L) {
    cli::cli_abort(
      c(
        "Directory {.path {dir}} mixes wide and long MIV files.",
        "i" = "A directory must contain files of a single shape."
      ),
      class = "Huwise_error_schema",
      call = call
    )
  }

  if (shapes[[1L]] == "wide") {
    load_miv_dir(dir, sep = sep, call = call) |>
      transform_miv() |>
      join_miv(join) |>
      pivot_miv_long()
  } else {
    load_miv_long_dir(dir, sep = sep, call = call) |>
      transform_miv_long() |>
      join_miv(join)
  }
}

#' Process one or more MIV traffic count files into the long published schema
#'
#' Detects whether \code{path} is a single \code{.TXT} file or a directory of
#' them, normalises wide and/or long input to the long published schema (parse
#' -> transform -> join -> pivot for wide input; parse -> transform -> join for
#' long input), sorts, and de-duplicates on the long key. The join dataset is
#' loaded once internally. The internal processing engine shared by
#' \code{\link{publish_miv}} and \code{\link{update_miv}}; it produces no
#' tracking columns.
#'
#' Time-zone assumption (see \code{\link{derive_miv_cols}}): \code{datum} and
#' \code{stunde} are civil-time labels in the source's own fixed-offset,
#' DST-free clock and are never time-zone-converted. The row key includes both,
#' so correct \code{\link{publish_miv}}/\code{\link{update_miv}} behaviour relies
#' on that clock being consistent across all inputs and the published data; the
#' format carries no zone signal, so the pipeline cannot detect a source-zone
#' change and a silent switch would misalign near-midnight keys.
#'
#' @param path Character scalar. A \code{.TXT} file or a directory of them.
#' @param join_file Character scalar. Path to the join dataset CSV.
#' @param sep Character scalar. Field separator; see \code{\link{read_miv_file}}.
#' @param call Environment. Caller, for error attribution.
#'
#' @return A long \code{data.frame} with the columns \code{miv_schema()$processed}
#'   (character), 24 rows per \code{c(lnr, ort_id, datum, richtung)} key, sorted
#'   by \code{bezeichnung}, descending \code{datum}, \code{richtung}, and numeric
#'   \code{stunde}.
#'
#' @noRd
#' @keywords internal
process_miv <- function(
  path,
  join_file = "~/LWP/3_Publikationsreihen/OGD/Datensätze OGD/OGD Stadt St.Gallen/TBA - Tiefbauamt/MIV - Motorisierter Individualverkehr/Join_datasets/detailplane-und-geokoordinaten-miv-zahlstellen-stadt-stgallen.csv",
  sep = "auto",
  call = rlang::caller_env()
) {
  is_dir <- dir.exists(path)
  is_file <- !is_dir && file.exists(path)
  if (!is_dir && !is_file) {
    cli::cli_abort(
      c(
        "{.arg path} is neither an existing file nor an existing directory.",
        "x" = "Path not found: {.path {path}}"
      ),
      class = "Huwise_error_path",
      call = call
    )
  }

  join_data <- load_join_dataset(join_file, call = call)

  processed <- if (is_dir) {
    process_miv_dir(path, join_data, sep = sep, call = call)
  } else {
    process_miv_file(path, join_data, sep = sep, call = call)
  }

  processed <- processed |>
    dplyr::select(dplyr::all_of(miv_schema()$processed)) |>
    dplyr::arrange(
      .data$bezeichnung,
      dplyr::desc(as.Date(.data$datum)),
      .data$richtung,
      as.integer(.data$stunde)
    )

  check_duplicates(
    data = processed,
    key_cols = miv_schema()$key
  )

  processed
}


#' Migrate published MIV data to the long schema
#'
#' Detects, by schema, whether the downloaded published data is still wide or
#' already long, and normalises \code{datum} to ISO 8601 in either case via
#' \code{\link{parse_miv_date}}, which accepts \code{YYYY-MM-DD} or
#' \code{DD.MM.YYYY} and aborts rather than coercing a bad value to \code{NA}
#' (an \code{NA} \code{datum} would collapse otherwise-distinct keys into
#' duplicates). Wide data is additionally converted with
#' \code{\link{pivot_miv_long}} (which drops the daily total) and both tracking
#' columns are stamped with \code{run_date} for every pre-existing row. Long
#' data keeps its rows; legacy long data lacking tracking columns is stamped
#' with \code{run_date}. The conversion is idempotent.
#'
#' @param published A \code{data.frame} from \code{\link{download_dataset}}.
#' @param run_date  Character scalar. ISO 8601 run date for legacy rows.
#' @param call      Environment. Caller, for error attribution.
#'
#' @return A long \code{data.frame} with the columns \code{miv_schema()$published}.
#'
#' @noRd
#' @keywords internal
migrate_published_to_long <- function(
  published,
  run_date,
  call = rlang::caller_env()
) {
  cols <- colnames(published)
  hours <- miv_schema()$hours
  tracking <- miv_schema()$tracking

  is_long <- all(c("stunde", "anzahl_fahrzeuge") %in% cols)
  is_wide <- all(hours %in% cols)

  if (is_long) {
    published$datum <- parse_miv_date(published$datum, call = call)
    if (!all(tracking %in% cols)) {
      published <- published |>
        dplyr::mutate(
          erstpublikation = run_date,
          letzte_aktualisierung = run_date
        )
    }
    return(dplyr::select(published, dplyr::all_of(miv_schema()$published)))
  }

  if (!is_wide) {
    cli::cli_abort(
      c(
        "Published MIV dataset has an unrecognised schema.",
        "i" = "Expected the wide hourly columns {.col {hours}} or the long \\
               columns {.col stunde} and {.col anzahl_fahrzeuge}."
      ),
      class = "Huwise_error_schema",
      call = call
    )
  }

  published$datum <- parse_miv_date(published$datum, call = call)

  published |>
    dplyr::select(-dplyr::any_of("tagestotal")) |>
    pivot_miv_long() |>
    dplyr::mutate(
      erstpublikation = run_date,
      letzte_aktualisierung = run_date
    ) |>
    dplyr::select(dplyr::all_of(miv_schema()$published))
}


#' Publish new MIV traffic count data (initial publish or append)
#'
#' Processes the raw data at \code{path} into the long published schema and
#' either publishes it as the initial dataset (\code{first_publish = TRUE}) or
#' appends it to the existing published dataset (\code{first_publish = FALSE}).
#' Append is for genuinely new records only: any overlap between the new keys and
#' the already-published keys is a hard error, never an upsert. Already-published
#' rows are never altered. Use \code{\link{update_miv}} to correct existing
#' values.
#'
#' On append the published dataset is downloaded and, if it is still wide,
#' migrated to long with \code{migrate_published_to_long} before binding.
#' Both tracking columns of the affected new rows are set to the run date.
#'
#' Time zone: \code{datum} and \code{stunde} are taken as civil-time labels in
#' the source's own clock and are never converted (only \code{erstpublikation}/
#' \code{letzte_aktualisierung} are stamped in \code{Europe/Zurich}). Overlap
#' detection compares these labels directly, so the source clock must be
#' consistent with the already-published data — see \code{derive_miv_cols}.
#'
#' @param path Character scalar. A \code{.TXT} file or a directory of
#'   \code{.TXT} files with the new MIV traffic counts (wide or long).
#' @param dataset_id Character scalar. Human-readable identifier of the
#'   published dataset, used to download the currently published data (passed
#'   to \code{\link{download_dataset}}). Default
#'   \code{"verkehrszahlung-miv-stadt-stgallen"}.
#' @param dataset_uid Character scalar. Technical UID of the publication target,
#'   used for the upload (passed to \code{publish_huwise}). Default
#'   \code{"da_ur11wi"}.
#' @param domain Character scalar. Publication domain for both the download and
#'   the upload. Default \code{"stadt"}.
#' @param first_publish Logical scalar. \code{TRUE} publishes \code{path} as the
#'   initial dataset and errors if a dataset is already published; \code{FALSE}
#'   (default) appends and errors if nothing is published yet.
#' @param ... Passed to \code{process_miv} (e.g. \code{join_file},
#'   \code{sep}).
#' @param call Environment. Caller, for error attribution.
#'
#' @return Invisibly, the published \code{data.frame} with the columns
#'   \code{miv_schema()$published}.
#'
#' @seealso \code{\link{update_miv}}, \code{process_miv},
#'   \code{\link{download_dataset}}
#'
#' @examples
#' \dontrun{
#' # Initial publication
#' publish_miv("data/miv_counts.TXT", first_publish = TRUE)
#'
#' # Append genuinely new records
#' publish_miv("data/new_counts/")
#' }
#'
#' @export
publish_miv <- function(
  path,
  dataset_id = "verkehrszahlung-miv-stadt-stgallen-2018-2024-test",
  dataset_uid = "da_ur11wi",
  domain = "stadt",
  first_publish = FALSE,
  ...,
  call = rlang::caller_env()
) {
  check_string(path, call = call)
  check_logical(first_publish, call = call)

  run_date <- miv_run_date()
  key <- miv_schema()$key

  # New data in long form; process_miv() also aborts on within-data duplicates.
  new_long <- process_miv(path, ..., call = call)

  existing <- tryCatch(
    download_dataset(dataset_id = dataset_id, domain = domain),
    Huwise_error_no_data = function(cnd) NULL
  )
  has_published <- !is.null(existing) && nrow(existing) > 0L

  if (first_publish) {
    if (has_published) {
      cli::cli_abort(
        c(
          "A MIV dataset is already published; refusing to overwrite it.",
          "x" = "{.arg first_publish} is {.val {TRUE}} but {nrow(existing)} row{?s} \\
                 {?is/are} already published.",
          "i" = "Use {.code publish_miv(first_publish = FALSE)} to append."
        ),
        class = c("Huwise_error_value", "Huwise_error_already_published"),
        call = call
      )
    }

    upload <- new_long |>
      dplyr::mutate(
        erstpublikation = run_date,
        letzte_aktualisierung = run_date
      ) |>
      dplyr::select(dplyr::all_of(miv_schema()$published))
  } else {
    if (!has_published) {
      cli::cli_abort(
        c(
          "Nothing is published yet, so there is nothing to append to.",
          "i" = "Use {.code publish_miv(first_publish = TRUE)} for the initial \\
                 publication."
        ),
        class = "Huwise_error_no_data",
        call = call
      )
    }

    published_long <- migrate_published_to_long(
      existing,
      run_date = run_date,
      call = call
    )

    overlap <- dplyr::semi_join(new_long, published_long, by = key)
    if (nrow(overlap) > 0L) {
      cli::cli_abort(
        c(
          "New data overlaps already-published records.",
          "x" = "{nrow(overlap)} new row{?s} share a key with published data.",
          "i" = "Append adds new records only; use {.fn update_miv} to change \\
                 existing values."
        ),
        class = c("Huwise_error_duplicate", "Huwise_error_overlap"),
        call = call
      )
    }

    new_tracked <- new_long |>
      dplyr::mutate(
        erstpublikation = run_date,
        letzte_aktualisierung = run_date
      ) |>
      dplyr::select(dplyr::all_of(miv_schema()$published))

    upload <- dplyr::bind_rows(published_long, new_tracked)
  }

  upload <- upload |>
    dplyr::arrange(
      .data$lnr,
      .data$ort_id,
      as.Date(.data$datum),
      .data$richtung,
      as.integer(.data$stunde)
    )

  check_duplicates(data = upload, key_cols = key)

  publish_huwise(
    tibble = upload,
    dataset_uid = dataset_uid,
    domain = domain,
    call = call
  )
}

#' Replace published MIV values for whole calendar years
#'
#' Corrects or replaces already-published values, scoped to the set of calendar
#' years present in the new data. Every year that appears in the new data has its
#' entire published content replaced; every other year is kept verbatim,
#' including both tracking columns. The published dataset is downloaded and, if
#' still wide, migrated to long with \code{migrate_published_to_long}
#' before the replacement.
#'
#' Every calendar year in the new data must already exist in the published
#' dataset. A year present in the new data but not yet published is rejected
#' (class \code{Huwise_error_new_year}): publishing a brand-new period is an
#' append, which is \code{\link{publish_miv}}'s job, not this function's. This
#' check is not relaxed by \code{force}. Partial overlap within an
#' already-published year — a newly installed station or an extra station-day —
#' is permitted and handled by the provenance matching below; only an entirely
#' unpublished year is blocked.
#'
#' Provenance is matched by the long key within scope: a key present in both with
#' an unchanged \code{anzahl_fahrzeuge} keeps both tracking dates; a changed
#' value keeps \code{erstpublikation} and sets \code{letzte_aktualisierung} to
#' the run date; a new key within an already-published year sets both to the run
#' date; a key previously published within scope but absent from the new data is
#' dropped.
#'
#' Two safety checks run per in-scope year, comparing the new data against the
#' published data for that same year (a station is \code{ort_richtung_id}):
#' coverage (every published \code{(ort_richtung_id, datum)} must reappear in the
#' new data) and volume (the new-data row count must be at least
#' \code{min_row_ratio} times the published row count). Either violation is a
#' hard stop by default; \code{force = TRUE} downgrades both to warnings and
#' proceeds.
#'
#' Time zone: \code{datum} and \code{stunde} are taken as civil-time labels in
#' the source's own clock and are never converted (only \code{erstpublikation}/
#' \code{letzte_aktualisierung} are stamped in \code{Europe/Zurich}). Year
#' scoping, the per-day coverage check, and provenance all compare these labels
#' directly, so the source clock must be consistent with the already-published
#' data; a silent source-zone change would misalign near-midnight keys and
#' corrupt coverage — see \code{derive_miv_cols}.
#'
#' @param path Character scalar. A \code{.TXT} file or a directory of
#'   \code{.TXT} files with the replacement MIV traffic counts (wide or long).
#' @param dataset_id Character scalar. Human-readable identifier of the
#'   published dataset, used to download the currently published data (passed
#'   to \code{\link{download_dataset}}). Default
#'   \code{"verkehrszahlung-miv-stadt-stgallen"}.
#' @param dataset_uid Character scalar. Technical UID of the publication target,
#'   used for the upload (passed to \code{publish_huwise}). Default
#'   \code{"da_ur11wi"}.
#' @param domain Character scalar. Publication domain for both the download and
#'   the upload. Default \code{"stadt"}.
#' @param force Logical scalar. \code{TRUE} downgrades both safety-check
#'   violations to warnings and continues. Default \code{FALSE}. Does not relax
#'   the whole-new-year guard.
#' @param min_row_ratio Numeric scalar in \code{[0, 1]}. Minimum ratio of new to
#'   published rows required per in-scope year. Default \code{0.9}.
#' @param ... Passed to \code{process_miv} (e.g. \code{join_file},
#'   \code{sep}).
#' @param call Environment. Caller, for error attribution.
#'
#' @return Invisibly, the published \code{data.frame} with the columns
#'   \code{miv_schema()$published}.
#'
#' @seealso \code{\link{publish_miv}}, \code{process_miv},
#'   \code{\link{download_dataset}}
#'
#' @examples
#' \dontrun{
#' # Replace all of 2024 with a corrected export; other years untouched.
#' update_miv("data/miv_2024_corrected.TXT")
#'
#' # Deliberately replace a year with a smaller partial set.
#' update_miv("data/miv_2024_partial.TXT", force = TRUE)
#' }
#'
#' @export
update_miv <- function(
  path,
  dataset_id = "verkehrszahlung-miv-stadt-stgallen",
  dataset_uid = "da_ur11wi",
  domain = "stadt",
  force = FALSE,
  min_row_ratio = 0.9,
  ...,
  call = rlang::caller_env()
) {
  check_string(path, call = call)
  check_logical(force, call = call)
  check_numeric(min_row_ratio, call = call)
  if (min_row_ratio < 0 || min_row_ratio > 1) {
    cli::cli_abort(
      "{.arg min_row_ratio} must be between {.val {0}} and {.val {1}}, \\
       not {.val {min_row_ratio}}.",
      class = "Huwise_error_value",
      call = call
    )
  }

  run_date <- miv_run_date()
  key <- miv_schema()$key

  # New data in long form; process_miv() also aborts on within-data duplicates.
  new_long <- process_miv(path, ..., call = call) |>
    dplyr::mutate(jahr = substr(.data$datum, 1L, 4L))

  existing <- tryCatch(
    download_dataset(dataset_id = dataset_id, domain = domain),
    Huwise_error_no_data = function(cnd) NULL
  )
  if (is.null(existing) || nrow(existing) == 0L) {
    cli::cli_abort(
      c(
        "Nothing is published yet, so there is nothing to update.",
        "i" = "Publish first with {.code publish_miv(first_publish = TRUE)}."
      ),
      class = "Huwise_error_no_data",
      call = call
    )
  }

  published_long <- migrate_published_to_long(
    existing,
    run_date = run_date,
    call = call
  ) |>
    dplyr::mutate(jahr = substr(.data$datum, 1L, 4L))

  scope_years <- sort(unique(new_long[["jahr"]]))

  new_years <- setdiff(scope_years, unique(published_long[["jahr"]]))
  if (length(new_years) > 0L) {
    cli::cli_abort(
      c(
        "New data covers {cli::qty(length(new_years))}calendar year{?s} not \\
         present in the published dataset.",
        "x" = "{cli::qty(length(new_years))}Unpublished year{?s} in the new \\
               data: {.val {new_years}}.",
        "i" = "{.fn update_miv} replaces values within already-published years. \\
               Use {.code publish_miv(first_publish = FALSE)} to append a new \\
               period."
      ),
      class = c("Huwise_error_value", "Huwise_error_new_year"),
      call = call
    )
  }

  in_scope <- dplyr::filter(published_long, .data$jahr %in% scope_years)
  out_scope <- dplyr::filter(published_long, !.data$jahr %in% scope_years)

  # Safety checks, per in-scope year, new vs. published for that same year.
  coverage_bad <- list()
  volume_bad <- character(0)
  for (y in scope_years) {
    new_y <- dplyr::filter(new_long, .data$jahr == y)
    pub_y <- dplyr::filter(in_scope, .data$jahr == y)

    pub_sd <- dplyr::distinct(dplyr::select(pub_y, "ort_richtung_id", "datum"))
    new_sd <- dplyr::distinct(dplyr::select(new_y, "ort_richtung_id", "datum"))
    missing <- dplyr::anti_join(
      pub_sd,
      new_sd,
      by = dplyr::join_by("ort_richtung_id", "datum")
    )
    if (nrow(missing) > 0L) {
      # Separate whole-station gaps (station absent from the new data
      # entirely) from day-level gaps (station present, only some dates gone).
      coverage_bad[[y]] <- dplyr::mutate(
        missing,
        station_absent = !.data$ort_richtung_id %in% new_sd$ort_richtung_id
      )
    }

    if (nrow(new_y) < min_row_ratio * nrow(pub_y)) {
      volume_bad[[y]] <- sprintf(
        "%s: %d new vs. %d published",
        y,
        nrow(new_y),
        nrow(pub_y)
      )
    }
  }

  if (length(coverage_bad) > 0L) {
    cov <- dplyr::bind_rows(coverage_bad, .id = "jahr")
    yrs <- unique(cov$jahr)
    n_missing <- nrow(cov)

    absent_ids <- sort(unique(cov$ort_richtung_id[cov$station_absent]))
    partial_days <- cov |>
      dplyr::filter(!.data$station_absent) |>
      dplyr::summarise(n = dplyr::n(), .by = "ort_richtung_id") |>
      dplyr::arrange(dplyr::desc(.data$n))
    partial_labels <- sprintf(
      "%s (%d)",
      partial_days$ort_richtung_id,
      partial_days$n
    )

    absent_vec <- cli::cli_vec(absent_ids, list("vec-trunc" = 10))
    partial_vec <- cli::cli_vec(partial_labels, list("vec-trunc" = 10))

    msg <- c(
      "New data does not fully cover the published station-days.",
      "x" = "{cli::qty(n_missing)}{n_missing} published station-day{?s} \\
             {?is/are} missing for {cli::qty(length(yrs))}year{?s} {.val {yrs}}."
    )
    if (length(absent_ids) > 0L) {
      msg <- c(
        msg,
        "x" = "{cli::qty(length(absent_ids))}{length(absent_ids)} station{?s} \\
               absent from the new data entirely (every published day missing): \\
               {.val {absent_vec}}."
      )
    }
    if (nrow(partial_days) > 0L) {
      msg <- c(
        msg,
        "x" = "{cli::qty(nrow(partial_days))}{nrow(partial_days)} station{?s} \\
               present but missing only some days, shown as \\
               {.field station (days missing)}: {partial_vec}."
      )
    }
    msg <- c(
      msg,
      "i" = "Every published {.code (ort_richtung_id, datum)} in an in-scope \\
             year must reappear in the new data, or set {.arg force} to override."
    )

    if (force) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(
        msg,
        class = c("Huwise_error_value", "Huwise_error_coverage"),
        call = call
      )
    }
  }

  if (length(volume_bad) > 0L) {
    yrs <- names(volume_bad)
    msg <- c(
      "New-data volume is below {.arg min_row_ratio} ({.val {min_row_ratio}}).",
      "x" = "{cli::qty(length(yrs))}Year{?s} below the ratio: {volume_bad}.",
      "i" = "Set {.arg force} to replace with a smaller set anyway."
    )
    if (force) {
      cli::cli_warn(msg)
    } else {
      cli::cli_abort(
        msg,
        class = c("Huwise_error_value", "Huwise_error_volume"),
        call = call
      )
    }
  }

  # Provenance within scope, matched by the long key.
  prov <- in_scope |>
    dplyr::select(
      dplyr::all_of(key),
      old_anzahl = "anzahl_fahrzeuge",
      old_erst = "erstpublikation",
      old_letzte = "letzte_aktualisierung"
    )

  updated_scope <- new_long |>
    dplyr::select(-dplyr::any_of("jahr")) |>
    dplyr::left_join(prov, by = key) |>
    dplyr::mutate(
      erstpublikation = dplyr::if_else(
        is.na(.data$old_erst),
        run_date,
        .data$old_erst
      ),
      letzte_aktualisierung = dplyr::case_when(
        is.na(.data$old_anzahl) ~ run_date,
        .data$old_anzahl != .data$anzahl_fahrzeuge ~ run_date,
        .default = .data$old_letzte
      )
    ) |>
    dplyr::select(dplyr::all_of(miv_schema()$published))

  upload <- dplyr::bind_rows(
    dplyr::select(out_scope, dplyr::all_of(miv_schema()$published)),
    updated_scope
  ) |>
    dplyr::arrange(
      .data$lnr,
      .data$ort_id,
      as.Date(.data$datum),
      .data$richtung,
      as.integer(.data$stunde)
    )

  check_duplicates(data = upload, key_cols = key)

  publish_huwise(
    tibble = upload,
    dataset_uid = dataset_uid,
    domain = domain,
    call = call
  )
}
