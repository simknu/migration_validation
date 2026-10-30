# =============================================================================
# UTF-8 import/export pipeline — full corrected implementation
#
# One function per file in R/ (split on the section markers); shown here as a
# single file for convenience. Shared validators (check_file, check_string,
# check_logical, check_data_frame, check_list, check_one_of,
# check_character_vector, check_integer, check_length, check_xlsx_path) live
# in R/00_input_check_fct.R and are NOT reprinted. Condition classes use the
# package's "Huwise_error_type" vocabulary so callers can tryCatch() them.
#
# Packages used (add to Imports): cli, logger, tibble, dplyr, readr, purrr,
# stringi, digest, data.table, openxlsx2, rlang, tools, utils.
# =============================================================================

# =============================================================================
# SECTION 0 – Configuration & Logger Initialisation ----
# =============================================================================

#' Build a CSV-export pipeline configuration object
#'
#' Pure-value constructor; performs no side effects. Call [init_logger()]
#' separately to apply logging settings.
#'
#' @param strict_encoding Logical. `TRUE` aborts on undecodable bytes;
#'   `FALSE` substitutes U+FFFD and records the affected rows. Default `TRUE`.
#' @param dry_run Logical. Run all checks but skip file writes. Default
#'   `FALSE`.
#' @param log_path Character scalar or `NULL`. Tee-log file path.
#' @param log_level `logger` threshold. Default `logger::INFO`.
#' @param error_sample_path Character scalar. Path for the problem-row CSV.
#'   Default a file in `tempdir()`.
#' @param unicode_form One of `"NFC"`, `"NFD"`, `"NFKC"`, `"NFKD"`. Default
#'   `"NFC"`.
#' @param write_bom Logical. Prepend a UTF-8 BOM (`0xEF 0xBB 0xBF`), which
#'   Excel-on-Windows needs to open UTF-8 CSVs correctly. Default `FALSE`.
#' @param na_string Character scalar. Written for `NA` values. May be `""`.
#'   Default `""`.
#' @param quote_all Logical. Force-quote every field. Default `FALSE`.
#' @param eol Character scalar. Line ending: `\\n` (LF, default) or
#'   `\\r\\n` (CRLF).
#' @param max_retry Integer >= 0. Reserved for upload retry logic. Default 2.
#'
#' @return A named list of configuration values (pure value; no side
#'   effects), with elements matching the arguments above.
#' @family utf8_io
#' @seealso [init_logger()], [write_utf8_csv()]
#' @export
#'
#' @examples
#' cfg <- csv_export_config(strict_encoding = FALSE, write_bom = TRUE)
#' init_logger(cfg)
csv_export_config <- function(
  strict_encoding = TRUE,
  dry_run = FALSE,
  log_path = NULL,
  log_level = logger::INFO,
  error_sample_path = file.path(
    tempdir(),
    "encoding_errors.csv"
  ),
  unicode_form = c("NFC", "NFD", "NFKC", "NFKD"),
  write_bom = FALSE,
  na_string = "",
  quote_all = FALSE,
  eol = "\n",
  max_retry = 2L
) {
  unicode_form <- match.arg(unicode_form)
  max_retry <- suppressWarnings(as.integer(max_retry))

  check_logical(strict_encoding)
  check_logical(dry_run)
  check_logical(write_bom)
  check_logical(quote_all)
  if (!is.null(log_path)) {
    check_string(log_path)
  }
  check_string(error_sample_path)
  # na_string may legitimately be "", so validate as a length-1 char vector
  # rather than a non-empty string.
  check_character_vector(na_string)
  check_length(na_string, 1L)
  check_one_of(eol, c("\n", "\r\n"))
  check_integer(max_retry)

  list(
    strict_encoding = strict_encoding,
    dry_run = dry_run,
    log_path = log_path,
    log_level = log_level,
    error_sample_path = error_sample_path,
    unicode_form = unicode_form,
    write_bom = write_bom,
    na_string = na_string,
    quote_all = quote_all,
    eol = eol,
    max_retry = max_retry
  )
}


#' Apply logging settings from a csv_export_config object
#'
#' Kept separate from [csv_export_config()] so the constructor stays free of
#' global side effects. Call once after building a config.
#'
#' @param cfg A list produced by [csv_export_config()].
#'
#' @return Invisibly `NULL`.
#' @family utf8_io
#' @seealso [csv_export_config()]
#' @export
#'
#' @examples
#' cfg <- csv_export_config(log_path = tempfile(fileext = ".log"))
#' init_logger(cfg)
init_logger <- function(cfg) {
  check_list(cfg)
  logger::log_threshold(cfg$log_level)
  if (!is.null(cfg$log_path)) {
    logger::log_appender(logger::appender_tee(cfg$log_path))
  } else {
    logger::log_appender(logger::appender_console)
  }
  invisible(NULL)
}


# =============================================================================
# SECTION 1 – Encoding Detection ----
# =============================================================================

#' Detect the most probable character encoding of a file
#'
#' Reads up to `sample_bytes` raw bytes. If those bytes are already valid
#' UTF-8, the encoding is confirmed as UTF-8 without consulting the statistical
#' detector; otherwise they are passed to [stringi::stri_enc_detect()]. XLSX
#' files always report `"UTF-8"` because the Office Open XML string store is
#' UTF-8.
#'
#' @param path Character scalar. Path to the file.
#' @param sample_bytes Integer. Bytes to sample. Default `65536L` (64 KB).
#'
#' @return A tibble with columns `encoding`, `confidence`, `language`,
#'   sorted by confidence descending; row 1 is the best guess.
#' @family utf8_io
#' @export
#'
#' @examples
#' \dontrun{
#' enc <- detect_file_encoding("data/legacy.csv")
#' enc$encoding[1]
#' }
detect_file_encoding <- function(path, sample_bytes = 65536L) {
  check_file(path)
  check_integer(sample_bytes)

  ext <- tolower(tools::file_ext(path))
  if (ext == "xlsx") {
    logger::log_info("XLSX detected - string store is UTF-8 (openxlsx2).")
    return(tibble::tibble(
      encoding = "UTF-8",
      confidence = 1.0,
      language = NA_character_
    ))
  }

  raw_bytes <- readBin(path, what = "raw", n = sample_bytes)
  if (length(raw_bytes) == 0L) {
    cli::cli_abort(
      "File is empty: {.path {path}}.",
      class = "Huwise_error_empty_file"
    )
  }

  # Positive confirmation beats the statistical detector. If the sampled bytes
  # are valid UTF-8 (ASCII is a subset), the file *is* UTF-8 and there is
  # nothing to guess. stri_enc_detect() reports low confidence for mostly-ASCII
  # text carrying only a few multibyte characters, which otherwise triggers a
  # misleading "supply the encoding manually" warning and, worse, can hand a
  # wrong Latin-1 guess to convert_to_utf8() that double-decodes the very
  # umlauts we mean to preserve. A multibyte sequence split at the sample_bytes
  # boundary can only cause a false negative here (fall through to detection),
  # never a false positive, so this is safe on a truncated sample.
  if (isTRUE(stringi::stri_enc_isutf8(raw_bytes))) {
    logger::log_info(
      "Encoding confirmed UTF-8 by validation for [{basename(path)}]."
    )
    return(tibble::tibble(
      encoding = "UTF-8",
      confidence = 1.0,
      language = NA_character_
    ))
  }

  result <- stringi::stri_enc_detect(raw_bytes)[[1L]]
  result <- result[order(-result$Confidence), ]
  result <- tibble::tibble(
    encoding = result$Encoding,
    confidence = result$Confidence,
    language = result$Language
  )

  logger::log_info(
    "Encoding detection [{basename(path)}]: ",
    "top = {result$encoding[1]} (confidence {round(result$confidence[1], 3)})"
  )
  if (result$confidence[1L] < 0.5) {
    logger::log_warn(
      "Low confidence ({round(result$confidence[1], 2)}) for ",
      "[{basename(path)}]. Consider supplying the encoding manually."
    )
  }
  result
}


# =============================================================================
# SECTION 2 – Encoding Conversion, Normalisation & Integrity ----
# =============================================================================

#' Convert a character vector to valid UTF-8 (whole-vector, file-level)
#'
#' Encoding is a property of the file, not the individual cell, so the entire
#' vector is decoded from `from_encoding` in one operation. The previous
#' implementation gated conversion per cell on `stri_enc_isutf8()`, which left
#' any value whose bytes coincidentally formed valid UTF-8 untouched and thus
#' mojibake. When `from_encoding` is UTF-8 the vector is validated (not
#' re-decoded); otherwise it is decoded from the named charset.
#'
#' @param x Character vector (or non-character, returned unchanged).
#' @param from_encoding Non-empty character scalar. Source encoding label,
#'   e.g. `"ISO-8859-1"`, `"Windows-1252"`, `"UTF-8"`.
#' @param strict Logical. Abort on undecodable bytes (`TRUE`) or substitute
#'   U+FFFD (`FALSE`).
#' @param col_name Character scalar. Column label used in messages.
#' @param call The calling environment, for error reporting.
#'
#' @return A named list: `result` (UTF-8 character vector), `problem_indices`
#'   (integer positions substituted), `n_substitutions` (integer count).
#' @noRd
#' @keywords internal
convert_to_utf8 <- function(
  x,
  from_encoding,
  strict = TRUE,
  col_name = "unknown",
  call = rlang::caller_env()
) {
  check_string(from_encoding, call = call)
  check_string(col_name, call = call)
  check_logical(strict, call = call)

  if (!is.character(x)) {
    return(list(
      result = x,
      problem_indices = integer(0L),
      n_substitutions = 0L
    ))
  }

  from_is_utf8 <- toupper(from_encoding) %in% c("UTF-8", "UTF8")

  converted <- if (from_is_utf8) {
    stringi::stri_enc_toutf8(x) # relabel/keep bytes; NA-safe
  } else {
    stringi::stri_encode(x, from = from_encoding, to = "UTF-8") # whole-vector decode
  }

  invalid <- !stringi::stri_enc_isutf8(converted) & !is.na(converted)
  if (!any(invalid)) {
    return(list(
      result = converted,
      problem_indices = integer(0L),
      n_substitutions = 0L
    ))
  }

  bad_rows <- which(invalid)
  if (strict) {
    cli::cli_abort(
      c(
        "Column {.val {col_name}}: {cli::qty(length(bad_rows))}{length(bad_rows)} \\
         value{?s} could not be decoded as valid UTF-8 from \\
         {.val {from_encoding}}.",
        "i" = "First offending row{cli::qty(length(bad_rows))}{?s}: \\
               {.val {utils::head(bad_rows, 10L)}}.",
        "i" = "Set {.arg strict_encoding = FALSE} to substitute U+FFFD and \\
               continue."
      ),
      class = "Huwise_error_encoding",
      call = call
    )
  }

  # Non-strict: substitute only the undecodable subsequences, keeping the
  # valid parts of each affected string (validate = TRUE inserts U+FFFD).
  converted <- withCallingHandlers(
    stringi::stri_enc_toutf8(converted, validate = TRUE),
    warning = function(w) {
      if (grepl("invalid UTF-8", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  logger::log_warn(
    "Column '{col_name}': {length(bad_rows)} value(s) had bytes replaced ",
    "with U+FFFD. Rows: {paste(utils::head(bad_rows, 20L), collapse = ', ')}"
  )

  list(
    result = converted,
    problem_indices = bad_rows,
    n_substitutions = length(bad_rows)
  )
}


#' NFC-normalise character columns without mutating content
#'
#' `stringi::stri_trans_nfc()` is NA-safe and encoding-aware, so no manual
#' byte surgery is needed. The previous version round-tripped each cell
#' through `charToRaw()`/`rawToChar()` to strip NUL bytes; that silently
#' turned every `NA` into the literal string `"NA"` and dropped the UTF-8
#' encoding mark from every value. An R character string cannot even hold an
#' embedded NUL (`rawToChar()` errors on one), so the NUL-strip guarded an
#' impossible input while corrupting real data.
#'
#' @param df A data frame or tibble.
#' @param unicode_form One of `"NFC"`, `"NFD"`, `"NFKC"`, `"NFKD"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `df` with every character column NFC-normalised and marked UTF-8;
#'   `NA` preserved as `NA`.
#' @noRd
#' @keywords internal
normalize_unicode <- function(
  df,
  unicode_form = "NFC",
  call = rlang::caller_env()
) {
  check_data_frame(df, call = call)
  check_one_of(unicode_form, c("NFC", "NFD", "NFKC", "NFKD"), call = call)

  norm_fn <- switch(
    unicode_form,
    NFC = stringi::stri_trans_nfc,
    NFD = stringi::stri_trans_nfd,
    NFKC = stringi::stri_trans_nfkc,
    NFKD = stringi::stri_trans_nfkd
  )

  char_cols <- names(df)[vapply(df, is.character, logical(1L))]
  for (col in char_cols) {
    df[[col]] <- norm_fn(stringi::stri_enc_toutf8(df[[col]], validate = TRUE))
  }

  logger::log_debug(
    "Unicode normalisation ({unicode_form}) applied to ",
    "{length(char_cols)} character column(s)."
  )
  df
}


#' Assemble a problem-row report and write a sample to disk
#'
#' Shared by the import fixer and the pre-write revalidator so the two paths
#' cannot drift.
#'
#' @param problem_rows A named list of per-column problem tibbles (may be
#'   empty).
#' @param cfg A [csv_export_config()] list.
#' @param stage Character scalar. `"import"` or `"pre-write"`, for messages.
#' @param call The calling environment, for error reporting.
#'
#' @return A tibble with columns `column`, `row_index`, `original_value`
#'   (0-row when clean).
#' @noRd
#' @keywords internal
build_problem_report <- function(
  problem_rows,
  cfg,
  stage = "import",
  call = rlang::caller_env()
) {
  check_list(problem_rows, call = call)
  check_list(cfg, call = call)

  if (length(problem_rows) == 0L) {
    return(tibble::tibble(
      column = character(),
      row_index = integer(),
      original_value = character()
    ))
  }

  rpt <- dplyr::bind_rows(problem_rows)
  logger::log_warn(
    "{nrow(rpt)} encoding problem(s) [{stage}]. ",
    "Writing sample -> {cfg$error_sample_path}"
  )
  tryCatch(
    readr::write_csv(rpt, cfg$error_sample_path),
    error = function(e) {
      logger::log_error("Could not write error sample: {conditionMessage(e)}")
    }
  )
  rpt
}


#' Convert every character column to UTF-8 and collect a problem report
#'
#' Captures each column's original values *before* conversion, so the report
#' shows the offending input (rendered UTF-8-safe with U+FFFD where the bytes
#' were undecodable) rather than the post-substitution result.
#'
#' @param df A data frame or tibble.
#' @param from_enc Character scalar. Expected source encoding.
#' @param cfg A [csv_export_config()] list.
#' @param call The calling environment, for error reporting.
#'
#' @return A named list: `df` (corrected tibble/data frame) and `report`
#'   (tibble of problem rows; 0-row if clean).
#' @noRd
#' @keywords internal
apply_encoding_fixes <- function(
  df,
  from_enc,
  cfg,
  call = rlang::caller_env()
) {
  check_data_frame(df, call = call)
  check_string(from_enc, call = call)
  check_list(cfg, call = call)

  problem_rows <- list()
  char_cols <- names(df)[vapply(df, is.character, logical(1L))]

  for (col in char_cols) {
    original <- df[[col]] # captured BEFORE conversion
    res <- convert_to_utf8(
      original,
      from_encoding = from_enc,
      strict = cfg$strict_encoding,
      col_name = col,
      call = call
    )
    df[[col]] <- res$result

    if (length(res$problem_indices) > 0L) {
      problem_rows[[col]] <- tibble::tibble(
        column = col,
        row_index = res$problem_indices,
        # UTF-8-safe rendering of the original so write_csv() cannot choke on
        # the very bytes we are reporting; U+FFFD marks the undecodable spans.
        original_value = stringi::stri_enc_toutf8(
          original[res$problem_indices],
          validate = TRUE
        )
      )
    }
  }

  list(
    df = df,
    report = build_problem_report(
      problem_rows,
      cfg,
      stage = "import",
      call = call
    )
  )
}


#' Re-validate UTF-8 on all character columns immediately before writing
#'
#' Final guard before disk I/O. Returns the corrected data frame and a
#' problem report rather than mutating the caller's environment.
#'
#' @param df A data frame or tibble.
#' @param cfg A [csv_export_config()] list.
#' @param call The calling environment, for error reporting.
#'
#' @return A named list: `df` (corrected) and `report` (tibble of problem
#'   rows; 0-row if clean).
#' @noRd
#' @keywords internal
revalidate_utf8_columns <- function(df, cfg, call = rlang::caller_env()) {
  check_data_frame(df, call = call)
  check_list(cfg, call = call)

  problem_rows <- list()
  char_cols <- names(df)[vapply(df, is.character, logical(1L))]

  for (col in char_cols) {
    original <- df[[col]]
    res <- convert_to_utf8(
      original,
      from_encoding = "UTF-8",
      strict = cfg$strict_encoding,
      col_name = col,
      call = call
    )
    df[[col]] <- res$result

    if (length(res$problem_indices) > 0L) {
      problem_rows[[col]] <- tibble::tibble(
        column = col,
        row_index = res$problem_indices,
        original_value = stringi::stri_enc_toutf8(
          original[res$problem_indices],
          validate = TRUE
        )
      )
    }
  }

  list(
    df = df,
    report = build_problem_report(
      problem_rows,
      cfg,
      stage = "pre-write",
      call = call
    )
  )
}


#' Order- and NA-sensitive SHA-256 fingerprint of character columns
#'
#' Detects content change across a read/write round-trip. Unlike a
#' sort-then-hash, this preserves row order and encodes the NA pattern, so
#' both row reordering and a change in NA count alter the hash. (`sort()`
#' drops `NA` and discards order, hiding exactly those two failure modes.)
#'
#' @param df A data frame or tibble.
#' @param call The calling environment, for error reporting.
#'
#' @return A tibble with columns `column` and `sha256`; one row per character
#'   column, 0-row if none.
#' @noRd
#' @keywords internal
utf8_fingerprint <- function(df, call = rlang::caller_env()) {
  check_data_frame(df, call = call)

  char_cols <- names(df)[vapply(df, is.character, logical(1L))]
  if (length(char_cols) == 0L) {
    return(tibble::tibble(column = character(), sha256 = character()))
  }

  fp <- vapply(
    char_cols,
    function(col) {
      x <- df[[col]]
      # \u0001 is a control char normalised text never contains, so the NA
      # sentinel cannot collide with data; digest() preserves order/length.
      # enc2utf8() removes the encoding-mark dependency in R's serialize():
      # identical bytes marked "latin1"/"unknown" vs "UTF-8" would otherwise
      # hash differently. Fingerprint the content, not the Encoding() flag.
      keyed <- ifelse(is.na(x), "\u0001NA\u0001", enc2utf8(x))
      digest::digest(keyed, algo = "sha256")
    },
    character(1L),
    USE.NAMES = FALSE
  )

  tibble::tibble(column = char_cols, sha256 = fp)
}


#' Assert that every character column is valid, UTF-8-encoded text
#'
#' Post-condition guard for the pipeline. Silent on success by design, so it
#' can back a "quiet unless something broke" mode. On failure it either
#' aborts (`loud = TRUE`) or warns and returns the offending locations
#' invisibly (`loud = FALSE`).
#'
#' @param df A data frame or tibble.
#' @param stage Character scalar. Context label, e.g. `"import"`/`"export"`.
#' @param loud Logical. Abort (`TRUE`) or warn (`FALSE`) on failure.
#' @param call The calling environment, for error reporting.
#'
#' @return Invisibly, a tibble of offending `column`/`row_index` (0-row when
#'   every value is valid UTF-8).
#' @noRd
#' @keywords internal
assert_utf8 <- function(
  df,
  stage = "import",
  loud = TRUE,
  call = rlang::caller_env()
) {
  check_data_frame(df, call = call)
  check_string(stage, call = call)
  check_logical(loud, call = call)

  char_cols <- names(df)[vapply(df, is.character, logical(1L))]
  bad <- purrr::map(char_cols, function(col) {
    ok <- stringi::stri_enc_isutf8(df[[col]])
    idx <- which(!ok & !is.na(df[[col]]))
    if (length(idx) == 0L) {
      NULL
    } else {
      tibble::tibble(column = col, row_index = idx)
    }
  }) |>
    purrr::list_rbind()

  if (nrow(bad) > 0L) {
    cols <- unique(bad$column)
    msg <- c(
      "{stage}: {cli::qty(nrow(bad))}{nrow(bad)} value{?s} not valid UTF-8.",
      "i" = "Affected column{cli::qty(length(cols))}{?s}: {.val {cols}}."
    )
    if (loud) {
      cli::cli_abort(msg, class = "Huwise_error_encoding", call = call)
    } else {
      cli::cli_warn(msg, class = "Huwise_warning_encoding")
    }
  }
  invisible(bad)
}


# =============================================================================
# SECTION 3 – File Readers ----
# =============================================================================

#' Read a delimited text file and return a UTF-8 tibble
#'
#' Uses [data.table::fread()] with automatic separator detection. All columns
#' are imported as character so type inference cannot silently coerce values
#' to `NA`. The detected (or supplied) encoding is then applied whole-column
#' by the pipeline, and NFC normalisation follows.
#'
#' @param path Character scalar. Path to the delimited text file.
#' @param cfg A [csv_export_config()] list.
#' @param encoding Character scalar or `NULL`. Override the detected source
#'   encoding. `NULL` (default) uses the encoding inferred from raw bytes.
#'   Supply this when detection is unreliable (e.g. mostly-ASCII files with a
#'   few high bytes).
#' @param ... Extra arguments forwarded to [data.table::fread()] (e.g.
#'   `skip`, `nrows`). Do not pass `colClasses`, `encoding`, `sep`, or
#'   `data.table` — these are set internally.
#'
#' @return A named list: `data` (tibble of UTF-8 character columns),
#'   `detected_encoding` (character), `problem_report` (tibble; 0-row if
#'   clean).
#' @family utf8_io
#' @seealso [read_xlsx_utf8()], [read_txt_utf8()], [load_csv_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- csv_export_config(strict_encoding = FALSE)
#' init_logger(cfg)
#' res <- read_csv_utf8("data/legacy.csv", cfg)
#' head(res$data)
#' }
read_csv_utf8 <- function(
  path,
  cfg = csv_export_config(),
  encoding = NULL,
  ...
) {
  check_file(path)
  check_list(cfg)
  if (!is.null(encoding)) {
    check_string(encoding)
  }

  logger::log_info("Reading CSV: {path}")

  enc_df <- detect_file_encoding(path)
  best_enc <- if (is.null(encoding)) enc_df$encoding[1L] else encoding

  read_res <- tryCatch(
    tibble::as_tibble(
      data.table::fread(
        file = path,
        colClasses = "character",
        encoding = "unknown", # bytes read as-is; decoded whole-column below
        sep = "auto",
        header = TRUE,
        showProgress = FALSE,
        data.table = FALSE,
        ...
      )
    ),
    error = function(e) e
  )
  if (inherits(read_res, "error")) {
    cli::cli_abort(
      c(
        "Failed to read {.path {path}}.",
        "x" = "{conditionMessage(read_res)}",
        "i" = "Check that the file is a valid delimited text file \\
               (CSV, TSV, semicolon-separated, etc.)."
      ),
      class = "Huwise_error_io",
      parent = read_res
    )
  }

  fix_res <- apply_encoding_fixes(read_res, best_enc, cfg)
  df <- normalize_unicode(fix_res$df, cfg$unicode_form)

  logger::log_info(
    "CSV loaded (separator auto-detected): {nrow(df)} rows x {ncol(df)} cols. ",
    "Source encoding: {best_enc}."
  )
  list(data = df, detected_encoding = best_enc, problem_report = fix_res$report)
}


#' Read an XLSX file and return a UTF-8 tibble
#'
#' Reads via [openxlsx2::wb_to_df()] (the package's mandated Excel backend);
#' the Office Open XML string store is UTF-8, so this validates and normalises
#' rather than converts.
#'
#' @param path Character scalar. Path to the `.xlsx` file.
#' @param sheet Integer or character. Sheet index or name. Default 1.
#' @param cfg A [csv_export_config()] list.
#' @param ... Extra arguments forwarded to [openxlsx2::wb_to_df()] (e.g.
#'   `start_row`, `na.strings`, `detect_dates`).
#'
#' @return A named list: `data` (tibble), `detected_encoding` (`"UTF-8"`),
#'   `problem_report` (tibble; 0-row if clean).
#' @family utf8_io
#' @seealso [read_csv_utf8()], [read_txt_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- csv_export_config()
#' init_logger(cfg)
#' res <- read_xlsx_utf8("data/report.xlsx", sheet = "Q1", cfg = cfg)
#' head(res$data)
#' }
read_xlsx_utf8 <- function(path, sheet = 1, cfg = csv_export_config(), ...) {
  check_xlsx_path(path)
  check_file(path)
  check_list(cfg)

  logger::log_info("Reading XLSX: {path}, sheet: {sheet}")

  read_res <- tryCatch(
    tibble::as_tibble(
      openxlsx2::wb_to_df(file = path, sheet = sheet, ...)
    ),
    error = function(e) e
  )
  if (inherits(read_res, "error")) {
    cli::cli_abort(
      c(
        "Failed to read XLSX {.path {path}} (sheet {.val {sheet}}).",
        "x" = "{conditionMessage(read_res)}",
        "i" = "Check the path, the sheet name/index, and read permissions."
      ),
      class = "Huwise_error_io",
      parent = read_res
    )
  }

  fix_res <- apply_encoding_fixes(read_res, "UTF-8", cfg)
  df <- normalize_unicode(fix_res$df, cfg$unicode_form)

  logger::log_info("XLSX loaded: {nrow(df)} rows x {ncol(df)} cols.")
  list(data = df, detected_encoding = "UTF-8", problem_report = fix_res$report)
}


# =============================================================================
# SECTION 4 – UTF-8 CSV Writer ----
# =============================================================================

#' Write a cleaned tibble/data frame to a UTF-8-encoded CSV file
#'
#' Guarantees UTF-8 output, re-validated encoding, a post-write header
#' read-back, and a SHA-256 file hash. In dry-run mode all checks run but no
#' file is created. The strong source-vs-file round-trip check lives in
#' [verify_csv_roundtrip()], which [export_csv_utf8()] runs after this.
#'
#' @param df A data frame or tibble (already cleaned by the caller).
#' @param out_path Character scalar or `NULL`. Destination path; `NULL`
#'   generates a timestamped temp file.
#' @param cfg A [csv_export_config()] list.
#'
#' @return A named list: `path`, `rows`, `cols`, `file_sha256`,
#'   `col_checksums` (tibble of `column`/`sha256`), `problem_report`,
#'   `dry_run`.
#' @family utf8_io
#' @seealso [verify_csv_roundtrip()], [validate_csv_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- csv_export_config()
#' df <- tibble::tibble(name = c("M\u00fcller", "\u00c5ngstr\u00f6m"))
#' out <- write_utf8_csv(df, out_path = tempfile(fileext = ".csv"), cfg = cfg)
#' out$path
#' }
write_utf8_csv <- function(df, out_path = NULL, cfg = csv_export_config()) {
  check_data_frame(df)
  check_list(cfg)
  if (!is.null(out_path)) {
    check_string(out_path)
  }

  reval_res <- revalidate_utf8_columns(df, cfg)
  df <- normalize_unicode(reval_res$df, cfg$unicode_form)
  problem_report <- reval_res$report

  if (cfg$dry_run) {
    logger::log_info(
      "[DRY RUN] Would write {nrow(df)} rows x {ncol(df)} cols. No file created."
    )
    return(list(
      path = NA_character_,
      rows = nrow(df),
      cols = ncol(df),
      file_sha256 = NA_character_,
      col_checksums = utf8_fingerprint(df),
      problem_report = problem_report,
      dry_run = TRUE
    ))
  }

  if (is.null(out_path)) {
    out_path <- tempfile(
      pattern = paste0("export_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"),
      fileext = ".csv"
    )
    logger::log_info("No out_path supplied; using temp file: {out_path}")
  }

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    logger::log_info("Created output directory: {out_dir}")
  }
  out_path <- normalizePath(out_path, mustWork = FALSE)

  write_res <- tryCatch(
    {
      if (cfg$write_bom) {
        bom_con <- file(out_path, open = "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), bom_con)
        close(bom_con)
        readr::write_csv(
          df,
          file = out_path,
          na = cfg$na_string,
          append = TRUE,
          col_names = TRUE,
          quote = if (cfg$quote_all) "all" else "needed",
          escape = "double",
          eol = cfg$eol
        )
      } else {
        readr::write_csv(
          df,
          file = out_path,
          na = cfg$na_string,
          append = FALSE,
          quote = if (cfg$quote_all) "all" else "needed",
          escape = "double",
          eol = cfg$eol
        )
      }
      TRUE
    },
    error = function(e) e
  )
  if (inherits(write_res, "error")) {
    cli::cli_abort(
      c(
        "Failed to write CSV to {.path {out_path}}.",
        "x" = "{conditionMessage(write_res)}",
        "i" = "Check disk space and write permissions."
      ),
      class = "Huwise_error_io",
      parent = write_res
    )
  }

  file_hash <- digest::digest(file = out_path, algo = "sha256")
  col_sums <- utf8_fingerprint(df)

  peek <- tryCatch(
    {
      p <- readr::read_csv(
        out_path,
        n_max = 5L,
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      )
      list(ok = TRUE, cols_match = identical(names(p), names(df)))
    },
    error = function(e) list(ok = FALSE, cols_match = FALSE)
  )

  if (!peek$ok) {
    logger::log_error(
      "Post-write re-read of '{out_path}' failed. File may be corrupted."
    )
  } else if (!peek$cols_match) {
    logger::log_warn(
      "Post-write header mismatch - possible corruption in column names."
    )
  } else {
    logger::log_info("Post-write re-read OK. Header columns match.")
  }

  sz_kb <- round(file.size(out_path) / 1024, 1L)
  logger::log_info(
    "CSV written: '{out_path}' | {nrow(df)} rows x {ncol(df)} cols | ",
    "{sz_kb} KB | SHA-256: {file_hash}"
  )

  list(
    path = out_path,
    rows = nrow(df),
    cols = ncol(df),
    file_sha256 = file_hash,
    col_checksums = col_sums,
    problem_report = problem_report,
    dry_run = FALSE
  )
}


# =============================================================================
# SECTION 5 – Post-Export Validation ----
# =============================================================================

#' Verify a written CSV round-trips to its source without content change
#'
#' Re-reads the file just written and compares an order- and NA-sensitive
#' fingerprint of every character column against the in-memory source. The
#' comparison is on the *written representation*: source `NA` is mapped to
#' `cfg$na_string` and the read-back uses `na = character()`, so it checks
#' "did the bytes round-trip", side-stepping the inherent CSV ambiguity that
#' `NA` and an empty string serialise identically. Silent on success.
#'
#' @param source_df The cleaned tibble that was written.
#' @param path Character scalar. Path to the written CSV.
#' @param cfg A [csv_export_config()] list.
#' @param loud Logical. Abort (`TRUE`) or warn (`FALSE`) on mismatch.
#' @param call The calling environment, for error reporting.
#'
#' @return Invisibly, a tibble of mismatched `column`/`src_sha256`/
#'   `file_sha256` (0-row when the round-trip is clean).
#' @noRd
#' @keywords internal
verify_csv_roundtrip <- function(
  source_df,
  path,
  cfg,
  loud = TRUE,
  call = rlang::caller_env()
) {
  check_data_frame(source_df, call = call)
  check_file(path, call = call)
  check_list(cfg, call = call)
  check_logical(loud, call = call)

  back <- tryCatch(
    readr::read_csv(
      path,
      col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = "UTF-8"),
      na = character(),
      trim_ws = FALSE,
      show_col_types = FALSE
    ),
    error = function(e) e
  )
  if (inherits(back, "error")) {
    cli::cli_abort(
      c(
        "Could not re-read exported file {.path {path}}.",
        "x" = "{conditionMessage(back)}"
      ),
      class = "Huwise_error_io",
      call = call,
      parent = back
    )
  }

  assert_utf8(back, stage = "export", loud = loud, call = call)

  char_cols <- names(source_df)[vapply(source_df, is.character, logical(1L))]
  src_repr <- source_df[char_cols]
  src_repr[] <- lapply(src_repr, function(x) ifelse(is.na(x), cfg$na_string, x))

  src_fp <- utf8_fingerprint(src_repr, call = call)
  file_fp <- utf8_fingerprint(
    back[intersect(char_cols, names(back))],
    call = call
  )

  cmp <- dplyr::full_join(
    dplyr::rename(src_fp, src_sha256 = "sha256"),
    dplyr::rename(file_fp, file_sha256 = "sha256"),
    by = dplyr::join_by("column")
  )
  mismatch <- dplyr::filter(
    cmp,
    is.na(.data$src_sha256) |
      is.na(.data$file_sha256) |
      .data$src_sha256 != .data$file_sha256
  )

  if (nrow(mismatch) > 0L) {
    cols <- mismatch$column
    msg <- c(
      "Export changed content: {cli::qty(length(cols))}column{?s} \\
       {.val {cols}} do not match between source and written file.",
      "i" = "Round-trip integrity check failed; the CSV does not match the \\
             data you passed in."
    )
    if (loud) {
      cli::cli_abort(msg, class = "Huwise_error_roundtrip", call = call)
    } else {
      cli::cli_warn(msg, class = "Huwise_warning_roundtrip")
    }
  }
  invisible(mismatch)
}


#' Validate a written UTF-8 CSV against the source data
#'
#' Reads the file back and checks: row count, column names, per-column
#' order/NA-sensitive checksums, and file-level encoding. Checksums are
#' byte-faithful (source `NA` mapped to `na_string`, read-back with no NA
#' strings), so row reordering and NA-count changes are detected.
#'
#' @param source_df The tibble/data frame that was written (post-cleaning).
#' @param csv_path Character scalar. Path to the written CSV.
#' @param na_string Character scalar. The `na_string` used when writing.
#'   Default `""`.
#' @param sample_n Integer. Rows returned for a spot-check. Default 5.
#'
#' @return A named list: `all_ok`, `row_count_ok`, `columns_ok`,
#'   `checksums_ok`, `encoding_ok` (all logical), `checksum_report` (tibble),
#'   `sample_rows` (tibble).
#' @family utf8_io
#' @seealso [write_utf8_csv()], [verify_csv_roundtrip()]
#' @export
#'
#' @examples
#' \dontrun{
#' out <- write_utf8_csv(df_clean, cfg = cfg)
#' vld <- validate_csv_export(df_clean, out$path)
#' stopifnot(vld$all_ok)
#' }
validate_csv_export <- function(
  source_df,
  csv_path,
  na_string = "",
  sample_n = 5L
) {
  check_data_frame(source_df)
  check_file(csv_path)
  check_character_vector(na_string)
  check_length(na_string, 1L)
  check_integer(sample_n)

  back <- tryCatch(
    readr::read_csv(
      csv_path,
      col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = "UTF-8"),
      na = character(),
      show_col_types = FALSE
    ),
    error = function(e) e
  )
  if (inherits(back, "error")) {
    cli::cli_abort(
      c(
        "Cannot re-read output CSV: {.path {csv_path}}.",
        "x" = "{conditionMessage(back)}"
      ),
      class = "Huwise_error_io",
      parent = back
    )
  }

  row_ok <- nrow(back) == nrow(source_df)
  if (row_ok) {
    logger::log_info("Row count OK ({nrow(source_df)} rows).")
  } else {
    logger::log_error(
      "Row count mismatch: source={nrow(source_df)}, file={nrow(back)}."
    )
  }

  cols_ok <- identical(sort(names(back)), sort(names(source_df)))
  if (!cols_ok) {
    logger::log_warn(
      "Column mismatch. ",
      "Missing: [{paste(setdiff(names(source_df), names(back)), collapse = ', ')}]. ",
      "Extra: [{paste(setdiff(names(back), names(source_df)), collapse = ', ')}]."
    )
  } else {
    logger::log_info("Column names OK.")
  }

  char_cols <- names(source_df)[vapply(source_df, is.character, logical(1L))]
  src_repr <- source_df[char_cols]
  src_repr[] <- lapply(src_repr, function(x) ifelse(is.na(x), na_string, x))

  src_fp <- dplyr::rename(utf8_fingerprint(src_repr), src_sha256 = "sha256")
  file_fp <- dplyr::rename(
    utf8_fingerprint(back[intersect(char_cols, names(back))]),
    file_sha256 = "sha256"
  )
  checksum_report <- dplyr::full_join(
    src_fp,
    file_fp,
    by = dplyr::join_by("column")
  )
  checksum_report <- dplyr::mutate(
    checksum_report,
    match = !is.na(.data$src_sha256) &
      !is.na(.data$file_sha256) &
      .data$src_sha256 == .data$file_sha256
  )

  checksums_ok <- nrow(checksum_report) == 0L || all(checksum_report$match)
  if (checksums_ok) {
    logger::log_info("All character column checksums match.")
  } else {
    bad_cols <- checksum_report$column[!checksum_report$match]
    logger::log_error(
      "Checksum mismatch in: {paste(bad_cols, collapse = ', ')}. ",
      "Possible silent corruption (content, order, or NA pattern)."
    )
  }

  raw_sample <- readBin(csv_path, what = "raw", n = 65536L)
  enc_detect <- stringi::stri_enc_detect(raw_sample)[[1L]]
  top_enc <- enc_detect$Encoding[1L]
  enc_conf <- enc_detect$Confidence[1L]
  # Detection may report ASCII for ASCII-only files; ASCII is a UTF-8 subset.
  enc_ok <- grepl("UTF-8|UTF8|ASCII", top_enc, ignore.case = TRUE)

  if (enc_ok) {
    logger::log_info(
      "File encoding confirmed UTF-8-compatible ",
      "('{top_enc}', confidence {round(enc_conf, 3)})."
    )
  } else {
    logger::log_warn(
      "Encoding detected as '{top_enc}' (confidence {round(enc_conf, 3)}), ",
      "expected UTF-8."
    )
  }

  sample_rows <- utils::head(back, as.integer(sample_n))
  all_ok <- row_ok && cols_ok && checksums_ok && enc_ok
  logger::log_info(
    "Validation summary: rows={row_ok} | cols={cols_ok} | ",
    "checksums={checksums_ok} | encoding={enc_ok} -> all_ok={all_ok}"
  )

  list(
    all_ok = all_ok,
    row_count_ok = row_ok,
    columns_ok = cols_ok,
    checksums_ok = checksums_ok,
    encoding_ok = enc_ok,
    checksum_report = checksum_report,
    sample_rows = sample_rows
  )
}


# =============================================================================
# SECTION 6 – Global wrapper functions (silent mode) ----
# =============================================================================

#' Load a CSV file into a UTF-8 normalised tibble
#'
#' Detects the source encoding, converts every character column to UTF-8
#' whole-column, and applies NFC normalisation. In the default silent mode
#' the function is quiet on success and speaks up only when the import did not
#' work: it aborts if any value is not valid UTF-8, and warns if any byte
#' sequence had to be replaced with U+FFFD (a lossy import). Set
#' `verbose = TRUE` for a success summary.
#'
#' @param path Character scalar. Path to the `.csv` file.
#' @param verbose Logical. Print a load summary on success. Default `FALSE`.
#' @param silent Logical. When `TRUE` (default) suppress routine progress
#'   logging so the only output is a failure signal; the UTF-8 validity and
#'   lossy-conversion checks fire regardless of this flag.
#'
#' @return A tibble with all character columns as valid NFC UTF-8; `NA`
#'   preserved. Non-character columns are returned unchanged.
#' @family utf8_io
#' @seealso [read_csv_utf8()], [load_txt_utf8()], [export_csv_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- load_csv_utf8("data/sales.csv")                 # silent unless broken
#' tbl <- load_csv_utf8("data/legacy_latin1.csv", verbose = TRUE)
#' }
load_csv_utf8 <- function(path, verbose = FALSE, silent = TRUE) {
  check_file(path)
  check_logical(verbose)
  check_logical(silent)

  cfg <- csv_export_config(
    strict_encoding = FALSE,
    unicode_form = "NFC",
    log_level = if (silent) logger::WARN else logger::INFO,
    error_sample_path = file.path(tempdir(), "load_encoding_errors.csv")
  )
  init_logger(cfg)

  result <- read_csv_utf8(path = path, cfg = cfg)
  table <- result$data

  # --- silent-mode integrity gate: loud only if the import did not work -----
  assert_utf8(table, stage = "import", loud = TRUE)
  n_lost <- nrow(result$problem_report)
  if (n_lost > 0L) {
    cli::cli_warn(
      c(
        "Import of {.path {path}} changed content.",
        "!" = "{cli::qty(n_lost)}{n_lost} value{?s} could not be decoded; \\
               bytes replaced with U+FFFD (lossy import).",
        "i" = "Affected rows: {.path {cfg$error_sample_path}}."
      ),
      class = "Huwise_warning_encoding"
    )
  }

  if (isTRUE(verbose)) {
    cat("\n-- Load summary ----------------------------------------------\n")
    cat(sprintf("  File             : %s\n", path))
    cat(sprintf("  Detected encoding: %s\n", result$detected_encoding))
    cat(sprintf("  Rows             : %d\n", nrow(table)))
    cat(sprintf(
      "  Columns          : %d  (%s)\n",
      ncol(table),
      paste(names(table), collapse = ", ")
    ))
    if (n_lost > 0L) {
      cat(sprintf(
        "  [!] %d value(s) replaced with U+FFFD (see %s)\n",
        n_lost,
        cfg$error_sample_path
      ))
    } else {
      cat("  [OK] No encoding problems detected\n")
    }
  }

  table
}


#' Write a tibble to a validated UTF-8 CSV file
#'
#' Exports to a temporary UTF-8 CSV with NFC normalisation and a wired-in
#' round-trip integrity check. In the default silent mode the function is
#' quiet on success and speaks up only when the export did not work: it aborts
#' if invalid UTF-8 reaches the writer, if the written file is not UTF-8, or
#' if the re-read file does not match the source. Set `verbose = TRUE` for a
#' success summary.
#'
#' @param tibble A data frame or tibble to export.
#' @param verbose Logical. Print a save summary on success. Default `FALSE`.
#' @param silent Logical. When `TRUE` (default) suppress routine progress
#'   logging; the UTF-8 and round-trip checks fire regardless.
#'
#' @return Character scalar. Path to the written CSV file.
#' @family utf8_io
#' @seealso [write_utf8_csv()], [verify_csv_roundtrip()], [load_csv_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' path <- export_csv_utf8(tibble::tibble(name = c("M\u00fcller", "\u00c5ngstr\u00f6m")))
#' }
export_csv_utf8 <- function(tibble, verbose = FALSE, silent = TRUE) {
  check_data_frame(tibble)
  check_logical(verbose)
  check_logical(silent)

  # Coerce every character column to true UTF-8 bytes *and* the UTF-8
  # encoding mark, so the object written by write_utf8_csv() and the object
  # checked by verify_csv_roundtrip() share one baseline. Columns arriving
  # marked "latin1"/"unknown" (e.g. from the smart-me join) otherwise
  # serialise to a different SHA-256 than the UTF-8-marked read-back, which
  # trips the round-trip check as a false "content changed".
  tibble[] <- lapply(
    tibble,
    function(x) if (is.character(x)) stringi::stri_trans_nfc(enc2utf8(x)) else x
  )

  cfg <- csv_export_config(
    strict_encoding = TRUE,
    unicode_form = "NFC",
    write_bom = FALSE,
    na_string = "",
    quote_all = FALSE,
    eol = "\r\n",
    log_level = if (silent) logger::WARN else logger::INFO,
    error_sample_path = file.path(tempdir(), "export_encoding_errors.csv")
  )
  init_logger(cfg)

  out <- write_utf8_csv(df = tibble, out_path = NULL, cfg = cfg)

  # --- silent-mode integrity gate: loud only if the export did not work -----
  if (!out$dry_run) {
    verify_csv_roundtrip(
      source_df = tibble,
      path = out$path,
      cfg = cfg,
      loud = TRUE
    )
  }

  if (isTRUE(verbose)) {
    cat("\n-- Save summary ----------------------------------------------\n")
    if (out$dry_run) {
      cat("  [DRY RUN] No file written.\n")
    } else {
      cat(sprintf("  Temp file  : %s\n", out$path))
      cat(sprintf("  Rows       : %d\n", out$rows))
      cat(sprintf("  Columns    : %d\n", out$cols))
      cat(sprintf("  SHA-256    : %s\n", out$file_sha256))
      cat("  [OK] Round-trip integrity check passed\n")
    }
  }

  out$path
}


# =============================================================================
# SECTION 7 – TXT File Reader ----
# =============================================================================

#' Read a delimited TXT file and return a UTF-8 tibble
#'
#' Uses [data.table::fread()] for most encodings and falls back to
#' [readr::read_delim()] when UTF-16 LE/BE is detected (common in Windows and
#' Swiss open-data exports). All columns are imported as character; the
#' detected (or supplied) encoding is applied whole-column, then NFC
#' normalisation follows.
#'
#' `comment` is honoured only on the UTF-16 (readr) path; [data.table::fread()]
#' has no comment-stripping argument, so a non-empty `comment` on a non-UTF-16
#' file is ignored with a warning.
#'
#' @param path Character scalar. Path to the delimited text file.
#' @param sep Character scalar. Field separator. Default `\\t`. `"auto"`
#'   lets `fread` guess (non-UTF-16 files; UTF-16 falls back to tab).
#' @param cfg A [csv_export_config()] list.
#' @param encoding Character scalar or `NULL`. Override the detected encoding.
#' @param skip Non-negative integer. Lines to skip before the header. Default
#'   `0L`.
#' @param n_max Numeric. Maximum data rows to read. `Inf` (default) reads all.
#' @param na Character vector of strings to treat as `NA`. Default
#'   `c("", "NA")`.
#' @param quote Character scalar. Quoting character. Default `"\""`.
#' @param trim_ws Logical. Strip leading/trailing whitespace. Default `FALSE`.
#' @param comment Character scalar. Lines beginning with this are ignored
#'   (UTF-16/readr path only). `""` (default) disables.
#'
#' @return A named list: `data` (tibble of UTF-8 character columns),
#'   `detected_encoding` (character), `problem_report` (tibble; 0-row if
#'   clean).
#' @family utf8_io
#' @seealso [load_txt_utf8()], [read_csv_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- csv_export_config(strict_encoding = FALSE)
#' init_logger(cfg)
#' res <- read_txt_utf8("data/MIV_2025_open_data.txt", cfg = cfg)
#' head(res$data)
#' }
read_txt_utf8 <- function(
  path,
  sep = "\t",
  cfg = csv_export_config(),
  encoding = NULL,
  skip = 0L,
  n_max = Inf,
  na = c("", "NA"),
  quote = "\"",
  trim_ws = FALSE,
  comment = ""
) {
  check_file(path)
  check_string(sep)
  check_list(cfg)
  if (!is.null(encoding)) {
    check_string(encoding)
  }
  check_logical(trim_ws)

  logger::log_info("Reading TXT: {path}")

  enc_df <- detect_file_encoding(path)
  best_enc <- if (is.null(encoding)) enc_df$encoding[1L] else encoding
  is_utf16 <- grepl("UTF-16", best_enc, ignore.case = TRUE)

  if (is_utf16) {
    actual_sep <- if (identical(sep, "auto")) "\t" else sep
    logger::log_info(
      "UTF-16 detected ({best_enc}); using readr::read_delim, sep = {actual_sep}."
    )
    read_res <- tryCatch(
      tibble::as_tibble(
        readr::read_delim(
          file = path,
          delim = actual_sep,
          col_types = readr::cols(.default = "c"),
          name_repair = "unique",
          show_col_types = FALSE,
          locale = readr::locale(encoding = best_enc),
          skip = skip,
          n_max = n_max,
          na = na,
          quote = quote,
          trim_ws = trim_ws,
          comment = comment
        )
      ),
      error = function(e) e
    )
    if (inherits(read_res, "error")) {
      cli::cli_abort(
        c(
          "Failed to read UTF-16 file {.path {path}}.",
          "x" = "{conditionMessage(read_res)}",
          "i" = "Detected encoding {.val {best_enc}}; verify {.arg sep} \\
                 ({.val {actual_sep}}) matches the file."
        ),
        class = "Huwise_error_io",
        parent = read_res
      )
    }
  } else {
    # data.table::fread() has no `comment` argument; honour it only on the
    # readr/UTF-16 path and warn rather than silently ignore a real request.
    if (!identical(comment, "")) {
      cli::cli_warn(
        c(
          "{.arg comment} is not supported by {.fn data.table::fread}; \\
           ignored for {.path {path}}.",
          "i" = "Comment stripping applies only to the UTF-16 (readr) path."
        ),
        class = "Huwise_warning_unsupported_arg"
      )
    }
    read_res <- tryCatch(
      tibble::as_tibble(
        data.table::fread(
          file = path,
          colClasses = "character",
          sep = sep,
          header = TRUE,
          showProgress = FALSE,
          data.table = FALSE,
          skip = skip,
          nrows = if (is.infinite(n_max)) -1L else as.integer(n_max),
          na.strings = na,
          quote = quote,
          strip.white = trim_ws
        )
      ),
      error = function(e) e
    )
    if (inherits(read_res, "error")) {
      cli::cli_abort(
        c(
          "Failed to read {.path {path}}.",
          "x" = "{conditionMessage(read_res)}",
          "i" = "Check the file is valid delimited text and {.arg sep} \\
                 ({.val {sep}}) matches it."
        ),
        class = "Huwise_error_io",
        parent = read_res
      )
    }
  }

  post_read_enc <- if (is_utf16) "UTF-8" else best_enc
  fix_res <- apply_encoding_fixes(read_res, post_read_enc, cfg)
  df <- normalize_unicode(fix_res$df, cfg$unicode_form)

  logger::log_info(
    "TXT loaded: {nrow(df)} rows x {ncol(df)} cols. ",
    "Source encoding: {best_enc}. Separator: {sep}."
  )
  list(data = df, detected_encoding = best_enc, problem_report = fix_res$report)
}


#' Load a TXT file into a UTF-8 normalised tibble
#'
#' Detects the source encoding — including UTF-16 LE/BE with BOM — converts
#' every character column to UTF-8, and applies NFC normalisation. In the
#' default silent mode the function is quiet on success and speaks up only
#' when the import did not work (invalid UTF-8 aborts; U+FFFD substitutions
#' warn). Set `verbose = TRUE` for a success summary.
#'
#' @param path Character scalar. Path to the `.txt` file.
#' @param sep Character scalar. Field separator. Default `\\t`.
#' @param verbose Logical. Print a load summary on success. Default `FALSE`.
#' @param silent Logical. When `TRUE` (default) suppress routine progress
#'   logging; the integrity checks fire regardless.
#' @param skip Non-negative integer. Lines to skip before the header.
#' @param n_max Numeric. Maximum data rows. `Inf` (default) reads all.
#' @param na Character vector treated as `NA`. Default `c("", "NA")`.
#' @param quote Character scalar. Quoting character. Default `"\""`.
#' @param trim_ws Logical. Strip surrounding whitespace. Default `FALSE`.
#' @param comment Character scalar. Comment prefix; `""` disables.
#'
#' @return A tibble with all character columns as valid NFC UTF-8; `NA`
#'   preserved. Non-character columns unchanged.
#' @family utf8_io
#' @seealso [read_txt_utf8()], [load_csv_utf8()]
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- load_txt_utf8("data/MIV_2025_open_data.txt", verbose = TRUE)
#' tbl <- load_txt_utf8("data/legacy.txt", sep = ";", verbose = TRUE)
#' }
load_txt_utf8 <- function(
  path,
  sep = "\t",
  verbose = FALSE,
  silent = TRUE,
  skip = 0L,
  n_max = Inf,
  na = c("", "NA"),
  quote = "\"",
  trim_ws = FALSE,
  comment = ""
) {
  check_file(path)
  check_string(sep)
  check_logical(verbose)
  check_logical(silent)

  cfg <- csv_export_config(
    strict_encoding = FALSE,
    unicode_form = "NFC",
    log_level = if (silent) logger::WARN else logger::INFO,
    error_sample_path = file.path(tempdir(), "load_encoding_errors.csv")
  )
  init_logger(cfg)

  result <- read_txt_utf8(
    path = path,
    sep = sep,
    cfg = cfg,
    skip = skip,
    n_max = n_max,
    na = na,
    quote = quote,
    trim_ws = trim_ws,
    comment = comment
  )
  table <- result$data

  # --- silent-mode integrity gate: loud only if the import did not work -----
  assert_utf8(table, stage = "import", loud = TRUE)
  n_lost <- nrow(result$problem_report)
  if (n_lost > 0L) {
    cli::cli_warn(
      c(
        "Import of {.path {path}} changed content.",
        "!" = "{cli::qty(n_lost)}{n_lost} value{?s} could not be decoded; \\
               bytes replaced with U+FFFD (lossy import).",
        "i" = "Affected rows: {.path {cfg$error_sample_path}}."
      ),
      class = "Huwise_warning_encoding"
    )
  }

  if (isTRUE(verbose)) {
    cat("\n-- Load summary ----------------------------------------------\n")
    cat(sprintf("  File             : %s\n", path))
    cat(sprintf("  Detected encoding: %s\n", result$detected_encoding))
    cat(sprintf(
      "  Separator        : %s\n",
      if (identical(sep, "\t")) "\\t (tab)" else sep
    ))
    cat(sprintf("  Rows             : %d\n", nrow(table)))
    cat(sprintf(
      "  Columns          : %d  (%s)\n",
      ncol(table),
      paste(names(table), collapse = ", ")
    ))
    if (n_lost > 0L) {
      cat(sprintf(
        "  [!] %d value(s) replaced with U+FFFD (see %s)\n",
        n_lost,
        cfg$error_sample_path
      ))
    } else {
      cat("  [OK] No encoding problems detected\n")
    }
  }

  table
}
