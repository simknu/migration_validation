#' Check that a value inherits from a class
#'
#' Generic building block used by [check_data_frame()] and [check_list()].
#'
#' @param x Object to check.
#' @param what Class name(s) `x` must inherit from. Passed to [inherits()].
#' @param arg Name of the argument being checked, used in the error message.
#'   Defaults to the argument name in the calling function.
#' @param call Calling environment, used so the error is attributed to the
#'   user-facing function rather than to this helper. Defaults to the caller.
#'
#' @return Invisible `NULL`. Errors if `x` does not inherit from `what`.
#' @noRd
#' @keywords internal
check_inherits <- function(
  x,
  what,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, what)) {
    cli::cli_abort(
      "{.arg {arg}} must inherit from {.cls {what}}, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is a single string
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a single string.
#' @noRd
#' @keywords internal
check_string <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_string(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single string, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is a character vector
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a character vector.
#' @noRd
#' @keywords internal
check_character_vector <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.character(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is integerish
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not integer-like.
#' @noRd
#' @keywords internal
check_integer <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_integerish(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be an integer, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is numeric
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not numeric.
#' @noRd
#' @keywords internal
check_numeric <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is logical
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not logical.
#' @noRd
#' @keywords internal
check_logical <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be logical, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is not NULL
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is `NULL`.
#' @noRd
#' @keywords internal
check_not_null <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (rlang::is_null(x)) {
    cli::cli_abort(
      "{.arg {arg}} must not be {.code NULL}.",
      class = "Huwise_error_missing_value",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value contains no missing values
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` contains any `NA`.
#' @noRd
#' @keywords internal
check_not_na <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} must not contain missing values ({.code NA}).",
      class = "Huwise_error_missing_value",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value has a specific length
#'
#' @inheritParams check_inherits
#' @param n Required length of `x`.
#'
#' @return Invisible `NULL`. Errors if `length(x) != n`.
#' @noRd
#' @keywords internal
check_length <- function(
  x,
  n,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (length(x) != n) {
    cli::cli_abort(
      "{.arg {arg}} must have length {.val {n}}, not {.val {length(x)}}.",
      class = "Huwise_error_length",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is one of a set of allowed strings
#'
#' @inheritParams check_inherits
#' @param choices Character vector of allowed values for `x`.
#'
#' @return Invisible `NULL`. Errors if `x` is not in `choices`.
#' @noRd
#' @keywords internal
check_one_of <- function(
  x,
  choices,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_string(x, arg = arg, call = call)
  if (!x %in% choices) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be one of {.val {choices}}.",
        "x" = "You provided {.val {x}}."
      ),
      class = "Huwise_error_value",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a path is an existing directory
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a single string pointing to
#'   an existing directory.
#' @noRd
#' @keywords internal
check_directory <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_string(x, arg = arg, call = call)
  if (!dir.exists(x)) {
    cli::cli_abort(
      "{.arg {arg}} is not an existing directory: {.path {x}}.",
      class = "Huwise_error_path",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a path is an existing file
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a single string pointing to
#'   an existing, non-directory file.
#' @noRd
#' @keywords internal
check_file <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_string(x, arg = arg, call = call)
  if (!file.exists(x)) {
    cli::cli_abort(
      "{.arg {arg}} is not an existing file: {.file {x}}.",
      class = "Huwise_error_path",
      call = call
    )
  }
  if (isTRUE(file.info(x)$isdir)) {
    # A directory where a file is expected is a path-kind error, not a type error.
    cli::cli_abort(
      "{.arg {arg}} must be a file, not a directory: {.path {x}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a path points to an `.xlsx` file
#'
#' Use this ahead of any `openxlsx2`-based read/write helper so a wrong
#' extension fails fast with a clear message instead of surfacing an
#' `openxlsx2` internal error.
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` does not end in `.xlsx`.
#' @noRd
#' @keywords internal
check_xlsx_path <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_string(x, arg = arg, call = call)
  if (!grepl("\\.xlsx$", x, ignore.case = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} must point to an {.val .xlsx} file, not {.path {x}}.",
      class = "Huwise_error_path",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is a data frame (non-empty by default)
#'
#' @inheritParams check_inherits
#' @param allow_empty Logical. When `FALSE` (default) a zero-row `x` aborts
#'   with `Huwise_error_empty_input`. Set `TRUE` for checks where row count is
#'   irrelevant (e.g. column-membership), so a 0-row frame passes.
#'
#' @return Invisible `NULL`. Errors if `x` is not a data frame (or subclass,
#'   e.g. a tibble), or has zero rows unless `allow_empty = TRUE`.
#' @noRd
#' @keywords internal
check_data_frame <- function(
  x,
  allow_empty = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_inherits(x, "data.frame", arg = arg, call = call)
  if (!allow_empty && nrow(x) == 0) {
    # Caller passed a zero-row object -> empty_input (fatal), not no_data (skippable upstream).
    cli::cli_abort(
      "{.arg {arg}} is empty. Please provide data with at least one row.",
      class = "Huwise_error_empty_input",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is a list
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a list. Note this
#'   deliberately checks `inherits(x, "list")` rather than `is.list()`,
#'   because data frames are also `is.list() == TRUE` and should be rejected
#'   here.
#' @noRd
#' @keywords internal
check_list <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_inherits(x, "list", arg = arg, call = call)
  invisible(NULL)
}

#' Check that a data frame contains a set of required columns
#'
#' @inheritParams check_inherits
#' @param required_cols Character vector of column names that must be present
#'   in `x`.
#'
#' @return Invisible `NULL`. Errors if any of `required_cols` is missing
#'   from `colnames(x)`.
#' @noRd
#' @keywords internal
check_cols_in_df <- function(
  x,
  required_cols,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_data_frame(x, allow_empty = TRUE, arg = arg, call = call)
  check_character_vector(required_cols, call = call)
  missing_cols <- setdiff(required_cols, colnames(x))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing {cli::qty(length(missing_cols))} \\
       required column{?s}: {.col {missing_cols}}.",
      class = "Huwise_error_schema",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that a value is a single number between 0 and 1
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a single number, is `NA`, or
#'   is outside `[0, 1]`.
#' @noRd
#' @keywords internal
check_percentage <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!rlang::is_scalar_double(x) && !rlang::is_scalar_integerish(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number, not {.cls {class(x)}}.",
      class = "Huwise_error_type",
      call = call
    )
  }
  if (is.na(x)) {
    cli::cli_abort(
      "{.arg {arg}} must not be {.code NA}.",
      class = "Huwise_error_missing_value",
      call = call
    )
  }
  if (x < 0 || x > 1) {
    cli::cli_abort(
      "{.arg {arg}} must be between {.val {0}} and {.val {1}}, not {.val {x}}.",
      class = "Huwise_error_value",
      call = call
    )
  }
  invisible(NULL)
}

#' Check that an argument is a date string in `YYYY-MM-DD` format
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a valid `YYYY-MM-DD` date.
#' @noRd
#' @keywords internal
check_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_string(x, arg = arg, call = call)

  if (!stringr::str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")) {
    cli::cli_abort(
      "{.arg {arg}} must be a date in {.val YYYY-MM-DD} format, not {.val {x}}.",
      class = "Huwise_error_value",
      call = call
    )
  }

  if (is.na(as.Date(x, format = "%Y-%m-%d"))) {
    cli::cli_abort(
      "{.arg {arg}} is not a valid calendar date: {.val {x}}.",
      class = "Huwise_error_value",
      call = call
    )
  }

  invisible(NULL)
}

#' Check that an input is a single positive number
#'
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Errors if `x` is not a single positive number.
#' @noRd
#' @keywords internal
check_positive_number <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, arg = arg, call = call)

  if (length(x) != 1L || is.na(x) || x <= 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive number, not {.val {x}}.",
      class = "Huwise_error_value",
      call = call
    )
  }

  invisible(NULL)
}

#' Check that an input is a valid purrr progress specification
#'
#' @param progress Object to check. Valid: `TRUE`, `FALSE`, or a non-empty
#'   character scalar naming the progress bar.
#' @inheritParams check_inherits
#'
#' @return Invisible `NULL`. Called for its side effect of aborting.
#' @noRd
#' @keywords internal
check_progress <- function(
  progress,
  arg = rlang::caller_arg(progress),
  call = rlang::caller_env()
) {
  valid <- (is.logical(progress) &&
    length(progress) == 1L &&
    !is.na(progress)) ||
    (is.character(progress) && length(progress) == 1L && nzchar(progress))

  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.code TRUE}, {.code FALSE}, or a non-empty \\
         string naming the progress bar.",
        "x" = "Got {.obj_type_friendly {progress}}."
      ),
      class = "Huwise_error_type",
      call = call
    )
  }

  invisible(NULL)
}
