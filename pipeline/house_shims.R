# Minimal stand-ins for the house-package helpers that pipeline/miv_stadt_fct.R
# calls but that are not part of the supplied file.
#
# Only the helpers reachable from `migrate_published_to_long()` are needed, so
# this file is deliberately tiny. The pipeline file itself is vendored verbatim
# and must stay unmodified: it is the code under test, and editing it to make
# the harness pass would defeat the point.

# Mirrors the house checker's contract: character vector or a hard error.
check_character_vector <- function(x, call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a character vector.",
        "x" = "Got {.cls {class(x)}}."
      ),
      class = "Huwise_error_type",
      call = call
    )
  }
  invisible(x)
}

check_duplicates <- function(data, key_cols, call = rlang::caller_env()) {
  dup <- sum(duplicated(data[key_cols]))
  if (dup > 0L) {
    cli::cli_abort(
      c(
        "Duplicate rows on the declared key.",
        "x" = "{dup} duplicate row{?s} on {.col {key_cols}}."
      ),
      class = "Huwise_error_duplicate",
      call = call
    )
  }
  invisible(data)
}

# The remaining house helpers (file/directory readers, the portal download and
# upload) are not reachable from the step under test. They are stubbed so a
# stray call fails loudly instead of silently resolving to something else.
not_available <- function(what) {
  function(...) {
    stop(
      what, "() is not available in the validation harness. The harness ",
      "compares frozen snapshots and never touches files or portals."
    )
  }
}

check_file <- not_available("check_file")
check_directory <- not_available("check_directory")
check_string <- not_available("check_string")
check_logical <- not_available("check_logical")
check_numeric <- not_available("check_numeric")
load_txt_utf8 <- not_available("load_txt_utf8")
load_csv_utf8 <- not_available("load_csv_utf8")
download_dataset <- not_available("download_dataset")
publish_huwise <- not_available("publish_huwise")
