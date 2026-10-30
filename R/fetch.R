# Source resolution: local file, in-memory object, or an Opendatasoft portal.
#
# Fetch once, then freeze. A resolved source is cached to .RDS with its fetch
# timestamp; every later run reads that frozen snapshot, so the comparison is
# always two in-memory R objects and the portal cannot shift the ground truth
# halfway through a migration. A local original and an ODS original resolve to
# the same kind of object; nothing downstream knows the source.

ods_bases <- c(
  "daten.sg.ch" = "https://daten.sg.ch/api/explore/v2.1",
  "daten.stadt.sg.ch" = "https://daten.stadt.sg.ch/api/v2"
)

#' Resolve a configured source to an in-memory tibble, freezing a snapshot
#'
#' @param source A list describing the source. `kind` is one of `"object"`,
#'   `"rds"`, `"parquet"`, `"csv"` or `"ods"`; the remaining fields depend on
#'   the kind (`path`, `data`, or `dataset_id` plus `portal`).
#' @param cache_dir Character scalar. Directory for frozen `.RDS` snapshots.
#' @param label Character scalar used to name the snapshot file. A `label`
#'   field on `source` overrides it, so several configs over the same input
#'   share one frozen snapshot instead of each fetching their own.
#' @return A tibble, carrying `migval_source` and `migval_frozen_at` attributes.
#' @export
resolve_source <- function(source, cache_dir = "snapshots", label = "source") {
  if (identical(source[["kind"]], "object")) {
    return(tibble::as_tibble(source[["data"]]))
  }

  if (identical(source[["kind"]], "rds")) {
    return(tibble::as_tibble(readRDS(source[["path"]])))
  }

  # Everything else is fetched or parsed once and frozen.
  snapshot <- file.path(cache_dir, paste0(source[["label"]] %||% label, ".rds"))
  if (file.exists(snapshot)) {
    return(tibble::as_tibble(readRDS(snapshot)))
  }

  data <- switch(
    source[["kind"]],
    parquet = read_parquet_source(source[["path"]]),
    csv = read_csv_source(source[["path"]]),
    ods = fetch_ods_dataset(
      dataset_id = source[["dataset_id"]],
      portal = source[["portal"]],
      format = source[["format"]] %||% "parquet"
    ),
    stop("unknown source kind: ", source[["kind"]])
  )

  attr(data, "migval_source") <- source[["path"]] %||% source[["dataset_id"]]
  attr(data, "migval_frozen_at") <- format(Sys.time(), tz = "Europe/Zurich")

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, snapshot)
  tibble::as_tibble(data)
}

# Parquet is the default wire format because it preserves types, which keeps
# exact-equality honest. `arrow` is a soft dependency so the harness still runs
# where only frozen .RDS snapshots are present.
read_parquet_source <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "reading Parquet needs the 'arrow' package. Install it, or supply a ",
      "frozen .RDS snapshot instead (source kind \"rds\")."
    )
  }
  tibble::as_tibble(arrow::read_parquet(path))
}

read_csv_source <- function(path) {
  tibble::as_tibble(
    utils::read.csv(
      path,
      colClasses = "character",
      check.names = FALSE,
      encoding = "UTF-8"
    )
  )
}

#' Fetch a whole dataset from an Opendatasoft portal
#'
#' Uses the exports endpoint, never records: records caps the offset at 10'000
#' rows and would silently truncate, while exports streams the whole dataset.
#'
#' @param dataset_id Character scalar. The portal's dataset identifier.
#' @param portal Character scalar. Either `"daten.sg.ch"` or
#'   `"daten.stadt.sg.ch"`.
#' @param format Character scalar. `"parquet"` (default) or `"csv"`.
#' @return A tibble of the full dataset.
#' @export
fetch_ods_dataset <- function(dataset_id, portal, format = "parquet") {
  if (!portal %in% names(ods_bases)) {
    stop(
      "unknown portal: ", portal,
      ". Known portals: ", paste(names(ods_bases), collapse = ", ")
    )
  }
  if (!format %in% c("parquet", "csv")) {
    stop("format must be \"parquet\" or \"csv\", not ", format)
  }

  req <- httr2::request(ods_bases[[portal]]) |>
    httr2::req_url_path_append("catalog", "datasets", dataset_id, "exports", format) |>
    httr2::req_retry(max_tries = 3L)

  # Only a few datasets need a key; the header is omitted when it is unset, and
  # the key is never written to code or config.
  api_key <- Sys.getenv("ODS_API_KEY", unset = "")
  if (nzchar(api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Apikey", api_key))
  }

  tmp <- tempfile(fileext = paste0(".", format))
  on.exit(unlink(tmp), add = TRUE)
  httr2::req_perform(req, path = tmp)

  if (identical(format, "parquet")) {
    read_parquet_source(tmp)
  } else {
    read_csv_source(tmp)
  }
}
