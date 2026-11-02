#' Safely evaluate an odsAutomationR credential getter
#'
#' Wrapper used by [set_credentials()] to read the current \pkg{odsAutomationR}
#' credential values without letting a getter error abort the caller. The
#' argument is evaluated lazily inside a [tryCatch()], so any error (or a
#' \code{NULL} result) collapses to \code{NA_character_}.
#'
#' @param getter_call An expression that returns a credential value, e.g.
#'   \code{odsAutomationR::getKey()}. Evaluated lazily inside this function.
#'
#' @return A character scalar: the getter's value coerced with
#'   [as.character()], or \code{NA_character_} on error or \code{NULL}.
#'
#' @noRd
#' @keywords internal
safe_get <- function(getter_call) {
  tryCatch(
    {
      val <- getter_call
      if (is.null(val)) NA_character_ else as.character(val)
    },
    error = function(e) NA_character_
  )
}

#' Is a value present and non-empty?
#'
#' Predicate used by [set_credentials()] to decide whether a credential value
#' is usable: not \code{NULL}, not \code{NA}, and not the empty string.
#'
#' @param x A length-one value (typically the result of [safe_get()]).
#'
#' @return A single logical: \code{TRUE} if \code{x} is a non-empty, non-\code{NA}
#'   value, otherwise \code{FALSE}.
#'
#' @noRd
#' @keywords internal
is_nonempty <- function(x) {
  !is.null(x) && !is.na(x) && nzchar(as.character(x))
}


#' Parse a raw Automation API response body as JSON
#'
#' Shared helper for the harvester/response utilities: reads the body of an
#' \pkg{httr2} response as text and parses it as JSON, returning an empty list
#' if the body is missing or not valid JSON (so callers can look up fields such
#' as \code{message} or \code{job_id} without a parse error propagating).
#'
#' @param resp An \pkg{httr2} response object (of class \code{httr2_response}).
#'
#' @return The parsed JSON as a list, or an empty \code{list()} if parsing
#'   fails.
#'
#' @noRd
#' @keywords internal
parse_api_response <- function(resp) {
  tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp)),
    error = function(e) list()
  )
}


#' Set credentials for the Huwise Automation API
#'
#' Reads API credentials from the environment and configures the
#' \pkg{odsAutomationR} client with the API key, API type and
#' domain. The function performs a minimal, safe check of existing
#' \pkg{odsAutomationR} credentials and only loads and sets the credentials if
#' they have not already been set.
#'
#' This function expects the matching environment variables to be set (for
#' example in your \code{~/.Renviron} file): \code{ODS_STADT_KEY} / \code{ODS_KANTON_KEY},
#' \code{ODS_STADT_DOMAIN} / \code{ODS_KANTON_DOMAIN} and \code{ODS_APITYPE}.
#' It then calls \pkg{odsAutomationR} helpers to set these values so subsequent
#' Automation API calls use the configured credentials and domain.
#'
#' @param domain Character scalar. The OpenDataSoft domain to target. Must be
#'   one of \code{"stadt"} or \code{"kanton"}.
#'
#' @return Invisibly, a named list with elements \code{api_key},
#'   \code{api_type} and \code{domain}. Called mainly for its side effect of
#'   configuring \pkg{odsAutomationR}.
#'
#' @seealso [get_dataset_uid()], [get_harvester_uid()]
#'
#' @examples
#' \dontrun{
#' # ensure the ODS_* variables exist in your environment (e.g. in ~/.Renviron)
#' set_credentials("kanton")
#' }
#'
#' @export
set_credentials <- function(domain) {
  # Check input variables
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  ## Try reading current creds from odsAutomationR (errors -> NA)
  current_key <- safe_get(odsAutomationR::getKey())
  current_api_type <- safe_get(odsAutomationR::getApiType())
  current_domain <- safe_get(odsAutomationR::getDomain())

  ## If any getter failed (NA) or the domain changed, reset all credentials
  need_reset <- !(is_nonempty(current_key) &&
    is_nonempty(current_api_type) &&
    is_nonempty(current_domain) &&
    identical(current_domain, domain))

  if (!need_reset) {
    ### Credentials present and domain matches — return them unchanged
    return(invisible(list(
      api_key = current_key,
      api_type = current_api_type,
      domain = current_domain
    )))
  }

  # Set variable for domain (Stadt / Kanton)
  domain_id <- if (domain == "stadt") "STADT" else "KANTON"

  # Define variables for Automation API
  api_key <- Sys.getenv(glue::glue("ODS_{domain_id}_KEY"))
  api_type <- Sys.getenv("ODS_APITYPE")
  domain <- Sys.getenv(glue::glue("ODS_{domain_id}_DOMAIN"))

  # Set credentials for odsAutomationR
  odsAutomationR::setKey(api_key)
  odsAutomationR::setApiType(api_type)
  odsAutomationR::setDomain(domain)

  invisible(list(api_key = api_key, api_type = api_type, domain = domain))
}


#' Gzip a file on disk (streaming, base R)
#'
#' Compresses \code{path_in} to \code{path_out} in gzip format by copying the
#' bytes through a \code{\link[base]{gzfile}} connection in fixed-size chunks,
#' so a multi-hundred-megabyte CSV is never held in memory in full. Used by
#' [publish_huwise()] to shrink the export below the portal's 240 MB
#' uploaded-file limit before sending it. The compression is lossless: Huwise
#' (ODS) decompresses a \code{.gz} upload and processes the inner file per the
#' declared resource type, so the inner CSV is byte-identical to \code{path_in}.
#'
#' @param path_in Character scalar. Path to the file to compress.
#' @param path_out Character scalar. Destination \code{.gz} path.
#' @param chunk Integer. Bytes read and written per iteration. Default 10 MB.
#' @param call Environment. Caller, for error attribution.
#'
#' @return Invisibly, \code{path_out}.
#'
#' @noRd
#' @keywords internal
gzip_file <- function(
  path_in,
  path_out,
  chunk = 1e7,
  call = rlang::caller_env()
) {
  check_file(path_in, call = call)
  check_string(path_out, call = call)

  con_in <- file(path_in, open = "rb")
  con_out <- gzfile(path_out, open = "wb")
  on.exit(
    {
      close(con_in)
      close(con_out)
    },
    add = TRUE
  )

  repeat {
    bytes <- readBin(con_in, what = "raw", n = chunk)
    if (length(bytes) == 0L) {
      break
    }
    writeBin(bytes, con_out)
  }

  invisible(path_out)
}


#' Write a tibble to a temp CSV and publish it to a Huwise dataset
#'
#' Shared publish step for the archive/aforms/services pipelines: exports
#' `tibble` to a UTF-8 CSV ([export_csv_utf8()]), gzips that CSV to keep the
#' upload below the portal's 240 MB uploaded-file cap ([gzip_file()]), sets the
#' Huwise credentials for `domain` ([set_credentials()]), then replaces the
#' dataset resource with the compressed file ([odsAutomationR::update_resource()])
#' and republishes it ([odsAutomationR::publish_dataset()]). A success message
#' names the dataset.
#'
#' Huwise/ODS accepts a `.gz` upload and decompresses it server-side, reading
#' the inner file as the declared `csvfile` resource, so compressing is
#' transparent to the dataset's schema and column parsing. Gzip is the
#' platform-recommended way to publish a source file that would otherwise exceed
#' the 240 MB limit; this data compresses by well over an order of magnitude.
#'
#' @param tibble A [tibble::tibble()] (or data frame) to publish as the
#'   resource's new content.
#' @param dataset_uid Character scalar. Huwise dataset UID to update and
#'   publish.
#' @param domain Character scalar. Huwise domain, forwarded to
#'   [set_credentials()].
#' @param call Environment. Caller, for error attribution.
#'
#' @return Invisibly, `tibble` unchanged.
#'
#' @export
publish_huwise <- function(
  tibble,
  dataset_uid,
  domain,
  call = rlang::caller_env()
) {
  check_data_frame(tibble, call = call)
  check_string(dataset_uid, call = call)
  check_string(domain, call = call)

  # Write data to temp file (UTF-8 CSV, round-trip-verified by export_csv_utf8).
  tmp <- export_csv_utf8(tibble = tibble, verbose = FALSE, silent = TRUE)

  tmp_gz <- paste0(tmp, ".gz")
  gzip_file(tmp, tmp_gz, call = call)
  try(unlink(tmp), silent = TRUE)

  # Publish dataset
  set_credentials(domain = domain)
  odsAutomationR::update_resource(
    dataset_uid = dataset_uid,
    filepath = tmp_gz,
    encoding = "UTF-8"
  )
  odsAutomationR::publish_dataset(dataset_uid = dataset_uid)

  cli::cli_inform(c("v" = "Published {.val {dataset_uid}}."))
  invisible(tibble)
}


#' Check Automation API response status and warn on failure
#'
#' Helper that checks the HTTP status code of an \pkg{httr2} response. If the
#' API did not return a \code{200} status code, the response body is parsed for
#' a message, a classed warning is emitted and \code{NULL} is returned
#' invisibly.
#'
#' @param res An \pkg{httr2} response object (of class \code{httr2_response}).
#'
#' @return Invisibly \code{NULL}. If the status code is not \code{200}, a
#'   warning of class \code{"Huwise_error_api"} is emitted containing the status
#'   code and any message found in the parsed response body.
#'
#' @seealso [start_harvester()], [publish_harvester_datasets()]
#'
#' @examples
#' \dontrun{
#' res <- httr2::request("https://...") |> httr2::req_perform()
#' check_response_status(res)
#' }
#'
#' @export
check_response_status <- function(res) {
  # Check input arguments
  check_inherits(res, "httr2_response")
  # Check status code
  status <- httr2::resp_status(res)
  if (status != 200L) {
    response <- parse_api_response(res)
    detail <- if (!is.null(response$message)) {
      response$message
    } else {
      "No message provided."
    }
    cli::cli_warn(
      c(
        "API request returned status {.val {status}}.",
        "x" = "{.val {detail}}"
      ),
      class = "Huwise_error_api"
    )
    return(invisible(NULL))
  }
  invisible(NULL)
}


#' Get dataset UID from Automation API catalog
#'
#' Query the Automation API catalog (via \pkg{odsAutomationR}) and return the
#' \code{dataset_uid} for a given \code{dataset_id}.
#'
#' @inheritParams set_credentials
#'
#' @param dataset_id Character scalar. The dataset identifier (technical
#'   identifier) used on the Huwise portal.
#'
#' @return A character scalar with the \code{dataset_uid} if a single match is
#'   found; otherwise \code{NULL} invisibly (with a classed warning). Dataset
#'   details are reported via [cli::cli_inform()] on success.
#'
#' @family huwise_dataset
#' @seealso [set_credentials()]
#'
#' @examples
#' \dontrun{
#' get_dataset_uid("my-dataset-id", "stadt")
#' }
#'
#' @importFrom rlang .data .env
#' @export
get_dataset_uid <- function(dataset_id, domain) {
  # Check input arguments
  check_string(dataset_id)
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  # Set credentials
  set_credentials(domain = domain)

  # Get dataset_uid for the requested dataset
  catalog <- odsAutomationR::get_catalog() |>
    dplyr::filter(.data$dataset_id == .env$dataset_id)

  if (nrow(catalog) > 1L) {
    cli::cli_warn(
      "More than one dataset matched {.val {dataset_id}}.",
      class = "Huwise_error_value"
    )
    return(invisible(NULL))
  }

  if (nrow(catalog) == 0L) {
    cli::cli_warn(
      "No dataset found with id {.val {dataset_id}}.",
      class = "Huwise_error_no_data"
    )
    return(invisible(NULL))
  }

  dataset_uid <- dplyr::pull(catalog, "dataset_uid")

  cli::cli_inform(c(
    "Dataset ID: {.val {catalog$dataset_id}}",
    "Dataset title: {.val {catalog$title}}",
    "Dataset UID: {.val {dataset_uid}}"
  ))

  dataset_uid
}


#' Get resource UIDs for a dataset
#'
#' Query the API for a dataset's resources and return the \code{uid} field for
#' each resource.
#'
#' @inheritParams get_dataset_uid
#'
#' @param dataset_uid Character scalar. The \code{dataset_uid}.
#'
#' @return A character vector of resource UIDs (the \code{uid} values from the
#'   dataset's resources). If the request yields no resources, an empty
#'   character vector is returned.
#'
#' @family huwise_dataset
#' @seealso [set_credentials()]
#'
#' @examples
#' \dontrun{
#' # get all resource UIDs for dataset "my-ds-uid" on the city domain
#' get_resource_uid("my-ds-uid", "stadt")
#'
#' # get all resource UIDs for the canton domain
#' get_resource_uid("my-ds-uid", "kanton")
#' }
#'
#' @export
get_resource_uid <- function(dataset_uid, domain) {
  # Check input arguments
  check_string(dataset_uid)
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  # Set credentials (side effect: configures odsAutomationR globally)
  set_credentials(domain = domain)

  # Build request from the now-configured credentials
  api_base <- glue::glue(
    "https://{odsAutomationR::getDomain()}/api/{odsAutomationR::getApiType()}"
  )
  auth_hdr <- httr::add_headers(
    Authorization = paste("Apikey", odsAutomationR::getKey())
  )
  resp <- httr::GET(
    url = glue::glue("{api_base}/datasets/{dataset_uid}/resources/"),
    config = auth_hdr
  )

  # Extract all resource uid(s)
  parsed <- httr::content(resp, as = "parsed", simplifyVector = FALSE)
  purrr::map_chr(parsed$results, "uid")
}


#' Get harvester UID from the Automation API
#'
#' Query the Automation API and return the \code{uid} for a harvester
#' identified by its \code{name}. Harvesters are named objects, not slug-ID
#' objects, so matching is done on the \code{name} field.
#'
#' @inheritParams set_credentials
#'
#' @param harvester_name Character scalar. The human-readable name of the
#'   harvester as shown in the Huwise back office.
#'
#' @return A character scalar with the harvester \code{uid} if exactly one
#'   match is found; otherwise \code{NULL} invisibly (with a classed warning).
#'   Harvester details are reported via [cli::cli_inform()] on success.
#'
#' @family huwise_harvester
#' @seealso [set_credentials()]
#'
#' @examples
#' \dontrun{
#' get_harvester_uid("My FTP Harvester", "kanton")
#' }
#'
#' @export
get_harvester_uid <- function(harvester_name, domain) {
  # Check input arguments
  check_string(harvester_name)
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  # Set credentials
  set_credentials(domain = domain)

  # Build request from the now-configured credentials
  api_base <- glue::glue(
    "https://{odsAutomationR::getDomain()}/api/{odsAutomationR::getApiType()}"
  )
  auth_hdr <- httr::add_headers(
    Authorization = paste("Apikey", odsAutomationR::getKey())
  )

  resp <- httr::GET(
    url = glue::glue("{api_base}/harvesters/"),
    config = auth_hdr
  )

  check_response_status(resp)

  parsed <- httr::content(resp, as = "parsed", simplifyVector = TRUE)
  harvesters <- tibble::as_tibble(parsed$results)

  if (nrow(harvesters) == 0L) {
    cli::cli_warn(
      "No harvesters found on this domain.",
      class = "Huwise_error_no_data"
    )
    return(invisible(NULL))
  }

  # Match on the `name` field (harvesters have names, not slug IDs)
  matched <- harvesters[harvesters$name == harvester_name, ]

  if (nrow(matched) == 0L) {
    cli::cli_warn(
      "No harvester found with name {.val {harvester_name}}.",
      class = "Huwise_error_no_data"
    )
    return(invisible(NULL))
  }

  if (nrow(matched) > 1L) {
    cli::cli_warn(
      c(
        "More than one harvester matched {.val {harvester_name}}.",
        "i" = "Returning {.code NULL}."
      ),
      class = "Huwise_error_value"
    )
    return(invisible(NULL))
  }

  # Harvesters use `uid` as their identifier, not `harvester_uid`
  harvester_uid <- matched$uid

  cli::cli_inform(c(
    "Harvester name: {.val {matched$name}}",
    "Harvester type: {.val {matched$type}}",
    "Harvester UID: {.val {harvester_uid}}"
  ))

  harvester_uid
}


#' Start a harvester via the Automation API
#'
#' Sends a \code{POST} request to trigger a harvest run. This is an
#' asynchronous operation: the API returns immediately with a \code{job_id}
#' that can be used to poll the job status. The harvester discovers and
#' creates (unpublished) datasets from the remote source; use
#' [publish_harvester_datasets()] afterwards to make them visible.
#'
#' @inheritParams set_credentials
#'
#' @param harvester_uid Character scalar. The UID of the harvester to start,
#'   as returned by [get_harvester_uid()].
#'
#' @return Invisibly, a character scalar with the \code{job_id} returned by
#'   the API if the request was accepted (HTTP 200/202), or \code{NULL} on
#'   failure (with a classed warning).
#'
#' @family huwise_harvester
#' @seealso [get_harvester_uid()], [publish_harvester_datasets()]
#'
#' @examples
#' \dontrun{
#' uid <- get_harvester_uid("My FTP Harvester", "kanton")
#' job <- start_harvester(uid, "kanton")
#' }
#'
#' @export
start_harvester <- function(harvester_uid, domain) {
  # Check input arguments
  check_string(harvester_uid)
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  # Set credentials
  cred <- set_credentials(domain = domain)

  # Build request
  api_base <- glue::glue("https://{cred$domain}/api/{cred$api_type}")
  auth_hdr <- httr::add_headers(Authorization = paste("Apikey", cred$api_key))

  resp <- httr::POST(
    url = glue::glue("{api_base}/harvesters/{harvester_uid}/start/"),
    config = auth_hdr
  )

  # Starting a harvester is async — API returns 200/202 with a job_id
  # (not a body describing a resource), so we do a lightweight status check.
  if (!resp$status_code %in% c(200L, 202L)) {
    response <- parse_api_response(resp)
    detail <- if (!is.null(response$message)) {
      response$message
    } else {
      "No message provided."
    }
    cli::cli_warn(
      c(
        "Harvester start failed with HTTP {.val {resp$status_code}}.",
        "x" = "{.val {detail}}"
      ),
      class = "Huwise_error_api"
    )
    return(invisible(NULL))
  }

  # Parse and return the job_id for optional polling
  response <- parse_api_response(resp)
  job_id <- response$job_id

  msg <- c("v" = "Harvester {.val {harvester_uid}} started successfully.")
  if (!is.null(job_id)) {
    msg <- c(msg, "i" = "Job ID: {.val {job_id}}.")
  }
  cli::cli_inform(msg)

  invisible(job_id)
}


#' Publish all datasets associated with a harvester
#'
#' Calls the dedicated harvester-level \code{publish} endpoint, which publishes
#' all of the harvester's datasets in a single request. This is an asynchronous
#' operation and returns a \code{job_id}.
#'
#' Note: this is separate from [start_harvester()]. The typical workflow is:
#' (1) [start_harvester()] to discover and (re-)create datasets from the remote
#' source, then (2) [publish_harvester_datasets()] to make them publicly
#' visible on the portal.
#'
#' @inheritParams set_credentials
#'
#' @param harvester_uid Character scalar. The UID of the harvester whose
#'   datasets should be published, as returned by [get_harvester_uid()].
#'
#' @return Invisibly, a character scalar with the \code{job_id} returned by
#'   the API if the request was accepted, or \code{NULL} on failure (with a
#'   classed warning).
#'
#' @family huwise_harvester
#' @seealso [get_harvester_uid()], [start_harvester()]
#'
#' @examples
#' \dontrun{
#' uid <- get_harvester_uid("My FTP Harvester", "kanton")
#' start_harvester(uid, "kanton")
#' publish_harvester_datasets(uid, "kanton")
#' }
#'
#' @export
publish_harvester_datasets <- function(harvester_uid, domain) {
  # Check input arguments
  check_string(harvester_uid)
  domain <- rlang::arg_match(domain, c("stadt", "kanton"))

  # Set credentials
  cred <- set_credentials(domain = domain)

  # Build request — single harvester-level publish endpoint
  api_base <- glue::glue("https://{cred$domain}/api/{cred$api_type}")
  auth_hdr <- httr::add_headers(Authorization = paste("Apikey", cred$api_key))

  resp <- httr::POST(
    url = glue::glue("{api_base}/harvesters/{harvester_uid}/publish/"),
    config = auth_hdr
  )

  # Async endpoint — accept 200 or 202
  if (!resp$status_code %in% c(200L, 202L)) {
    response <- parse_api_response(resp)
    detail <- if (!is.null(response$message)) {
      response$message
    } else {
      "No message provided."
    }
    cli::cli_warn(
      c(
        "Publish failed with HTTP {.val {resp$status_code}}.",
        "x" = "{.val {detail}}"
      ),
      class = "Huwise_error_api"
    )
    return(invisible(NULL))
  }

  response <- parse_api_response(resp)
  job_id <- response$job_id

  msg <- c(
    "v" = "Publish triggered for all datasets of harvester \\
    {.val {harvester_uid}}."
  )
  if (!is.null(job_id)) {
    msg <- c(msg, "i" = "Job ID: {.val {job_id}}.")
  }
  cli::cli_inform(msg)

  invisible(job_id)
}


#' Coerce a vector to a comparison-safe text representation
#'
#' Helper behind the format-insensitive duplicate checks: renders a vector as
#' text so that logically-equal but differently-typed values collide (e.g.
#' \code{5L} vs \code{5.0}). Date/time columns are formatted explicitly rather
#' than via \code{as.character()}, whose \code{POSIXct} method truncates to
#' whole seconds (and drops the time at midnight), which made distinct
#' timestamps within the same second look identical.
#'
#' @param x A vector of any type.
#'
#' @return A character vector the same length as \code{x}, with \code{NA}
#'   preserved. \code{POSIXct}/\code{POSIXlt} are rendered as
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%OS6"} in UTC (instant-based, so the
#'   \code{tzone} attribute does not affect the result); \code{Date} as
#'   \code{"\%Y-\%m-\%d"}; everything else via \code{as.character()}.
#'
#' @noRd
#' @keywords internal
as_text_key <- function(x) {
  if (inherits(x, "POSIXt")) {
    # UTC + explicit %OS6: independent of options(digits.secs) and of the
    # column's tzone attribute; resolves to 1 microsecond.
    format(as.POSIXct(x), format = "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
  } else if (inherits(x, "Date")) {
    format(x, format = "%Y-%m-%d")
  } else {
    as.character(x)
  }
}


#' Check a combined dataset for duplicate rows (format-insensitive)
#'
#' Runs two duplicate checks on an already-joined dataset, comparing every
#' value **as text** so that logically-identical rows stored in different
#' formats are still flagged (e.g. integer \code{5} vs double \code{5.0}, or a
#' key that is character in one source and numeric in another). Before either
#' check every column is coerced with \code{as_text_key()} and the comparison
#' is run on that character copy; the original, un-coerced rows are what get
#' reported. Date/time columns keep sub-second precision and are normalised to
#' UTC, so timestamps inside the same second are *not* treated as equal. The
#' two checks are: (1) exact full-row duplicates across all columns, and (2)
#' duplicate keys (rows sharing the same values across \code{key_cols}). If
#' either check finds duplicates the function aborts with an informative error.
#'
#' @param data A data frame. The already-joined / combined dataset to check.
#' @param key_cols Character vector. Column name(s) that together should
#'   uniquely identify each observation (e.g. \code{c("id", "date")}).
#'
#' @return \code{data} invisibly if no duplicates are found. On failure the
#'   function aborts; the raised condition carries the offending rows in
#'   \code{full_row_dupes} (class \code{"Huwise_error_duplicate"}) or
#'   in \code{key_dupes} together with \code{key_cols} (class
#'   \code{"Huwise_error_duplicate"}).
#'
#' @examples
#' \dontrun{
#' combined <- dplyr::bind_rows(data_old, data_new)
#' check_duplicates(combined, key_cols = c("id", "date"))
#' }
#'
#' @export
check_duplicates <- function(data, key_cols) {
  # ── Input checks ─────────────────────────────────────────────────────────
  check_data_frame(data)
  check_character_vector(key_cols)
  check_cols_in_df(data, key_cols)
  # ── Character copy: compare every value as text so differently-typed but
  #    logically-equal values (e.g. 5L vs 5.0) collide during both passes.
  #    NB: as_text_key(), not as.character() — the latter truncates POSIXct to
  #    whole seconds and produced spurious duplicates on dttm columns.
  data_chr <- dplyr::mutate(
    data,
    dplyr::across(dplyr::everything(), as_text_key)
  )
  # ── Pass 1: exact full-row duplicates (text comparison) ──────────────────
  full_dup <- duplicated(data_chr) | duplicated(data_chr, fromLast = TRUE)
  if (any(full_dup)) {
    full_row_dupes <- data[full_dup, ] # report the original, un-coerced rows
    cli::cli_abort(
      c(
        "Found {nrow(full_row_dupes)} full-row duplicate{?s} \\
        (all columns compared as text).",
        "i" = "{cli::qty(nrow(full_row_dupes))}Duplicated \\
        row{?s}: {.val {which(full_dup)}}."
      ),
      class = "Huwise_error_duplicate",
      full_row_dupes = full_row_dupes
    )
  }
  # ── Pass 2: key-level duplicates (keys compared as text) ─────────────────
  key_n <- data_chr |>
    dplyr::mutate(.dup_n = dplyr::n(), .by = dplyr::all_of(key_cols)) |>
    dplyr::pull(".dup_n")
  if (any(key_n > 1L)) {
    key_dupes <- data[key_n > 1L, ] # report the original, un-coerced rows
    cli::cli_abort(
      c(
        "{cli::qty(nrow(key_dupes))}Found {nrow(key_dupes)} row{?s} \\
        with a duplicate key (values compared as text).",
        "i" = "{cli::qty(length(key_cols))}Key column{?s}: {.field {key_cols}}.",
        "i" = "{cli::qty(nrow(key_dupes))}Duplicated \\
        row{?s}: {.val {which(key_n > 1L)}}."
      ),
      class = "Huwise_error_duplicate",
      key_dupes = key_dupes,
      key_cols = key_cols
    )
  }
  invisible(data)
}
