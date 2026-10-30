# The report's spine is a coverage ledger. Finding missing checks is a stated
# goal of this harness, so a column no check covers is printed as a gap, as
# loudly as a failure.

rule <- function(label = NULL) {
  if (is.null(label)) {
    cat(strrep("-", 78), "\n", sep = "")
  } else {
    cat("\n", label, "\n", strrep("=", 78), "\n", sep = "")
  }
}

yesno <- function(x) {
  ifelse(is.na(x), "n/a", ifelse(x, "yes", "NO"))
}

#' Build the coverage ledger
#'
#' One row per output column: its lineage class, the check that covered it,
#' whether it matched on both sides, and how missing values were treated. A
#' column with no applicable check is classed `uncovered`.
#'
#' @param comparison A `compare_canonical()` result.
#' @param lineage A tibble with `column` and `class`, from the frozen config or
#'   `draft_lineage_spec()`.
#' @param left_diag,right_diag The `diagnostics` tibbles from `canonicalize()`.
#' @return A tibble, one row per attribute.
#' @export
coverage_ledger <- function(comparison, lineage, left_diag, right_diag) {
  diag <- dplyr::full_join(
    dplyr::select(left_diag, "attribute",
                  known_dropped_empty = "n_empty_dropped",
                  known_na_string = "n_na_string"),
    dplyr::select(right_diag, "attribute",
                  new_dropped_empty = "n_empty_dropped",
                  new_na_string = "n_na_string"),
    by = dplyr::join_by("attribute")
  )

  comparison$per_attribute |>
    dplyr::left_join(
      dplyr::select(lineage, attribute = "column", "class"),
      by = dplyr::join_by("attribute")
    ) |>
    dplyr::left_join(diag, by = dplyr::join_by("attribute")) |>
    mutate(
      class = dplyr::coalesce(.data$class, "unclassified"),
      check = dplyr::case_when(
        !.data$present_both ~ "none - one side only",
        .data$class %in% c("new", "dropped") ~ "none - no ground truth",
        .default = "exact multiset after normalization"
      ),
      covered = .data$check != "none - one side only" &
        !.data$class %in% c("new", "dropped")
    ) |>
    dplyr::select(
      "attribute", "class", "check", "covered", "present_both",
      "equal", "equal_on_shared_keys",
      "n_left", "n_right", "n_left_only", "n_right_only",
      "known_dropped_empty", "new_dropped_empty",
      "known_na_string", "new_na_string"
    ) |>
    dplyr::arrange(.data$equal, dplyr::desc(.data$covered), .data$attribute)
}

#' Print the validation report
#'
#' @param result A `validate_dataset()` result.
#' @return Invisibly, `result`.
#' @export
print_report <- function(result) {
  rule(paste0("MIGRATION VALIDATION REPORT - ", result$dataset))
  cat("run at              : ", result$run_at, "\n", sep = "")
  cat("grain key           : ", paste(result$grain_key, collapse = ", "), "\n", sep = "")
  cat("known-final source  : ", result$sources$known_final, "\n", sep = "")
  cat("original source     : ", result$sources$original, "\n", sep = "")
  cat("rows: original ", result$n_rows$original,
      " -> new output ", result$n_rows$new_output,
      " | known-final ", result$n_rows$known_final, "\n", sep = "")

  rule("1. GRAIN UNIQUENESS (guards row-level drill-down)")
  print(result$grain_check, n = Inf)
  if (!all(result$grain_check$unique)) {
    cat("\n  REFUSING row-level drill-down: the grain is not unique on at least\n")
    cat("  one side, so row diffs would mislead. A silent many-to-one here is\n")
    cat("  the subtle bug that invalidates everything downstream of it.\n")
    cat("  The verdict below is unaffected and still runs (bag semantics).\n")
  }

  rule("2. ENTITY-KEY ROSTER")
  k <- result$comparison$keys
  cat("known-final keys    : ", k$n_left, "\n", sep = "")
  cat("new-output keys     : ", k$n_right, "\n", sep = "")
  cat("shared keys         : ", k$n_shared, "\n", sep = "")
  cat("known-final only    : ", length(k$left_only), "\n", sep = "")
  cat("new-output only     : ", length(k$right_only), "\n", sep = "")
  cat("rosters identical   : ", yesno(k$equal), "\n", sep = "")
  if (!k$equal) {
    cat("\n  Example keys present on one side only (", result$examples_cap, " max each):\n", sep = "")
    show_keys("  known-final only", k$left_only, result$grain_key, result$examples_cap)
    show_keys("  new-output only ", k$right_only, result$grain_key, result$examples_cap)
  }

  rule("3. COVERAGE LEDGER (per output column)")
  print(
    dplyr::select(
      result$ledger,
      "attribute", "class", "covered", "equal", "equal_on_shared_keys",
      "n_left_only", "n_right_only", "check"
    ),
    n = Inf,
    width = Inf
  )

  gaps <- dplyr::filter(result$ledger, !.data$covered)
  rule("4. UNCOVERED GAPS")
  if (nrow(gaps) == 0L) {
    cat("none - every output column carries an applicable check.\n")
  } else {
    for (i in seq_len(nrow(gaps))) {
      cat("  ", gaps$attribute[[i]], " [", gaps$class[[i]], "]: ",
          gaps$check[[i]], "\n", sep = "")
    }
    cat("\n  These columns are asserted by nothing in this run.\n")
  }

  rule("5. VALUE PAIRS for `changed` or failing columns")
  if (length(result$changed_pairs) == 0L) {
    cat("no column is classed `changed` and none failed.\n")
  } else {
    for (nm in names(result$changed_pairs)) {
      pairs <- result$changed_pairs[[nm]]
      reviewed <- isTRUE(result$lookup_reviewed[[nm]])
      n_agree <- attr(pairs, "n_keys_agreeing") %||% 0L
      cat("\n  ", nm, ": ", nrow(pairs), " differing pair(s) over ",
          format(n_agree, big.mark = "'"), " agreeing key(s); lookup ",
          if (reviewed) "reviewed by the user" else "ASSERTED BY NOTHING",
          "\n", sep = "")
      if (nrow(pairs) == 0L) {
        cat("      every shared key agrees; the failure is roster-only.\n")
      } else {
        print(utils::head(pairs, result$examples_cap), n = Inf, width = Inf)
        if (nrow(pairs) > result$examples_cap) {
          cat("      ... and ", nrow(pairs) - result$examples_cap,
              " more differing pair(s)\n", sep = "")
        }
        if (!all(is.na(pairs$in_lookup))) {
          n_unexplained <- sum(!pairs$in_lookup)
          cat("      ", n_unexplained, " of ", nrow(pairs),
              " differing pair(s) are NOT in the reviewed lookup.\n", sep = "")
          unused <- attr(pairs, "unused_lookup")
          if (!is.null(unused) && nrow(unused) > 0L) {
            cat("      ", nrow(unused),
                " reviewed lookup entr(y/ies) never occur in the data:\n", sep = "")
            print(unused, n = Inf, width = Inf)
          }
        }
      }
    }
    cat("\n  These pairs are derived from the pipeline, so checking them against\n")
    cat("  the pipeline would be circular. They are printed for a human to\n")
    cat("  compare against the lookup table; that review is the assertion.\n")
  }

  rule("6. SCHEMA-DIFF RESIDUE (raw columns, before reshape)")
  if (nrow(result$schema_residue) == 0L) {
    cat("none - both sides carry the same raw column names.\n")
  } else {
    print(result$schema_residue, n = Inf)
  }

  rule("7. DRAFT LINEAGE SPEC (content-matched on shared keys; review and freeze)")
  print(result$draft_spec, n = Inf, width = Inf)

  rule("8. VERDICT")
  if (result$comparison$pass) {
    cat("PASS - the two sides reduce to identical multisets of triples.\n")
  } else {
    cat("FAIL - the multisets differ.\n")
    ex <- result$comparison$examples
    if (nrow(ex) > 0L) {
      cat("\nExample diverging triples (capped at ", result$examples_cap,
          " per attribute):\n", sep = "")
      print(
        mutate(ex, entity_key = gsub("\x1f", " | ", .data$entity_key)),
        n = Inf,
        width = Inf
      )
    }
  }

  rule("9. HONEST LIMITS OF THIS RUN")
  cat(paste0("  - ", result$limits, collapse = "\n"), "\n", sep = "")
  rule()
  invisible(result)
}

show_keys <- function(label, keys, grain_key, cap) {
  if (length(keys) == 0L) {
    cat(label, ": none\n", sep = "")
    return(invisible(NULL))
  }
  cat(label, " (", length(keys), "): ", sep = "")
  shown <- utils::head(keys, cap)
  cat("\n")
  for (k in shown) {
    cat("      ", paste(grain_key, strsplit(k, "\x1f", fixed = TRUE)[[1L]],
                        sep = "=", collapse = " "), "\n", sep = "")
  }
  if (length(keys) > cap) {
    cat("      ... and ", length(keys) - cap, " more\n", sep = "")
  }
  invisible(NULL)
}
