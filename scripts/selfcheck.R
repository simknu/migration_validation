#!/usr/bin/env Rscript
# Engine self-check: exercises the comparison machinery on tiny synthetic data,
# so a regression surfaces in seconds rather than after a full dataset run.
#
# Not a test suite to maintain -- no testthat, no fixtures, nothing to keep in
# step with the data. It asserts the properties the guarantee rests on.
#
# Usage: Rscript scripts/selfcheck.R   (exits non-zero if anything fails)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

failures <- 0L
ok <- function(label, result) {
  passed <- isTRUE(result)
  if (!passed) failures <<- failures + 1L
  cat(sprintf("%-48s %s\n", label, if (passed) "PASS" else "** FAIL **"))
}
errs <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

cat("-- normalization ------------------------------------------------\n")
ok("whitespace runs collapsed", identical(normalize_column("  a   b ")$value, "a b"))
ok("empty becomes NA (dropped later)", is.na(normalize_column("   ")$value))
ok("literal \"NA\" converted and counted", normalize_column(c("NA", "x"))$n_na_string == 1L)
ok("na_string off keeps \"NA\"", identical(
  normalize_column("NA", list(na_string = FALSE))$value, "NA"))
ok("numbers formatted element-wise", identical(
  normalize_column(c(1, 2.5), list(type = "number"))$value, c("1", "2.5")))
ok("dates compared as dates", identical(
  normalize_column(as.Date("2024-01-05"), list(type = "date"))$value, "2024-01-05"))
ok("raw bytes become lowercase hex", identical(
  normalize_column(list(as.raw(c(0x0a, 0xff))), list(type = "hex"))$value, "0aff"))

cat("\n-- tolerance ----------------------------------------------------\n")
nz <- function(x, tol) normalize_column(x, list(type = "number", tolerance = tol))$value
ok("bare number means absolute", identical(nz(c(1.004, 1.006), 0.01), c("1", "1.01")))
ok("list(abs=) matches bare form", identical(nz(1.004, list(abs = 0.01)), nz(1.004, 0.01)))
ok("relative: within tolerance collapses", identical(
  nz(1000, list(rel = 0.05)), nz(1004, list(rel = 0.05))))
ok("relative: beyond tolerance stays apart",
   nz(1000, list(rel = 0.05)) != nz(1500, list(rel = 0.05)))
ok("relative scales with magnitude", identical(
  nz(1e6, list(rel = 0.05)), nz(1.004e6, list(rel = 0.05))))
ok("relative leaves zero alone", identical(nz(0, list(rel = 0.05)), "0"))
ok("relative handles negatives",
   nz(-1000, list(rel = 0.05)) == paste0("-", nz(1000, list(rel = 0.05))))
ok("no tolerance means exact", identical(nz(c(1, 2.5), NULL), c("1", "2.5")))
ok("unknown tolerance field errors", errs(nz(1, list(nope = 1))))
ok("negative tolerance errors", errs(nz(1, list(abs = -1))))

cat("\n-- bag semantics ------------------------------------------------\n")
spec <- list(dataset = "t", grain_key = "id",
             normalize = list(n = list(type = "number")), na_string = TRUE)
mk <- function(id, n) canonicalize(tibble(id = id, n = n), list(), spec, "s")
ok("identical data passes", compare_canonical(
  mk(c("a", "b"), c(1L, 2L)), mk(c("a", "b"), c(1L, 2L)), 5L, TRUE)$pass)
ok("row order is ignored", compare_canonical(
  mk(c("a", "b"), c(1L, 2L)), mk(c("b", "a"), c(2L, 1L)), 5L, TRUE)$pass)
ok("a differing value fails", !compare_canonical(
  mk(c("a", "b"), c(1L, 2L)), mk(c("a", "b"), c(1L, 9L)), 5L, TRUE)$pass)
ok("repeats are kept, not deduplicated", {
  s2 <- list(dataset = "t", grain_key = "id", normalize = list(), na_string = TRUE)
  l <- canonicalize(tibble(id = c("a", "a"), v = c("x", "x")), list(), s2, "l")
  r <- canonicalize(tibble(id = c("a", "a", "a"), v = c("x", "x", "x")), list(), s2, "r")
  !compare_canonical(l, r, 5L, FALSE)$pass
})
ok("non-unique grain is detected", {
  s2 <- list(dataset = "t", grain_key = "id", normalize = list(), na_string = TRUE)
  !check_grain_unique(canonicalize(
    tibble(id = c("a", "a"), v = c("x", "y")), list(), s2, "s"), "s")$unique
})
ok("set and counted paths agree", {
  l <- mk(c("a", "b"), c(1L, 2L)); r <- mk(c("a", "b"), c(1L, 9L))
  identical(compare_canonical(l, r, 5L, TRUE)$per_attribute$equal,
            compare_canonical(l, r, 5L, FALSE)$per_attribute$equal)
})

cat("\n-- wide/long bridging -------------------------------------------\n")
ok("wide melts to the long grain", {
  sp <- list(dataset = "t", grain_key = c("id", "h"),
             normalize = list(v = list(type = "number")), na_string = TRUE)
  wide <- tibble(id = "a", `1` = 10L, `2` = 20L)
  long <- tibble(id = c("a", "a"), h = c(1L, 2L), v = c(10L, 20L))
  w <- canonicalize(wide, list(reshape = list(
    cols = c("1", "2"), names_to = "h", values_to = "v")), sp, "w")
  compare_canonical(canonicalize(long, list(), sp, "l"), w, 5L, TRUE)$pass
})

cat("\n-- key handling -------------------------------------------------\n")
bij <- function(m) check_key_bijection(list(k = m))
ok("clean 1:1 map is bijective", bij(tibble(from = c("a", "b"), to = c("x", "y")))$bijective)
ok("two from -> one to is caught", !bij(tibble(from = c("a", "b"), to = c("x", "x")))$bijective)
ok("one from -> two to is caught", !bij(tibble(from = c("a", "a"), to = c("x", "y")))$bijective)
ok("no map declared is empty", nrow(check_key_bijection(list())) == 0L)
ok("malformed map errors", errs(bij(c(1, 2))))
ok("NA in a map errors", errs(bij(tibble(from = c("a", NA), to = c("x", "y")))))
ok("a rename needs no map", {
  sp <- list(dataset = "t", grain_key = "id", normalize = list(), na_string = TRUE)
  l <- canonicalize(tibble(OLD = "a", v = "x"), list(rename = c(OLD = "id")), sp, "l")
  compare_canonical(l, canonicalize(tibble(id = "a", v = "x"), list(), sp, "r"), 5L, TRUE)$pass
})
ok("transformed key values align via map", {
  sp <- list(dataset = "t", grain_key = "id",
             normalize = list(n = list(type = "number")), na_string = TRUE)
  km <- list(id = tibble(from = c("OLD1", "OLD2"), to = c("NEW1", "NEW2")))
  l <- canonicalize(tibble(id = c("OLD1", "OLD2"), n = c(10L, 20L)), list(key_map = km), sp, "l")
  r <- canonicalize(tibble(id = c("NEW1", "NEW2"), n = c(10L, 20L)), list(), sp, "r")
  compare_canonical(l, r, 5L, TRUE)$pass
})

cat("\n-- geopoint normalization ---------------------------------------\n")
gp <- function(x, ...) normalize_column(x, c(list(type = "geopoint"), list(...)))$value
# A real station from the data: ort_id 10921, whose portal export carries this
# WKB and whose join-CSV row carries the matching "lat, lon" text.
wkb_le <- "010100000016c1d9a59cbe2240ccf9fac2d1b64740"
pt_text <- "47.4282764173, 9.37228887828"
ok("WKB point decodes to lon,lat", identical(gp(wkb_le), "9.37228888,47.42827642"))
ok("WKB and \"lat, lon\" text agree", identical(gp(wkb_le), gp(pt_text)))
ok("order = lonlat is honoured", identical(
  gp("9.37228887828, 47.4282764173", order = "lonlat"), gp(wkb_le)))
ok("digits control the precision", identical(gp(wkb_le, digits = 2L), "9.37,47.43"))
ok("blank becomes NA", is.na(gp("")))
ok("raw-list (blob) input works", identical(
  gp(list(as.raw(strtoi(substring(wkb_le, seq(1, 41, 2), seq(2, 42, 2)), 16L)))),
  gp(wkb_le)))
ok("unreadable geometry errors", errs(gp("not-a-point")))
ok("non-point WKB errors", errs(gp(paste0("0102000000", strrep("00", 16)))))
ok("bad order errors", errs(gp(wkb_le, order = "sideways")))

cat("\n-- lookup annotation --------------------------------------------\n")
pairs <- tibble(known_final = "Y", new_output = "Y2", n_keys = 1L)
ok("no lookup leaves in_lookup NA", all(is.na(annotate_with_lookup(pairs)$in_lookup)))
ok("a reviewed pair is recognised", all(annotate_with_lookup(
  pairs, tibble(known_final = "Y", new_output = "Y2"))$in_lookup))
ok("an unexplained pair is flagged", !any(annotate_with_lookup(
  pairs, tibble(known_final = "Q", new_output = "Q2"))$in_lookup))
ok("a dead lookup entry is reported", nrow(attr(annotate_with_lookup(
  pairs, tibble(known_final = "Q", new_output = "Q2")), "unused_lookup")) == 1L)
ok("malformed lookup errors", errs(annotate_with_lookup(pairs, c(1, 2))))

cat("\n", strrep("-", 64), "\n", sep = "")
if (failures == 0L) {
  cat("all checks passed\n")
} else {
  cat(failures, " CHECK(S) FAILED\n", sep = "")
  quit(status = 1L)
}
