# Reviewed lookup table for `bezeichnung`.
#
# WHAT THIS IS, precisely: an ACCEPTANCE LIST. Each row says "I have looked at
# this divergence and I accept it." The harness then reports every observed
# divergence as either reviewed or unexplained, and flags rows here that never
# occur in the data.
#
# WHAT IT IS NOT: a rename map. It changes no value, fixes no name and cannot
# make a verdict pass. This harness only ever reads data and reports; nothing in
# it writes or republishes. To change a published name you change the source
# data or the pipeline, not this file.
#
# TO CONFIRM a divergence you accept: keep its row.
# TO REJECT one: delete its row. The report then lists it as NOT in the reviewed
#   lookup -- that is the point, because an unexplained divergence should stay
#   visible until someone deals with it.
# A row whose pair no longer occurs is reported as a dead entry, which catches
#   typos and names fixed upstream since the review.
#
# Both columns must match the pair AS THE REPORT PRINTS IT: `known_final` is the
# value on the known-final side, `new_output` the value the pipeline produced.
# Values are normalized the same way the data is before matching -- UTF-8,
# trimmed, internal whitespace collapsed -- so encoding marks and stray spaces
# do not matter, but the words must agree.
#
# WIRING IT IN -- the table does nothing until the config references it. In
# configs/miv_stadt.R, uncomment:
#
#   lookup = list(bezeichnung = bezeichnung_lookup()),
#   lookup_reviewed = list(bezeichnung = TRUE, lnr = FALSE),
#
# Left off deliberately while the table is unreviewed: wiring in an unreviewed
# table would make the report claim an assertion nobody made.
#
# NOTE: this is NOT needed to certify the pipeline. configs/miv_stadt_raw.R
# passes with `bezeichnung` matching exactly, because the raw source is clean.
# This table only matters for reconciling the OLD published export against the
# new one. See FINDINGS.md section 3 for what each pair is: mojibake,
# 30-character truncation, damage in the known-final, or a real label change.

bezeichnung_lookup <- function() {
  tibble::tribble(
    ~known_final,                            ~new_output,

    # ---- mojibake in the old published export: the known-final is right -----
    "St.Gallen Stadt Fürstenlstr. 57",  "St.Gallen Stadt FÃ¼rstenlstr. 57",
    "St.Gallen Stadt Mühlegg",          "St.Gallen Stadt MÃ¼hlegg",
    "St.Gallen Stadt Müller-Fried.2",   "St.Gallen Stadt MÃ¼ller-Fried.2",
    "St.Gallen Stadt Splügen/Bachst",   "St.Gallen Stadt SplÃ¼gen/Bachst",

    # ---- old export truncated at 30 characters, losing a trailing char ------
    "St.Gallen Stadt Grossackers. 14",       "St.Gallen Stadt Grossackers. 1",
    "St.Gallen Stadt Schwarzer Bären",  "St.Gallen Stadt Schwarzer BÃ¤re",

    # ---- the KNOWN-FINAL is the damaged side here; also damaged at source ---
    "St.Gallen Stadt R?elibr?ke",            "St.Gallen Stadt RÃ¶telibrÃ¼cke",

    # ---- genuine label differences: decide which convention you want --------
    "St.Gallen Gallusst./Webergasse",        "St.Gallen Stadt Gallusst./Webergasse",
    "St.Gallen Herisauer/Schranke",          "St.Gallen Stadt Herisauer/Schranke",
    "Stadt SG Flurhofstr 68",                "St.Gallen Stadt Flurhofstr. 68"
  )
}
