# Reviewed lookup table for `bezeichnung` -- SUPPLY THE AUTHORITATIVE NAMES HERE.
#
# `bezeichnung` is a `changed` column: it has no ground truth, because the two
# sides disagree and neither is uniformly right. The harness collects the
# distinct pairs it observes; this file is where you record which pairs you have
# reviewed and accept. That review is the assertion of intent -- the harness
# cannot make it for you, and says so in the report.
#
# Rows below are the 10 pairs the first run observed, left UNREVIEWED. Keep a
# row once you have confirmed which side is authoritative, and delete the rest.
# Any observed pair missing from this table is reported as unexplained, and any
# row here that never occurs in the data is reported as dead.
#
# For context on each pair -- mojibake, 30-character truncation, damage in the
# known-final, or a genuine label change -- see FINDINGS.md section 3.

bezeichnung_lookup <- function() {
  tibble::tribble(
    ~known_final,                           ~new_output,                            # note
    "St.Gallen Stadt Fürstenlstr. 57", "St.Gallen Stadt FÃ¼rstenlstr. 57",  # mojibake in original
    "St.Gallen Stadt Mühlegg",         "St.Gallen Stadt MÃ¼hlegg",          # mojibake in original
    "St.Gallen Stadt Müller-Fried.2",  "St.Gallen Stadt MÃ¼ller-Fried.2",   # mojibake in original
    "St.Gallen Stadt Splügen/Bachst",  "St.Gallen Stadt SplÃ¼gen/Bachst",   # mojibake in original
    "St.Gallen Stadt Grossackers. 14",      "St.Gallen Stadt Grossackers. 1",              # original truncated at 30
    "St.Gallen Stadt Schwarzer Bären", "St.Gallen Stadt Schwarzer BÃ¤re",   # original truncated at 30
    "St.Gallen Stadt R?elibr?ke",           "St.Gallen Stadt RÃ¶telibrÃ¼cke", # known-final damaged
    "St.Gallen Gallusst./Webergasse",       "St.Gallen Stadt Gallusst./Webergasse",        # label change
    "St.Gallen Herisauer/Schranke",         "St.Gallen Stadt Herisauer/Schranke",          # label change
    "Stadt SG Flurhofstr 68",               "St.Gallen Stadt Flurhofstr. 68"               # label change
  )
}
