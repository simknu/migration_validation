# Deviations from the supplied `miv_stadt_fct.R`

The pipeline is vendored so the harness compares against the real code rather
than a paraphrase of it. It is kept verbatim apart from the change below, which
you authorised after the harness reported the divergence. Nothing here is an
edit made to help a run pass: the baseline is in this repo's git history, so
`git log -p pipeline/miv_stadt_fct.R` shows exactly what changed.

## 1. `derive_miv_cols()`: `kennzahl` separator

The harness found that `derive_miv_cols()` emitted a separator the published
data does not use, affecting **911'928 keys** (71% of the dataset). Left alone,
the next `publish_miv()` or `update_miv()` run from raw `.TXT` would silently
flip `kennzahl` on every `Werktage` row.

```diff
       kennzahl = dplyr::if_else(
         .data$arbeitstag == "Wochenende",
         "Durchschnittlicher Tagesverkehr (DTV)",
-        "Durchschnittlicher Tagesverkehr (DTV); Durchschnittlicher Werktagsverkehr (DWV)"
+        "Durchschnittlicher Tagesverkehr (DTV),Durchschnittlicher Werktagsverkehr (DWV)"
       ),
```

Apply the same one-line change in the house package. After it,
`configs/miv_stadt_derive.R` reports `kennzahl` as matching on every shared
key, which is the verification that the fix is complete.

## 2. `miv_schema()$key`: drop `lnr`

```diff
-    key = c("lnr", "ort_id", "datum", "richtung", "stunde"),
+    key = c("ort_id", "datum", "richtung", "stunde"),
```

`lnr` is a running number assigned per export, not an identity. The harness
established that the same records carry different `lnr` values in different
exports: `miv_original.parquet` numbers it 0–4'017 as a per-station counter
(unique within `ort_id`), while `MIV_2024_open_data.txt` numbers it 0–53'157 as
a global sequence. Keying on it had two consequences, both silent:

1. **`update_miv()` provenance.** Provenance is matched on this key
   (`left_join(prov, by = key)`), so a renumbered `lnr` would match nothing,
   and `erstpublikation` would be reset to the run date for records that were
   in fact already published.
2. **`publish_miv()` overlap detection.** The append guard is
   `semi_join(new_long, published_long, by = key)`. With `lnr` in the key,
   re-publishing the same station-days from a renumbered export would find *no*
   overlap, pass the guard as genuinely new records, and append duplicates.
   `check_duplicates(upload, key_cols = key)` would not catch them either,
   since the `lnr` values differ.

One edit covers both, plus `process_miv()`'s own duplicate assertion, because
all seven uses read `miv_schema()$key`.

`(ort_id, datum, richtung, stunde)` is unique in every file seen here: 54'478
rows in the old published export and 53'158 × 24 in the 2024 raw export, with
zero duplicates on either side. `lnr` remains a published *column* — only its
role as a key is removed.
