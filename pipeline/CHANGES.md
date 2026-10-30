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
