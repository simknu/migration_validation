# migval — one-time migration validation harness

A **throwaway** R harness that certifies a new R transformation reproduces an
old WYSIWYG one, one dataset at a time. Built for a report you *read*, not a
suite you keep.

The *guarantee* is a deterministic full-data comparison in R: zero external
calls, nothing uploaded. The old pipeline left no code, so its only ground
truth is its final output.

> The MIV Stadt St.Gallen run and its findings are in
> [`FINDINGS.md`](FINDINGS.md). The certification **passes**.

## What it does

Both sides are reduced to a shape-independent canonical form and compared as
**multisets** of triples `(entity_key, attribute, value)`:

1. Apply the rename map, so matching columns share one name.
2. Melt to triples. A wide column whose name is a *key dimension* (24 hourly
   columns → `hour`) melts into the **key**; a wide column that is its own
   measure melts into the **attribute**. This is what bridges wide ↔ long.
3. Normalize each value, and drop triples whose value is NA or empty.
4. Compare the **bag** of triples — counts per distinct triple, not sets.

**Pass = identical bags over the columns a check covers, plus identical
entity-key rosters.** Order is never checked. Repeats are kept rather than
deduplicated, so genuine repeats are covered without an occurrence index.

A column classed `new` or `dropped`, or present on one side only, has no ground
truth: it can neither pass nor fail, so it is a **gap**, not a verdict input.
The exclusion is never silent — the verdict names every excluded column and
says when one *does* differ.

## Vignette: validating a dataset

### What you need

| | What | Notes |
| --- | --- | --- |
| **Original** | the pre-transform input | a local file, or an ODS `dataset_id` |
| **Known-final** | the old tool's output — **ground truth** | its only truth, since the old pipeline left no code |
| **New pipeline** | the transformation under test | vendored into `pipeline/`, so the real code is tested |

Either data side may be `parquet`, `csv`, `rds`, an in-memory `object`, `ods`
(fetched and frozen), or `reader` — a function you supply, for an input whose
own pipeline owns the parsing.

### 1. Write a config

A config is a plain R list. Five fields are required, plus `sources`:

```r
my_dataset_config <- function() {
  list(
    dataset     = "my_dataset",                              # names the report
    sources     = c("pipeline/my_fct.R"),                    # sourced before the run
    original    = list(kind = "parquet", path = "data/in.parquet"),
    known_final = list(kind = "parquet", path = "data/truth.parquet"),
    transform   = function(original) my_pipeline(original),  # the code under test
    grain_key   = c("id", "date")                            # what makes a row unique
  )
}
```

Everything else is optional, needed only where the two sides don't line up:

| Field | For |
| --- | --- |
| `normalize` | per column: `number` (with `tolerance`), `date`, `datetime`, `hex`, `geopoint`, `string` |
| `sides$<side>$rename` | a column that changed name |
| `sides$<side>$reshape` | wide ↔ long: melts wide columns into a key dimension |
| `sides$<side>$key_map` | key *values* transformed — verified to be a bijection first |
| `lineage`, `lookup`, `lookup_reviewed` | lineage classes, and the reviewed lookup for `changed` columns |
| `ignore`, `na_string`, `examples_cap`, `limits`, `cache_dir` | per-dataset switches |

### 2. Run it

```bash
Rscript scripts/run_dataset.R configs/my_dataset.R
```

With no config function named, every `*_config` the file defines runs in turn.
**A new dataset needs a config, not a script.** The report lands in
`reports/my_dataset.txt`.

Check the engine itself in seconds, without any data:

```bash
Rscript scripts/selfcheck.R
```

### 3. Read the report

Its spine is a **coverage ledger**: finding *missing* checks is a goal, so an
absent check prints as loudly as a failure.

| § | Shows |
| --- | --- |
| 1 | **Grain & key integrity.** A non-unique grain, or a `key_map` that is not a bijection, **refuses** row-level drill-down rather than misleading. The verdict still runs. |
| 2 | **Entity-key roster**, on its own, so a roster difference is reported once instead of echoing through every column. |
| 3 | **Coverage ledger** per column: lineage class, check applied, NA treatment, and `equal_on_shared_keys` — which separates *values agree where both sides have the row* from *rosters agree*. Usually explains a failure. |
| 4 | **Uncovered gaps** — columns asserted by nothing. |
| 5 | **Value pairs** for `changed` or failing columns, and whether a reviewed lookup exists. |
| 6 | **Schema-diff residue** — raw columns on one side only. |
| 7 | **Draft lineage spec**, content-matched on shared keys. |
| 8 | **Verdict**, naming every excluded column, with example diverging triples. |
| 9 | **Honest limits** of that run. |

Leave `lineage` out of the first run: §7 drafts it for you. Review it once,
paste it in, and from then on the ledger judges each column against your stated
intent instead of guessing.

A first FAIL is normal and informative. `changed` columns stay "asserted by
nothing" until you supply a reviewed lookup — the pairs are derived from the
pipeline, so checking them against the pipeline would be circular. **That
one-time human review is the actual assertion of intent.**

### Under the hood: the same comparison, function by function

`validate_dataset()` chains the exported functions below; calling them directly
is how you inspect a comparison. Here the ground truth is long and the
pipeline's output is wide — and `label` carries stray whitespace.

```r
suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

known_final <- tibble::tibble(
  station = c("A", "A", "B", "B"),
  hour    = c(1L, 2L, 1L, 2L),
  count   = c(10L, 20L, 30L, 40L),
  label   = c("Alpha", "Alpha", "Beta", "Beta")
)

new_output <- tibble::tibble(
  station = c("A", "B"),
  `1`     = c(10L, 30L),
  `2`     = c(20L, 40L),
  label   = c("Alpha", "  Beta ")     # stray whitespace
)

spec <- list(
  dataset   = "demo",
  grain_key = c("station", "hour"),
  normalize = list(count = list(type = "number"))
)
```

**`canonicalize()`** reduces each side to triples. The wide side melts its
hour columns into the *key*, so both sides reach the same grain — this is the
wide ↔ long bridge:

```r
left  <- canonicalize(known_final, list(), spec, "known_final")
right <- canonicalize(
  new_output,
  list(reshape = list(cols = c("1", "2"), names_to = "hour", values_to = "count")),
  spec, "new_output"
)

left$attributes$count       #> entity_key "A\x1f1", value "10"; 4 rows
```

**`check_grain_unique()`** runs before any drill-down, and guards it:

```r
bind_rows(check_grain_unique(left, "known_final"),
          check_grain_unique(right, "new_output"))
#>   side        n_rows n_distinct_keys n_duplicated unique
#> 1 known_final      4               4            0 TRUE
#> 2 new_output       4               4            0 TRUE
```

**`compare_canonical()`** is the guarantee itself. `label` passes despite the
stray whitespace, because normalization collapsed it:

```r
cmp <- compare_canonical(left, right, examples_cap = 5L, unique_grain = TRUE)
select(cmp$per_attribute, attribute, equal, n_left_only, n_right_only)
#>   attribute equal n_left_only n_right_only
#> 1 count     TRUE            0            0
#> 2 label     TRUE            0            0
cmp$pass                    #> TRUE
```

**`draft_lineage_spec()`** proposes the spec by matching column *content* on
shared keys rather than names, so a rename is detected; **`coverage_ledger()`**
turns the result into the report's spine:

```r
draft_lineage_spec(left, right, cmp$keys$shared)
#>   column class     matched_to evidence
#> 1 label  unchanged label      same name, identical bag on shared keys
#> 2 count  unchanged count      same name, identical bag on shared keys

lineage <- tibble::tribble(~column, ~class, "count", "unchanged", "label", "unchanged")
coverage_ledger(cmp, lineage, left$diagnostics, right$diagnostics)
#>   attribute class     covered equal ...
```

**`normalize_column()`** is the per-attribute step, usable on its own:

```r
normalize_column("  Beta ")$value                            #> "Beta"
normalize_column(c(1, 2.5), list(type = "number"))$value     #> "1" "2.5"
normalize_column(1.004, list(type = "number",
                 tolerance = list(abs = 0.01)))$value         #> "1"
normalize_column("47.4282764173, 9.37228887828",
                 list(type = "geopoint"))$value  #> "9.37228888,47.42827642"
```

The rest: `resolve_source()` (resolve and freeze a source),
`check_key_bijection()`, `annotate_with_lookup()`, `changed_value_pairs()`,
`diff_schemas()`, `bag_fingerprint()`, `fetch_ods_dataset()`,
`print_report()`.

## Normalization

Default is **exact after normalization**; normalization removes only
deterministic representation noise.

| Type | Rule |
| --- | --- |
| String | UTF-8 enforced, trimmed, internal whitespace runs collapsed to one space |
| Missing | real `NA` and post-trim `""` both count as empty → dropped |
| Number | compared as numbers, not strings; exact by default, per-attribute `tolerance` hook |
| Date / datetime | parsed and compared as `Date`/`POSIXct`, never as strings |
| Binary | lowercase hex |
| Geo point | WKB *and* `"lat, lon"` text decoded to coordinates, rendered `"lon,lat"` — the two sides of a portal migration disagree on representation, not position |
| Literal `"NA"` | mapped to `NA` by default; per-dataset switch; the report states how many cells it converted |

Two details that matter:

- Numbers use `sprintf("%.15g")`, which formats **element-wise**. `format()`
  picks one layout per column, so two sides holding the same values among
  different neighbours would disagree spuriously.
- **Tolerance** (`0.01`, or `list(abs = 0.01, rel = 0.05)`) works by
  **quantization** — values snap to a grid, grid points compare exactly —
  because bag semantics give no pairwise row correspondence to subtract across.
  So there is a boundary effect: with `abs = 1`, `1.4999` and `1.5001` land in
  different buckets. Tolerance means *ignore differences below this scale*, not
  *pair up rows and subtract*.

## Keys

A key that was only **renamed** needs nothing — the rename map is applied
before the grain key is read. A key whose **values** changed is declared per
side and verified first:

```r
sides = list(known_final = list(key_map = list(
  id = tibble::tibble(from = c("OLD1", "OLD2"), to = c("NEW1", "NEW2"))
)))
```

If two old values reach one new value, rows collapse: every row-level diff
becomes a lie while the bag compare still looks tidy. So `check_key_bijection()`
must vouch for the map, and drill-down is refused when it cannot.

## Honest limits

- Exact multiset equivalence for `unchanged`/`renamed` columns is a true
  guarantee.
- Exhaustive over mapping inputs **present in the data**; an unseen future
  value is uncovered.
- `changed` columns are test-strength only, bounded by the reviewed lookup.
- A reshape is covered only where both sides reduce to the same triple grain.
- Row order is never checked — bag semantics by design.
- The verdict covers only columns with an applicable check; the rest are
  reported as gaps, by name.
- Nothing verifies a **tolerance** is wide enough to be meaningful or narrow
  enough to be safe. The report states the tolerance in force rather than
  vouching for it.

## Layout

```
R/fetch.R              ODS client, local readers, resolve + freeze to .RDS
R/canonicalize.R       rename, melt to triples, per-attribute normalize
R/verdict.R            multiset (bag) compare, grain-uniqueness guard
R/keys.R               key-value maps and their bijection check
R/spec.R               content-matched draft lineage spec, lookup annotation
R/report.R             coverage ledger and printer
R/validate_dataset.R   entry point: resolve → canonicalize → verdict → report

configs/               per-dataset configs, plain R lists
pipeline/              the code under test, vendored (see pipeline/README.md)
scripts/               runner, self-check, container bridge (see scripts/README.md)
data/ snapshots/       gitignored; inputs supplied per run, snapshots rebuilt
reports/               generated reports
```

There is no master registry and no loop: you run one dataset, read the report,
move on.

## Dependencies

`httr2` (fetch), `dplyr` / `tidyr` / `tibble` (reshape and compare), `digest`
(hashing), `rlang`. `arrow` is a **soft** dependency, loaded lazily, so the
harness runs where only frozen `.RDS` snapshots exist. `readr` is used by the
bridge scripts and one config fallback, not by the engine. Nothing else.

Conventions: native pipe `|>`, `snake_case`, tibbles out, base
`stop()`/`warning()` — no `cli`, no custom conditions. No testthat and no
roxygen2 dependency: `NAMESPACE` is hand-written, and `scripts/selfcheck.R`
takes the place of a test suite.
