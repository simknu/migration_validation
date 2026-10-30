# migval — one-time migration validation harness

A **throwaway** R package that certifies a new R transformation reproduces an
old WYSIWYG transformation, one dataset at a time. No tests to maintain, no
future runs: it is built for a report you *read*, not a suite you keep.

**The core split, which must not blur:** the *guarantee* is a deterministic
full-data comparison in R — zero external calls, nothing uploaded. Any LLM
review is diagnosis only, never the guarantee. The old pipeline left no code,
so its only ground truth is its final output.

## The guarantee

Both the known-final output and the new transformation's output are reduced to
a shape-independent canonical form and compared as **multisets**:

1. Apply the rename map, so matching columns share one name.
2. Melt replicated measures to triples `(entity_key, attribute, value)`. A wide
   column whose name is a *key dimension* (the 24 hourly MIV columns → `stunde`)
   melts into the key; a wide column that is its own measure melts into
   `attribute`. This is what bridges wide ↔ long.
3. Normalize each value (below) and **drop** triples whose value is NA/empty.
4. Compare the **bag** of triples — counts per distinct triple, not sets.

**Pass = identical bags.** Order is never checked. Repeated
`(entity, attribute)` values are kept rather than deduplicated, so genuine
repeats are covered without an occurrence index.

The comparison is factored by attribute rather than materialising one 25M-row
triple table. This is not a weakening: `attribute` is part of every triple, so
triples with different attributes can never collide, and the bag of all triples
is equal iff the bag of `(entity_key, value)` is equal for every attribute. It
also yields the per-column verdict the coverage ledger needs anyway.

## Normalization

Default is **exact after normalization**; normalization removes only
deterministic representation noise.

| Type | Rule |
| --- | --- |
| String | UTF-8 enforced, trimmed, internal whitespace runs collapsed to one space |
| Missing | real `NA` and post-trim `""` both count as empty → dropped before comparison |
| Number | compared as numbers, not strings; exact by default, with a per-attribute `tolerance` hook |
| Date / datetime | parsed and compared as `Date`/`POSIXct`, never as strings |
| Binary | compared as lowercase hex |
| Geo point | WKB *and* `"lat, lon"` text both decoded to coordinates and rendered `"lon,lat"` — the two sides of a portal migration disagree on representation, not position |
| Literal `"NA"` | mapped to `NA` **on by default**; per-dataset switch; the report always states how many cells it converted |

Numbers are rendered with `sprintf("%.15g")`, which formats element-wise.
`format()` would pick one layout for the whole column, so two sides holding the
same values among different neighbours would disagree spuriously.

**Tolerance** is per attribute: `tolerance = 0.01` (absolute, shorthand) or
`tolerance = list(abs = 0.01, rel = 0.05)`. It is applied by **quantization** —
values are snapped to a grid and the grid points compared exactly — because bag
semantics give no pairwise row correspondence to subtract across. The
consequence is a boundary effect: two values closer than the tolerance can still
land in adjacent buckets (with `abs = 1`, `1.4999` and `1.5001` become `1` and
`2`). Tolerance here means *ignore differences below this scale*, not *pair up
rows and subtract*. Relative tolerance quantizes in log space, so the bucket
width scales with magnitude; zero has no relative neighbourhood and is left
alone.

Normalization is a per-attribute pluggable step, so swapping exact → tolerant on
one numeric column disturbs nothing else.

## Keys

A key that was only **renamed** needs nothing: the rename map is applied before
the grain key is read. A key whose **values** were transformed is the dangerous
case, and is declared per side:

```r
sides = list(known_final = list(key_map = list(
  ort_id = tibble::tibble(from = c("OLD1", "OLD2"), to = c("NEW1", "NEW2"))
)))
```

`check_key_bijection()` verifies the map is one-to-one before anything trusts
it. If two old key values reach one new value, rows collapse and every
row-level diff below becomes a lie while the bag compare still returns a
tidy-looking verdict — so drill-down is **refused** and the report says which
key column failed. The verdict itself still runs, because bag semantics keep
repeats.

## Layout

```
R/fetch.R              ODS client, local readers, resolve + freeze to .RDS
R/canonicalize.R       rename map, melt to triples, per-attribute normalize
R/verdict.R            multiset (bag) compare, grain-uniqueness guard
R/spec.R               content-matched draft lineage spec, schema diff
R/report.R             coverage ledger, drill-down printer
R/validate_dataset.R   entry point: resolve → canonicalize → verdict → report

configs/miv_stadt.R         per-dataset configs, plain R lists (two variants)
configs/miv_stadt_raw.R     the real certification case, from the raw .TXT
pipeline/                   the pipeline under test, vendored (see its README)
scripts/run_dataset.R       generic runner: takes a config, not a dataset
scripts/selfcheck.R         engine self-check on synthetic data (seconds)
snapshots/                  frozen .RDS snapshots (gitignored, rebuilt on first run)
data/                       the raw export and join CSV are gitignored and
                            supplied per run; the reference .parquet pair is tracked
reports/                    generated reports
```

A dataset's config is a plain R list: source, rename map, grain key, frozen
lineage spec, per-attribute normalization overrides, tolerance, `na_string`
switch. There is no master registry and no loop — you run one dataset, read the
report, move on.

## Running it

```bash
Rscript scripts/run_dataset.R configs/miv_stadt_raw.R   # certification: PASSES
Rscript scripts/run_dataset.R configs/miv_stadt.R       # reconciliation vs the old export
```

With no config function named, every `*_config` function the file defines runs
in turn; name one to pick it. A config declares its own `sources` (the pipeline
under test and its shims), so **a new dataset needs a config, not a script**.
Reports land in `reports/`.

Check the engine itself in a few seconds, without touching the data:

```bash
Rscript scripts/selfcheck.R
```

## Fetch layer

Both portals are Opendatasoft, so one `httr2` client with two base URLs:

| Portal | Base URL |
| --- | --- |
| `daten.sg.ch` | `https://daten.sg.ch/api/explore/v2.1` |
| `daten.stadt.sg.ch` | `https://daten.stadt.sg.ch/api/v2` |

- **Exports, never records.** Records caps the offset at 10'000 rows and would
  silently truncate; exports streams the whole dataset.
- **Parquet by default**, which preserves types and so keeps exact equality
  honest. CSV only where a dataset lacks Parquet; normalization then carries
  more weight.
- **API key** read from `ODS_API_KEY` and sent as `Authorization: Apikey <key>`.
  Never stored in code or config; the header is omitted when unset.
- **Fetch once, then freeze.** A resolved source is cached to `.RDS` with its
  fetch timestamp. Every later run reads the frozen snapshot, so the portal
  cannot shift the ground truth mid-migration and the comparison is always two
  in-memory R objects. A local original and an ODS original resolve to the same
  kind of object; nothing downstream knows the source.

## The report

The spine is a **coverage ledger**. Finding *missing* checks is a stated goal,
so the absence of a check is printed as loudly as a failure:

1. **Grain uniqueness**, checked first. If the grain is not unique on either
   side, row-level drill-down is **refused** rather than shown misleadingly — a
   silent many-to-one is the subtle bug that invalidates everything downstream.
   The verdict is unaffected and still runs, because bag semantics keep repeats.
2. **Entity-key roster** — compared on its own, so a roster difference is
   reported once instead of echoing through every column's verdict.
3. **Coverage ledger**, per output column: lineage class, which check covered
   it, matched on both sides, and NA treatment. Each column also gets
   `equal_on_shared_keys`, which separates *do the values agree where both sides
   have the row* from *do the rosters agree*.
4. **Uncovered gaps** — columns asserted by nothing in this run.
5. **Value pairs** for `changed` or failing columns, and whether a reviewed
   lookup exists or the column is "asserted by nothing".
6. **Schema-diff residue** — raw columns present on one side only.
7. **Draft lineage spec**, content-matched, for review.
8. **Verdict**, with a capped set of example diverging triples.
9. **Honest limits of the run.**

### Changed columns are not self-certifying

For a `changed` column the distinct `input → output` pairs are collected so
thousands of rows collapse to the few mappings actually present. The draft is
derived from the pipeline, so checking it against the pipeline is circular: the
pairs are printed for a human to compare against the lookup table, and **that
one-time review is the actual assertion of intent**.

## Honest limits

- Exact multiset equivalence for `renamed`/`unchanged` columns is a true
  guarantee.
- Exhaustive over the mapping inputs **present in the data**; an unseen future
  value is uncovered.
- `changed` columns are test-strength only, bounded by the reviewed lookup.
- A reshape is covered only where both sides reduce to the same triple grain.
- Row order is never checked — bag semantics by design.

## Known gap

Nothing verifies that a **tolerance** was set wide enough to be meaningful, or
narrow enough to be safe — that is a judgement the config author makes, and the
report states the tolerance in force rather than vouching for it.

## Dependencies

`httr2` (fetch), `dplyr` / `tidyr` / `tibble` (reshape and compare), `digest`
(hashing), `rlang`. `readr` is used only by the container bridge scripts and one
config fallback, not by the engine. `arrow` is a **soft** dependency, loaded lazily by
`read_parquet_source()`: the harness still runs where only frozen `.RDS`
snapshots exist. Nothing else is added.

Snapshots are **not** committed. `resolve_source()` rebuilds them from
`data/*.parquet` on the first run, which needs `arrow`; where `arrow` is
unavailable, build them with the two bridge scripts below instead.

## What the MIV run establishes

`configs/miv_stadt_raw.R` is the certification: `process_miv()` on the raw
export against the known-final. It **passes** — identical rosters and exact
multiset equality on all eight substantive columns across 1'275'792 rows, with
nothing stubbed, so the file reading and encoding detection are covered too.

`configs/miv_stadt.R` compares the *older published* export against the same
known-final and fails, correctly: those two genuinely differ. See `FINDINGS.md`.

## Container note: the Parquet → RDS bridge

On a normal machine the harness reads Parquet directly with `arrow`. The
container this was built in cannot reach CRAN and cannot build `arrow`, so the
two supplied Parquet files were bridged to `.RDS` once:

```bash
python3 -I scripts/parquet_to_tsv.py data/x.parquet /tmp/x.tsv /tmp/x.manifest.tsv
Rscript scripts/tsv_to_rds.R /tmp/x.tsv /tmp/x.manifest.tsv snapshots/x.rds
```

The Python side renders every column to a string and SHA-256s it; the R side
recomputes each checksum before casting. All 49 column checksums verified, so
**no finding in the report can be an artefact of the bridge**. Both scripts are
container-only and are not part of the package.

## Code style

Native pipe `|>` only, `<-` for assignment, `snake_case`, functions are verbs,
tibbles out. Small pure functions, side effects (fetch, print) isolated from
logic. Base `stop()`/`warning()` — no `cli`, no custom condition classes. No
testthat, no roxygen2 dependency: `NAMESPACE` is hand-written and roxygen
comments document exported functions only.
