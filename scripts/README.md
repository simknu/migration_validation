# Scripts

| Script | Purpose |
| --- | --- |
| `run_dataset.R` | the runner. Takes a config, not a dataset: `Rscript scripts/run_dataset.R configs/<name>.R [<fn>]` |
| `selfcheck.R` | exercises the engine on synthetic data in seconds. Run it after touching `R/` |
| `parquet_to_tsv.py` | container-only bridge, step 1 (see below) |
| `tsv_to_rds.R` | container-only bridge, step 2 |

## The Parquet → RDS bridge

Not part of the package, and not needed on a normal machine: there the harness
reads Parquet directly with `arrow`, and `resolve_source()` freezes the
snapshot itself.

The container this was built in cannot reach CRAN and cannot build `arrow`, so
Parquet inputs were bridged to `.RDS` once:

```bash
python3 -I scripts/parquet_to_tsv.py data/x.parquet /tmp/x.tsv /tmp/x.manifest.tsv
```

```bash
Rscript scripts/tsv_to_rds.R /tmp/x.tsv /tmp/x.manifest.tsv snapshots/x.rds
```

The Python side renders every column to a string and SHA-256s it; the R side
recomputes each checksum before casting to the declared type. All 49 column
checksums verified on the MIV inputs, so **no finding in any report can be an
artefact of the bridge** — which is the only reason a text bridge is acceptable
under a harness whose whole point is exact comparison.
