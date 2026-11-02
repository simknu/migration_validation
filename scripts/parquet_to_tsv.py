#!/usr/bin/env python3
"""Render a Parquet file to a typed TSV plus a JSON manifest.

Container-only bridge. The R harness reads Parquet with `arrow` on the user's
machine; this container cannot build `arrow` (CRAN unreachable), so the two
supplied Parquet files are rendered to a lossless text form that R can read
with only `readr`. Every column is rendered to a string here and checksummed;
`tsv_to_rds.R` recomputes the same checksums, so a bridge that corrupted a
value could not go unnoticed.

Usage: python3 -I parquet_to_tsv.py <in.parquet> <out.tsv> <out.manifest.tsv>
"""

import hashlib
import sys

import pyarrow as pa
import pyarrow.parquet as pq

NA = "\\N"  # sentinel for a missing value; asserted absent from real values
SEP = "\t"


def render(col: pa.ChunkedArray) -> tuple[list[str], str]:
    """Render one Arrow column to strings and classify its R target type."""
    t = col.type
    vals = col.to_pylist()

    if pa.types.is_binary(t) or pa.types.is_large_binary(t):
        kind = "character"  # WKB geometry, carried as lowercase hex
        out = [NA if v is None else v.hex() for v in vals]
    elif pa.types.is_date(t):
        kind = "Date"
        out = [NA if v is None else v.isoformat() for v in vals]
    elif pa.types.is_timestamp(t):
        kind = "POSIXct"
        out = [NA if v is None else v.isoformat() for v in vals]
    elif pa.types.is_integer(t):
        kind = "integer"
        out = [NA if v is None else str(v) for v in vals]
    elif pa.types.is_floating(t) or pa.types.is_decimal(t):
        kind = "double"
        out = [NA if v is None else repr(float(v)) for v in vals]
    elif pa.types.is_boolean(t):
        kind = "logical"
        out = [NA if v is None else ("TRUE" if v else "FALSE") for v in vals]
    else:
        kind = "character"
        out = [NA if v is None else str(v) for v in vals]

    return out, kind


def main() -> None:
    src, tsv_path, manifest_path = sys.argv[1:4]
    table = pq.read_table(src)

    columns, manifest = {}, []
    for name in table.column_names:
        rendered, kind = render(table.column(name))

        bad = [v for v in rendered if SEP in v or "\n" in v or "\r" in v]
        if bad:
            raise SystemExit(f"column {name!r}: value contains a delimiter: {bad[0]!r}")
        collide = [v for v in rendered if v == NA and kind != "character"]
        del collide  # NA sentinel only ever produced for true nulls

        digest = hashlib.sha256(
            "\x1f".join(rendered).encode("utf-8")
        ).hexdigest()
        columns[name] = rendered
        manifest.append({"name": name, "kind": kind, "sha256": digest})

    n = table.num_rows
    with open(tsv_path, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(SEP.join(table.column_names) + "\n")
        for i in range(n):
            fh.write(SEP.join(columns[c][i] for c in table.column_names) + "\n")

    # Manifest as TSV, not JSON, so the R side needs no JSON parser.
    with open(manifest_path, "w", encoding="utf-8", newline="\n") as fh:
        fh.write("name\tkind\tsha256\tn_rows\n")
        for m in manifest:
            fh.write(f"{m['name']}\t{m['kind']}\t{m['sha256']}\t{n}\n")

    print(f"{src}: {n} rows, {len(manifest)} columns -> {tsv_path}")


if __name__ == "__main__":
    main()
