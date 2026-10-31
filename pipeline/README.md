# The pipeline under test

Vendored verbatim from the house package, so the harness compares against the
real code rather than a paraphrase of it. Sourced in this order by the configs'
`sources` field:

| File | Supplies |
| --- | --- |
| `00_input_check_fct.R` | the shared checkers (`check_file`, `check_string`, `check_character_vector`, …) |
| `import_export_utf8_fct.R` | the UTF-8 readers (`load_txt_utf8`, `load_csv_utf8`) and the encoding detection behind them |
| `00_helper_fct.R` | `check_duplicates`, `publish_huwise` and the portal helpers |
| `miv_stadt_fct.R` | the MIV pipeline itself |

`house_shims.R` is gone: every helper the MIV path reaches is now the real
implementation, so the **file reading and encoding detection are covered** — the
part that matters most here, since the encoding damage in the old published
dataset is exactly what the harness is being asked to adjudicate.

Two functions are never reached and so are never exercised: `publish_huwise()`
and `download_dataset()`. The harness compares frozen snapshots and does not
publish; `configs/*.R` call the read and transform functions only.

## Changes from what was supplied

Only changes you authorised, each recorded in `CHANGES.md`. The baseline is in
this repo's git history, so `git log -p pipeline/` shows exactly what moved.
Nothing here is ever edited to make a run pass.
