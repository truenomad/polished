# Build a unified QA report from a preprocessing output folder

Scans `polis_folder` (recursively) for the data-quality side files
written by a POLIS preprocessing run, counts the rows each check
flagged, and returns a tidy summary. Unknown files are ignored; missing
checks simply don't appear.

## Usage

``` r
polis_qa_report(polis_folder, registry = polis_qa_checks(), verbose = TRUE)
```

## Arguments

- polis_folder:

  Path to the folder the preprocessing run wrote to.

- registry:

  QA check registry (default
  [`polis_qa_checks()`](https://truenomad.github.io/polished/reference/polis_qa_checks.md));
  a tibble mapping filename patterns to a check name, domain, severity
  and description.

- verbose:

  Emit a cli report (default `TRUE`).

## Value

A list with:

- `summary`:

  Tibble: one row per check found, with `check`, `domain`, `severity`,
  `n_flagged` (rows), `n_files`, `description`.

- `files`:

  Tibble of every matched file and its row count.

- `meta`:

  Totals and the folder scanned.

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- polis_qa_report("data/polis")
qa$summary
} # }
```
