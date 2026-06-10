# Run the POLIS cleaning pipeline in memory

Cleans the raw POLIS tables supplied in `inputs` and returns the
canonical analytic set. Any subset of tables may be supplied; absent
tables are skipped. The virus (positives) table is *built* from the
cleaned AFP and ES streams via
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
whenever either is present – it is not read from `inputs`.

## Usage

``` r
run_pipeline(inputs, cfg = polis_config(), reconcile_with = NULL)
```

## Arguments

- inputs:

  Named list of raw POLIS data frames. Recognised names: `afp`, `es`,
  `activity`, `subactivity`. (A raw `virus` table is not used –
  positives are derived from the cleaned `afp`/`es` outputs.)

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)).

- reconcile_with:

  Optional named list of full-pull data frames (same keys as `inputs`)
  used to prune deleted/merged `id`s via
  [`reconcile()`](https://truenomad.github.io/polished/reference/reconcile.md).

## Value

A named list of cleaned tibbles: any of `afp`, `es`, `sia`, `virus`.

## Examples

``` r
afp <- data.frame(
  Id = 1, Epid = "A-1", `Last Update Date` = "2024-03-01",
  `Date Onset` = "2024-01-02", `Admin0 Name` = "NIGERIA",
  check.names = FALSE
)
out <- run_pipeline(list(afp = afp))
#> 
#> ── Cleaning AFP cases ──────────────────────────────────────────────────────────
#> ℹ Standardising names on 1 rows
#> ✔ Standardised names on 1 rows [22ms]
#> 
#> ℹ Parsing dates and deriving onset/age/intervals/timeliness
#> ✔ Parsed dates and derived onset/age/intervals/timeliness [18ms]
#> 
#> ℹ Classifying virus type and case classification
#> ✔ Classified virus type and case classification [13ms]
#> 
#> ℹ Standardising admin names
#> ✔ Standardised admin names [13ms]
#> 
#> ℹ Recovering missing admin from the EPID
#> ✔ Recovered admin for 0 cases from the EPID [13ms]
#> 
#> ℹ Enriching with country groupings and AFP flags
#> ✔ Enriched with country groupings and AFP flags [18ms]
#> 
#> ℹ Deduplicating by id and finalising
#> ✔ Deduplicated by id and finalised [16ms]
#> 
#> ✔ Cleaned 1 AFP cases.
#> 
#> ── Building virus / positives ──────────────────────────────────────────────────
#> ℹ Extracting poliovirus positives from cases and ES
#> ℹ No poliovirus positives found.
#> ℹ Extracting poliovirus positives from cases and ES
#> ✔ Extracted poliovirus positives from cases and ES [7ms]
#> 
#> ✔ Cleaned 1 dataset: "afp".
names(out)
#> [1] "afp"
```
