# Process raw LQAS lots into classifications and district pass rates

Cleans the raw POLIS `Lqas` table with the same recipe as
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
/
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
– standardise names via the crosswalk, clean strings, parse and sanitise
dates, fix admin names and (when a `shape` is supplied) reconcile admin
GUIDs, then dedup by `id` – and rolls the cleaned lots up to a
per-district-year table, classifying each lot **two ways**:

- *POLIS* – the classification POLIS already ships in the download
  (`lqas2_classification_name` / `lqas3_classification_name`), derived
  from its unexposed `REF_LQASThresholds` lookup. This is the faithful
  answer.

- *derived* – a transparent re-derivation from coverage
  (`1 - children_found_unvaccinated / children_checked`) against
  `pass_threshold` / `warn_threshold`, plus the documented default-60
  and 2019 "sample size must be a multiple of 60 or INVALID" rules.
  Provided for QA comparison against the POLIS classes.

## Usage

``` r
process_lqas(
  lqas,
  cfg = polis_active_config(),
  shape = NULL,
  default_checked = 60,
  multiple_of = 60,
  enforce_since = 2019,
  pass_threshold = 0.9,
  warn_threshold = 0.8,
  verbose = TRUE,
  summary = TRUE
)
```

## Arguments

- lqas:

  Raw LQAS table (data.frame/tibble), one row per lot.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md));
  its `crosswalk` maps the raw column names to canonical snake_case.

- shape:

  Optional district shape (an `sf` polygon layer or a long ADM2
  attribute table) for admin GUID reconciliation via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  (keyed on `year`). Default `NULL`.

- default_checked:

  Lot size assumed when the sample size is missing (default `60`, per
  POLIS).

- multiple_of:

  Sample-size modulus enforced from `enforce_since` (default `60`).

- enforce_since:

  Year from which the multiple-of rule makes a lot INVALID in the
  *derived* classification (default `2019`).

- pass_threshold:

  Coverage (vaccinated fraction) at/above which a lot is a Pass in the
  *derived* classification (default `0.90`).

- warn_threshold:

  Coverage band for the derived 3-level "Intermediate" class (default
  `0.80`). Lots in `[warn_threshold, pass_threshold)` are Intermediate
  (3-level) / Fail (2-level). Set to `NULL` to disable the Intermediate
  band, collapsing the derived classes to strict Pass/Fail.

- verbose:

  Emit cli progress + the one-line roll-up result (default `TRUE`).

- summary:

  Emit the full cli summary panel (rules + class breakdown). Default
  `TRUE`;
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  passes `FALSE` to keep the pipeline terse.

## Value

A list with `lots` (the cleaned lot-level grain carrying both
`lqas2_polis` / `lqas3_polis` and the derived `coverage` / `invalid` /
`lqas2_derived` / `lqas3_derived`), `district` (per-district roll-up
with `n_lots`, the POLIS `n_pass_polis` / `n_fail_polis` /
`n_invalid_polis` / `pass_pct_polis` and the derived `n_pass_derived` /
`n_fail_derived` / `n_invalid_derived` / `pass_pct_derived`), and
`meta`.

## Examples

``` r
lqas <- data.frame(
  Id = 1L, Admin0Name = "NIGERIA", Admin1Name = "KANO",
  Admin2Name = "NASSARAWA", Admin2GUID = "{A2}", Year = 2024L,
  ChildrenChecked = 60L, ChildrenFoundUnvaccinated = 3L,
  Lqas2ClassificationName = "Pass", Lqas3ClassificationName = "Pass"
)
process_lqas(lqas, verbose = FALSE)$district
#> # A tibble: 1 × 14
#>    year adm2_guid adm0    adm1  adm2      n_lots n_pass_polis n_fail_polis
#>   <int> <chr>     <chr>   <chr> <chr>      <int>        <int>        <int>
#> 1  2024 {A2}      NIGERIA KANO  NASSARAWA      1            1            0
#> # ℹ 6 more variables: n_invalid_polis <int>, n_pass_derived <int>,
#> #   n_fail_derived <int>, n_invalid_derived <int>, pass_pct_polis <dbl>,
#> #   pass_pct_derived <dbl>
```
