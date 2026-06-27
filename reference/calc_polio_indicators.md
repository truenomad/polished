# Calculate polio surveillance indicators (the POLIS indicator catalogue)

Computes the WHO POLIS surveillance-quality indicator catalogue from
cleaned analytic tables (the outputs of
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md),
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md),
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md),
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
and
[`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md))
at country (`adm0`), province (`adm1`) and district (`adm2`) level, by
year. Each indicator is a registry spec; the full catalogue is
discoverable with
[`available_indicators()`](https://truenomad.github.io/polished/reference/available_indicators.md).

## Usage

``` r
calc_polio_indicators(
  cases,
  population = NULL,
  virus = NULL,
  es = NULL,
  sia = NULL,
  lab = NULL,
  admin_units = NULL,
  indicators = "core",
  levels = c("adm0", "adm1", "adm2"),
  cols = list(),
  class_var = "classification_all",
  age_var = "age_months",
  year_var = "year_onset",
  onset_date_var = "paralysis_onset_date",
  adm0_guid_var = "adm0_guid",
  adm1_guid_var = "adm1_guid",
  adm2_guid_var = "adm2_guid",
  adm0_name_var = "adm0",
  adm1_name_var = "adm1",
  adm2_name_var = "adm2",
  adequacy_var = "adequate_stool",
  invest_interval_var = "notify_to_invest",
  npafp_classes = "NPAFP",
  pending_classes = c("PENDING", "LAB PENDING"),
  include_pending = TRUE,
  afp_exclude_classes = "NOT-AFP",
  pop_guid_var = "adm2_guid",
  pop_year_var = "year",
  pop_var = "u15_pop",
  rate_multiplier = 1e+05,
  npafp_target = 3,
  npafp_warn = 2,
  adequacy_target = 80,
  adequacy_warn = 60,
  invest_timely_days = 2,
  survindcat_rate_cutoff = 2,
  min_pop = 1e+05,
  min_cases = 10,
  reference_date = Sys.Date(),
  verbose = TRUE,
  summary = TRUE
)
```

## Arguments

- cases:

  Cleaned AFP case data, one row per case (data.frame/tibble).

- population:

  Optional under-15 population denominators, one row per admin unit per
  year (`pop_guid_var`, `pop_year_var`, `pop_var`). Required for the
  rate / district indicators; absent -\> those are skipped.

- virus, es, sia, lab:

  Optional cleaned analytic tables for the virus,
  environmental-surveillance, SIA and human-specimen (lab) indicator
  families. Absent -\> those families are skipped with a warning.

- admin_units:

  Optional universe of expected district admin units: a data frame with
  columns `adm2_guid`, `adm0_guid`, optionally `adm1_guid`. Build it
  from a district shape with `create_long_shape(shape, "adm2")` or
  `dplyr::distinct(sf::st_drop_geometry(shape), adm0_guid, adm1_guid, adm2_guid)`.
  Required for the silent-districts indicator; absent -\> it is skipped.
  ([`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  derives this from `cfg$shape` automatically.)

- indicators:

  Which indicators to compute: the keyword `"core"` (default – the core
  KPI set: NPAFP rate, condition-aware stool adequacy, EV detection
  rate, and timely detection), `"all"` (the full registered catalogue),
  or an explicit character vector of indicator codes. Anything whose
  source/columns are unavailable is skipped with a warning. See
  `available_indicators(core_only = TRUE)` for the core set.

- levels:

  Admin levels to report at: any of `"adm0"`, `"adm1"`, `"adm2"`.

- cols:

  Named list overriding default source-column mappings, e.g.
  `list(cases = list(class = "my_class"))`. See
  `polished:::.polio_default_cols()` for the full default map.

- class_var, age_var, year_var, onset_date_var:

  Case columns (back-compat shortcuts that override `cols$cases`).

- adm0_guid_var, adm1_guid_var, adm2_guid_var:

  Case GUID columns per level.

- adm0_name_var, adm1_name_var, adm2_name_var:

  Case admin-name columns.

- adequacy_var:

  Case column flagging adequate stool (timing-based).

- invest_interval_var:

  Case column with notification-\>investigation days.

- npafp_classes, pending_classes:

  Classification values counted as NPAFP / pending (matched
  case-insensitively).

- include_pending:

  If `TRUE` (default) pending cases count in the NPAFP numerator
  (`ufn_Indicator_NPAFP_RATE`); the `_nopending` variant is separate.

- afp_exclude_classes:

  Classifications excluded from the AFP denominator for percentage
  indicators (default `"NOT-AFP"`).

- pop_guid_var, pop_year_var, pop_var:

  Column names in `population` (defaults `"adm2_guid"`, `"year"`,
  `"u15_pop"`).

- rate_multiplier:

  Population scale for rates (default `1e5`).

- npafp_target, npafp_warn:

  Good / warn thresholds for NPAFP rate.

- adequacy_target, adequacy_warn:

  Thresholds for percentage indicators.

- invest_timely_days:

  Max notification-\>investigation days that count as timely (default
  `2`).

- survindcat_rate_cutoff:

  Policy NPAFP-rate cutoff used by `survindcat` (default `2`; WHO uses
  region/endemic-specific cutoffs – override per run).

- min_pop:

  Population below which a rate is flagged low-confidence.

- min_cases:

  Case count below which a percentage is flagged low-confidence.

- reference_date:

  Date capping the annualisation period (default today).

- verbose:

  Emit cli progress + the key one-line steps (default `TRUE`).

- summary:

  Emit the full cli summary panel (coverage-by-family report). Default
  `TRUE`;
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  passes `FALSE` to keep the pipeline terse.

## Value

A named list with per-level wide tibbles (`adm0`, `adm1`, `adm2`), a
tidy `long` tibble (one row per level x admin x year x indicator with
`value`, `numerator`, `denominator`, `confidence`, `category`,
`text_code`, `family`), a `meta` list (indicators, skipped indicators
with reasons, levels, thresholds, `reference_date`), and a `validation`
tibble: one row per automatically-run sense check (`check`, `severity`,
`n_flagged`, `description`) flagging out-of-range values,
numerator/denominator and value-vs-formula problems, duplicate keys,
blank admin keys, and level numerator-conservation gaps.

## Details

Indicators whose source table or required columns are absent are
**skipped with a warning** rather than erroring, so a partial schema
(e.g. `cases` + `population` only) still computes every applicable
indicator. Rates that need a population denominator require a
`population` table of *under-15* population per admin unit per year.

## Examples

``` r
# \donttest{
# available_indicators() lists the full catalogue without running anything.
available_indicators()
#> # A tibble: 62 × 17
#>    code            label family kind  core  formula numerator denominator source
#>    <chr>           <chr> <chr>  <chr> <lgl> <chr>   <chr>     <chr>       <chr> 
#>  1 afp_count       AFP … AFP    count FALSE COUNT(… AFP cases (count)     cases 
#>  2 npafp_count     NPAF… AFP    count FALSE COUNT(… NPAFP ca… (count)     cases 
#>  3 npafp_rate      NPAF… AFP    rate  TRUE  annual… Non-poli… Under-15 p… cases 
#>  4 npafp_rate_nop… NPAF… AFP    rate  FALSE annual… Non-poli… Under-15 p… cases 
#>  5 stool_adequacy… Stoo… Stool  perc… FALSE 100 * … AFP case… AFP cases   cases 
#>  6 stool_adequacy… Stoo… Stool  perc… TRUE  100 * … AFP case… Cases code… cases 
#>  7 stool_adequacy… Stoo… Stool  perc… FALSE 100 * … AFP case… Assessable… cases 
#>  8 afp_dose_0      AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#>  9 afp_dose_1_2    AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#> 10 afp_dose_3plus  AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#> # ℹ 52 more rows
#> # ℹ 8 more variables: period_basis <chr>, levels <chr>, requires_pop <lgl>,
#> #   target <dbl>, warn <dbl>, unit <chr>, polis_fn <chr>, notes <chr>
# }
```
