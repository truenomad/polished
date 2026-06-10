# Calculate polio surveillance indicators (NPAFP rate, stool adequacy, ...)

Computes the headline AFP surveillance indicators from a cleaned case
table (e.g. the output of
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md))
at country (`adm0`), province (`adm1`) and district (`adm2`) level, by
year. Rates that need a population denominator (NPAFP rate, % districts
NPAFP \>= 2) require a `population` table of *under-15* population per
admin unit per year.

## Usage

``` r
calc_polio_indicators(
  cases,
  population = NULL,
  indicators = c("npafp_rate", "stool_adequacy_pct", "inv_timeliness_pct",
    "pct_districts_npafp_ge2"),
  levels = c("adm0", "adm1", "adm2"),
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
  pop_guid_var = "guid",
  pop_year_var = "year",
  pop_var = "pop",
  rate_multiplier = 1e+05,
  npafp_target = 3,
  npafp_warn = 2,
  adequacy_target = 80,
  adequacy_warn = 60,
  invest_timely_days = 2,
  min_pop = 1e+05,
  min_cases = 10,
  reference_date = Sys.Date(),
  verbose = TRUE
)
```

## Arguments

- cases:

  Cleaned AFP case data, one row per case (data.frame/tibble).

- population:

  Optional under-15 population denominators, one row per admin unit per
  year, with columns named by `pop_guid_var`, `pop_year_var`, `pop_var`.
  Required for `npafp_rate` and `pct_districts_npafp_ge2`; if absent
  those indicators are skipped with a message.

- indicators:

  Character vector of indicators to compute (default: all).

- levels:

  Admin levels to report at: any of `"adm0"`, `"adm1"`, `"adm2"`.

- class_var, age_var, year_var, onset_date_var:

  Case columns: final classification, age in months, onset year, onset
  date.

- adm0_guid_var, adm1_guid_var, adm2_guid_var:

  Case GUID columns per level.

- adm0_name_var, adm1_name_var, adm2_name_var:

  Case admin-name columns.

- adequacy_var:

  Case column flagging adequate stool (logical / 0-1 / "Yes"-"No").

- invest_interval_var:

  Case column with notification-\>investigation days (default
  `"notify_to_invest"`, as produced by
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)).

- npafp_classes:

  Values of `class_var` that count as non-polio AFP (default `"NPAFP"`).
  Matched case-insensitively.

- pending_classes:

  Values treated as pending (default `c("PENDING", "LAB PENDING")`).

- include_pending:

  If `TRUE` (default) pending cases are included in the NPAFP numerator,
  matching `ufn_Indicator_NPAFP_RATE`. Set `FALSE` for the `_NOPENDING`
  variant.

- afp_exclude_classes:

  Classifications excluded from the AFP denominator for percentage
  indicators (default `c("NOT-AFP")`).

- pop_guid_var, pop_year_var, pop_var:

  Column names in `population`.

- rate_multiplier:

  Population scale for the rate (default `1e5`).

- npafp_target, npafp_warn:

  Good / warning thresholds for NPAFP rate (default `3` / `2` per
  100,000).

- adequacy_target, adequacy_warn:

  Thresholds for stool adequacy % and timeliness % (default `80` /
  `60`).

- invest_timely_days:

  Max notification-\>investigation days considered timely (default `2`).

- min_pop:

  Population below which a rate is flagged low-confidence (default
  `1e5`).

- min_cases:

  Case count below which a percentage is flagged low-confidence (default
  `10`).

- reference_date:

  Date used to cap the annualisation period (default today). Pass a
  fixed date for reproducible runs.

- verbose:

  Emit a cli progress + summary report (default `TRUE`).

## Value

A named list:

- `adm0`,`adm1`,`adm2`:

  Wide tibbles (one row per admin-year) with a value column per
  indicator plus `afp_cases`, `npafp_cases`, `under15_pop`.

- `long`:

  Tidy tibble: one row per level x admin x year x indicator with
  `value`, `numerator`, `denominator`, `confidence`, `category`,
  `text_code`.

- `meta`:

  Run metadata (indicators, levels, thresholds, `reference_date`).

## Details

Indicators currently implemented:

- `npafp_rate`:

  Annualised non-polio AFP cases per 100,000 children under 15:
  `annualise(npafp_cases / under15_pop * 100000)`. Target \>= 2 (warn) /
  \>= 3 (good).

- `stool_adequacy_pct`:

  % of AFP cases with two adequate stool specimens. Target \>= 80%
  (good) / \>= 60% (warn).

- `inv_timeliness_pct`:

  % of AFP cases investigated within `invest_timely_days` days of
  notification.

- `pct_districts_npafp_ge2`:

  % of districts (adm2) whose annualised NPAFP rate is \>= `npafp_warn`;
  reported at adm0 and adm1.
