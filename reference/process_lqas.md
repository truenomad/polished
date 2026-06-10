# Process raw LQAS lots into classifications and district pass rates

Turns the raw `Lqas` table into lot-level classifications and a
per-district roll-up. Reproduces the POLIS rules that are well defined
(default lot size 60; the 2019+ "sample size must be a multiple of 60 or
the lot is INVALID" rule; 2-level and 3-level recodes; pass% excluding
INVALID lots).

## Usage

``` r
process_lqas(
  lqas,
  adm2_guid_var = "Admin2Guid",
  adm2_name_var = "Admin2Name",
  adm1_name_var = "Admin1Name",
  adm0_name_var = "Admin0Name",
  date_var = "ActivityStart",
  checked_var = "ChildrenChecked",
  unvacc_var = "ChildrenUnvaccinated",
  lot_var = NULL,
  default_checked = 60,
  multiple_of = 60,
  enforce_since = 2019,
  pass_threshold = 0.9,
  warn_threshold = 0.8,
  verbose = TRUE
)
```

## Arguments

- lqas:

  Raw LQAS table (data.frame/tibble), one row per lot.

- adm2_guid_var, adm2_name_var:

  Column names for the district GUID/name.

- adm1_name_var, adm0_name_var:

  Optional province / country name columns (set to `NULL` if absent).

- date_var:

  Column with the lot's planned/assessment date (used to apply the 2019
  rule by year).

- checked_var:

  Column with the number of children checked / sample size.

- unvacc_var:

  Column with the number of children found unvaccinated.

- lot_var:

  Optional lot-identifier column (set `NULL` to auto-number).

- default_checked:

  Lot size assumed when `checked_var` is missing (default `60`, per
  POLIS).

- multiple_of:

  Sample-size modulus enforced from `enforce_since` (default `60`).

- enforce_since:

  Year from which the multiple-of rule makes a lot INVALID (default
  `2019`).

- pass_threshold:

  Coverage (vaccinated fraction) at/above which a lot is a Pass (default
  `0.90`). **Transparent stand-in for `REF_LQASThresholds`.**

- warn_threshold:

  Optional coverage band for the 3-level "Intermediate" class (default
  `0.80`). Lots in `[warn_threshold, pass_threshold)` are Intermediate
  (3-level) / Fail (2-level).

- verbose:

  Emit a cli summary (default `TRUE`).

## Value

A list with `lots` (lot-level tibble with `coverage`, `invalid`,
`lqas_class`, `lqas2`, `lqas3`), `district` (per-district roll-up with
`n_lots`, `n_pass`, `n_fail`, `n_invalid`, `pass_pct`), and `meta`.
