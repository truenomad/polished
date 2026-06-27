# Computing surveillance indicators

Once the surveillance streams are cleaned (see the *Cleaning POLIS data*
article),
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
computes the WHO POLIS indicator catalogue — the NPAFP rate, stool
adequacy, timeliness, dose history, environmental, virus, SIA and
composite families — at the admin levels you ask for.

## Browse the catalogue: `available_indicators()`

Every indicator the package knows about lives in a single registry. Read
it with
[`available_indicators()`](https://truenomad.github.io/polished/reference/available_indicators.md),
optionally filtered by `family`:

``` r

fams <- available_indicators()
table(fams$family)
#> 
#>        AFP  Composite       Dose         ES        Lab        SIA      Stool 
#>          7          4          8          8          1          7          3 
#> Timeliness      Virus 
#>         17          7

# the AFP family, with what each indicator needs
available_indicators(family = "AFP")[, c("code", "label", "requires_pop")]
#> # A tibble: 7 × 3
#>   code                 label                                    requires_pop
#>   <chr>                <chr>                                    <lgl>       
#> 1 afp_count            AFP count                                FALSE       
#> 2 npafp_count          NPAFP count                              FALSE       
#> 3 npafp_rate           NPAFP rate (per 100k under-15)           TRUE        
#> 4 npafp_rate_nopending NPAFP rate, no pending (per 100k)        TRUE        
#> 5 unclass_cases_pct    Unclassified cases (%)                   FALSE       
#> 6 fup_insa_cases_pct   60-day follow-up of inadequate cases (%) FALSE       
#> 7 case_contacts_avg    Contacts sampled per case (avg)          FALSE
```

`requires_pop = TRUE` marks the indicators that need an under-15
population denominator (the rate families); everything else is computed
from the case/ES/ virus/SIA tables alone.

## Inputs

[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
needs only a cleaned `cases` table to start; `es`, `sia`, `virus`,
`lab`, `population` and `admin_units` are optional and unlock the
indicators that depend on them. A missing source simply skips its
indicators with a message rather than erroring.

The example below builds a small **cleaned** AFP table by hand so it
renders without a live pull — in practice `cases` is the output of
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
or
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md).

``` r

cases <- tibble::tibble(
  epid = sprintf("NIE-BOR-D%02d-24-%03d", rep(1:2, length.out = 24), 1:24),
  classification_all = rep(c("NPAFP", "NPAFP", "NPAFP", "WPV1", "NOT-AFP", "PENDING"), 4),
  age_months = rep(c(12, 30, 48, 24), 6),
  year_onset = 2024L,
  paralysis_onset_date = as.Date("2024-01-01") + (1:24),
  adm0 = "NIGERIA", adm0_guid = "G0",
  adm1 = rep(c("BORNO", "YOBE"), length.out = 24),
  adm1_guid = rep(c("P1", "P2"), length.out = 24),
  adm2 = rep(c("MMC", "JERE", "GUJBA", "NGANZAI"), length.out = 24),
  adm2_guid = rep(c("D1", "D2", "D3", "D4"), length.out = 24),
  adequate_stool = rep(c("Yes", "No"), length.out = 24),
  notify_to_invest = rep(c(1, 2, 5, 1), 6)
)
```

The under-15 population denominator is an external table keyed by
`adm2_guid`, `year` and `u15_pop` (one row per admin unit per year):

``` r

population <- tibble::tibble(
  adm2_guid = c("G0", "P1", "P2", "D1", "D2", "D3", "D4"),
  year = 2024L,
  u15_pop = c(2e6, 1e6, 1e6, 5e5, 5e5, 5e5, 5e5)
)
```

## Compute

Ask for the admin levels you want; each is aggregated independently from
the case GUIDs. `indicators` selects what to compute: `"core"` (the
default — the core KPI set), `"all"` (the full registered catalogue), or
an explicit vector of codes. This article walks the broader catalogue,
so it asks for `"all"`:

``` r

ind <- calc_polio_indicators(
  cases = cases,
  population = population,
  levels = c("adm0", "adm1", "adm2"),
  indicators = "all",
  verbose = FALSE
)
names(ind)
#> [1] "adm0"       "adm1"       "adm2"       "long"       "meta"      
#> [6] "validation"
```

The result is a list: one **wide** tibble per level (`adm0` / `adm1` /
`adm2`, one row per admin unit × year with every indicator as a column),
a tidy **`long`** table (one row per admin × year × indicator), and a
`meta` record of what ran, was skipped, and the thresholds used.

``` r

ind$long[
  ind$long$indicator %in% c("afp_count", "npafp_count", "npafp_rate", "stool_adequacy_pct"),
  c("level", "name", "indicator", "value")
]
#> # A tibble: 28 × 4
#>    level name    indicator           value
#>    <chr> <chr>   <chr>               <dbl>
#>  1 adm0  NIGERIA afp_count          20    
#>  2 adm0  NIGERIA npafp_count        12    
#>  3 adm0  NIGERIA npafp_rate          0.798
#>  4 adm0  NIGERIA stool_adequacy_pct 40    
#>  5 adm1  BORNO   afp_count           8    
#>  6 adm1  YOBE    afp_count          12    
#>  7 adm1  BORNO   npafp_count         8    
#>  8 adm1  YOBE    npafp_count         4    
#>  9 adm1  BORNO   npafp_rate          0.798
#> 10 adm1  YOBE    npafp_rate          0.798
#> # ℹ 18 more rows
```

The wide per-level tibble is the analyst-facing shape — one row per
unit:

``` r

ind$adm1[, c("name", "afp_count", "npafp_count", "npafp_rate", "stool_adequacy_pct")]
#> # A tibble: 2 × 5
#>   name  afp_count npafp_count npafp_rate stool_adequacy_pct
#>   <chr>     <dbl>       <dbl>      <dbl>              <dbl>
#> 1 BORNO         8           8      0.798                100
#> 2 YOBE         12           4      0.798                  0
```

## Population denominators and suppression

Rate indicators (e.g. `npafp_rate`) divide a count by the under-15
population and scale by `rate_multiplier` (default per 100,000). To keep
small-denominator noise out of the output, a unit’s rate is suppressed
(`NA`) when its population is below `min_pop` or its case count is below
`min_cases`. Pass no `population` and the rate families are skipped
entirely — every population-independent indicator (counts, percentages,
timeliness) still computes:

``` r

no_pop <- calc_polio_indicators(
  cases = cases,
  levels = "adm0",
  indicators = "all",
  verbose = FALSE
)
no_pop$long[no_pop$long$indicator %in% c("npafp_count", "npafp_rate"), c("indicator", "value")]
#> # A tibble: 1 × 2
#>   indicator   value
#>   <chr>       <dbl>
#> 1 npafp_count    12
```

See
[`?calc_polio_indicators`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
for the full argument set — the column-name overrides, the
classification vocabularies, and every threshold.
