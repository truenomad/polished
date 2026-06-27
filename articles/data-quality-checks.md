# Data-quality checks

The check functions surface data-quality problems in a cleaned table by
**reading the columns the cleaners already produced** — duplicates,
blank keys, unreconciled admin GUIDs, out-of-range values and
date-ordering violations. Nothing here re-derives geography or
recomputes anything; each check is a cheap filter, so running them is
fast even on millions of rows.

There is one function per stream:

| Function | Stream |
|----|----|
| [`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md) | cleaned AFP cases |
| [`checks_es()`](https://truenomad.github.io/polished/reference/checks_es.md) | cleaned environmental samples |
| [`checks_hum_spec()`](https://truenomad.github.io/polished/reference/checks_hum_spec.md) | cleaned human specimens |
| [`checks_sia()`](https://truenomad.github.io/polished/reference/checks_sia.md) | cleaned SIA campaigns |
| [`checks_virus()`](https://truenomad.github.io/polished/reference/checks_virus.md) | cleaned poliovirus positives |

## Running a check

Each `checks_*()` takes a cleaned table and returns a list: a `summary`
tibble counting every applicable check, plus one tibble of flagged rows
per check that found problems. The example below seeds a small cleaned
AFP table with a few deliberate issues:

``` r

afp <- tibble::tibble(
  id = 1:5,
  epid = c("NIE-A-1", "NIE-A-1", "NIE-C-3", "NIE-D-4", "NIE-E-5"),
  adm0 = c("NIGERIA", "NIGERIA", "CHAD", "MALI", "NIGER"),
  adm1 = "p", adm2 = "d",
  paralysis_onset_date = c("2024-01-02", "2024-01-02", NA, "2999-01-01", "2024-03-03"),
  year_onset = c(2024L, 2024L, NA, 2999L, 2024L),
  classification_all = c("NPAFP", "NPAFP", "", "NPAFP", "NPAFP"),
  adm1_guid = c("g", "g", NA, "g", "g"),
  adm2_guid = "g",
  latitude = c(9.1, 9.1, 0, 9.1, 9.1),
  longitude = 7.2,
  age_months = c(24, 24, -5, 30, 36),
  notify_to_invest = c(1, 1, -1, 2, 1),
  adequate_stool = c("Yes", "Yes", "No", "Yes", "Yes")
)

res <- checks_afp(afp)
res$summary
#> # A tibble: 9 × 5
#>   check                  domain severity n_flagged description                  
#>   <chr>                  <chr>  <chr>        <int> <chr>                        
#> 1 afp_missing_guid       AFP    error            1 Cases missing an admin1/admi…
#> 2 afp_duplicates         AFP    warning          2 Duplicate EPID + onset date …
#> 3 afp_no_onset           AFP    warning          1 AFP cases with no paralysis …
#> 4 afp_no_classification  AFP    warning          1 AFP cases with no usable cla…
#> 5 afp_future_onset       AFP    warning          1 Onset date later than the ru…
#> 6 afp_negative_intervals AFP    warning          1 Negative timeliness interval…
#> 7 afp_empty_coords       AFP    info             1 Cases with missing or zero c…
#> 8 afp_age_out_of_range   AFP    info             1 Age in months negative or im…
#> 9 afp_inadequate_stool   AFP    info             1 Cases flagged with inadequat…
```

The `summary` lists each check, its `severity` (`error` / `warning` /
`info`, sorted worst first), and how many rows it flagged. Checks whose
required columns are absent are skipped, so a trimmed table is handled
gracefully.

The flagged rows for any failing check live under its name, holding the
key columns plus whatever the check is about:

``` r

names(res)
#>  [1] "summary"                "afp_duplicates"         "afp_no_onset"          
#>  [4] "afp_no_classification"  "afp_missing_guid"       "afp_empty_coords"      
#>  [7] "afp_future_onset"       "afp_age_out_of_range"   "afp_negative_intervals"
#> [10] "afp_inadequate_stool"
res$afp_missing_guid
#> # A tibble: 1 × 9
#>      id epid    adm0  adm1  adm2  paralysis_onset_date year_onset adm1_guid
#>   <int> <chr>   <chr> <chr> <chr> <chr>                     <int> <chr>    
#> 1     3 NIE-C-3 CHAD  p     d     <NA>                         NA <NA>     
#> # ℹ 1 more variable: adm2_guid <chr>
```

## Export to Excel

[`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md)
turns a check result into one styled `.xlsx` workbook — a `Summary` tab
plus one tab of flagged rows per failing check (navy headers, sized
columns, inferred number formats). There is no versioning; the file is
written straight to `path`.

``` r

write_checks_excel(res, "checks_afp.xlsx")
```

## Automatic check workbooks in the pipeline

When you run the file-based pipeline with an output directory,
[`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
writes a `checks_<dataset>.xlsx` workbook next to each `polished_*`
output automatically — so a single call produces both the cleaned data
and its quality report (see the *End-to-end pipeline* article). This
step needs the optional `openxlsx` package; without it the checks are
skipped with a message rather than failing the run.
