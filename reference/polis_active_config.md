# The session-active POLIS configuration

Returns the configuration most recently built by
[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
in this session – the object every cleaner and orchestrator defaults to.
If none has been built yet, a default
[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
is created, registered, and returned, so the getter always yields a
usable config.

## Usage

``` r
polis_active_config()
```

## Value

The active `polis_config` object.

## See also

[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md),
which sets the active config.

## Examples

``` r
polis_config(start_year = 2018)
#> 
#> ── POLIS pipeline configuration ────────────────────────────────────────────────
#> Start year: 2018
#> Regions: "AFRO", "AMRO", "EMRO", "EURO", "SEARO", and "WPRO"
#> Column-order groups: "id", "iso", "country", "geo_group", "adm_name",
#> "adm_guid", "coord", "onset_date", "onset_month", "onset_year", "age",
#> "core_dates", "classification", "indicators", and "dates" -> other
#> Synonyms: FALSE
#> Population: FALSE
#> WorldPop: FALSE
#> Population source: "reconciled"
#> Shape: FALSE
#> Inputs: "<none>"
#> Output dir: "<none>"
#> SIA cache: "<none>"
polis_active_config()$start_year
#> [1] 2018
```
