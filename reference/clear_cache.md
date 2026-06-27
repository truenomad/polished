# Clear a project's regenerable cache

Deletes everything in the project's `cache/` zone and nothing else – the
precious `raw/` and derived `processed/`/`validation/` zones are never
touched. Safe to call when the cache is already empty.

## Usage

``` r
clear_cache(project, quiet = FALSE)
```

## Arguments

- project:

  A `polis_project`.

- quiet:

  Suppress the success message. Default `FALSE`.

## Value

The `project`, invisibly.

## Examples

``` r
proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
clear_cache(proj, quiet = TRUE)
```
