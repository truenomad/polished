# Build a path inside a project zone

Resolves a path under one of a project's zones.
`project_path(proj, "processed", "sia")` returns `<root>/processed/sia`.
With no extra parts it returns the zone directory itself.

## Usage

``` r
project_path(project, zone = "root", ...)
```

## Arguments

- project:

  A `polis_project`.

- zone:

  One of `"root"`, `"raw"`, `"processed"`, `"validation"`, `"cache"`,
  `"logs"`. Default `"root"`.

- ...:

  Further path components appended under the zone.

## Value

A single file path string.

## Examples

``` r
proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
project_path(proj, "processed", "sia")
#> [1] "/tmp/RtmpzYWaFR/polis_demo/processed/sia"
```
