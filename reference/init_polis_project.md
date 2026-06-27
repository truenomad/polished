# Create (or re-open) a data project

Sets up the standard project layout under `root` and returns a
`polis_project` describing it. The layout has four role-named zones plus
logs:

- `raw`:

  downloaded source tables (precious – never written by cleaning).

- `processed`:

  cleaned analytic outputs (derived); a natural value for
  `polis_config(output_dir = )`.

- `validation`:

  data-quality reports and checks.

- `cache`:

  regenerable process caches; a natural value for
  `polis_config(cache_dir = )`.

- `logs`:

  run logs.

Creation is idempotent and never deletes: calling it again on an
existing project just re-opens it. Downstream functions take the
returned object via their `project` argument; nothing relies on a hidden
global.

## Usage

``` r
init_polis_project(root, gitignore = TRUE, quiet = FALSE)
```

## Arguments

- root:

  Path to the project root. Created (recursively) if absent.

- gitignore:

  Write a `.gitignore` ignoring the regenerable/precious-but- bulky
  zones (`raw/`, `cache/`, `logs/`) when one is not already present.
  Default `TRUE`.

- quiet:

  Suppress the success message. Default `FALSE`.

## Value

A `polis_project`: a list with `root` and one absolute path per zone
(`raw`, `processed`, `validation`, `cache`, `logs`), class
`polis_project`.

## Examples

``` r
proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
proj$processed
#> [1] "/tmp/RtmpwxxTHE/polis_demo/processed"
```
