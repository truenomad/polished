# polished <img src="man/figures/logo.png" align="right" height="139" alt="polished package logo" />

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

`polished` downloads and standardises polio surveillance data from the WHO POLIS
OData API. It gives you a one-function pull that works around POLIS's awkward
pagination and date-filter quirks, then a set of cleaners — starting with
EPID-driven recovery of missing administrative geography — to get the data
analysis-ready.

## Installation

```r
# install.packages("pak")
pak::pak("truenomad/polished")
```

Set your POLIS API key once per session (or in `.Renviron`):

```r
Sys.setenv(POLIS_API_KEY = "your-key")
```

## Download POLIS data

`get_polis_data()` is the entry point. It fetches a table (or all of them) into a
local cache, resuming from where a previous pull left off and verifying
completeness against POLIS when it finishes.

```r
# Pull one table; data is cached on disk, nothing returned
polished::get_polis_data(tables = "im")

# Same call, but get the data.frame back
im <- polished::get_polis_data(tables = "im", return = "df")

# All tables in parallel into a project folder, just the file paths
polished::get_polis_data(
  polis_folder = "data/polis",
  workers = parallel::detectCores() - 1L,
  return = "paths"
)
```

It handles the POLIS-specific traps for you: year-aligned date filters, Id-range
pagination (POLIS rejects OData `$skip`), per-table "update" date columns,
per-batch checkpointing so a dropped connection resumes cleanly, and an optional
post-download Id-completeness check that refetches anything missing. See
`?get_polis_data` and the *Downloading POLIS data* vignette for the full set of
arguments.

## Clean the data

`impute_geo_from_epid()` recovers missing admin columns from the EPID through an
ordered, provenance-stamped cascade (self-reference → prefix-match → external
reference → country code), filling only blank cells and leaving ambiguous ones
as `unresolved`.

```r
result <- polished::impute_geo_from_epid(cases)
result$data   # repaired frame + a <col>_source provenance factor per column
result$qa     # per-level fill counts and pct_resolved
```

Supporting functions:

- **EPID parsing** — `epid_split()`, `epid_country_code()`, `epid_prefix()`,
  `epid_strip_contact()`
- **Reference builders** — `build_admin_ref()`, `build_prefix_ref()`,
  `resolve_epid_country()`

See the vignettes for the full cascade, data-formatting requirements, and worked
examples.

## License

MIT © Mohamed A. Yusuf. See [LICENSE](LICENSE) for details. Issues and pull
requests welcome at <https://github.com/truenomad/polished>.
