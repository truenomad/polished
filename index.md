# polished

`polished` downloads and standardises polio surveillance data from the
WHO POLIS OData API. It gives you a one-function pull that works around
POLIS’s awkward pagination and date-filter quirks, then a set of
cleaners — starting with EPID-driven recovery of missing administrative
geography — to get the data analysis-ready.

## Installation

Install the development version from GitHub with
`pak::pak("truenomad/polished")`. Downloading requires a POLIS API key,
read from the `POLIS_API_KEY` environment variable.

## Key functions

### Download

| Function | Purpose |
|----|----|
| [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md) | Pull one or many POLIS tables into a local cache. Works around POLIS’s year-aligned date filters and Id-range pagination, checkpoints each batch so an interrupted pull resumes cleanly, optionally fetches years in parallel, and verifies completeness against POLIS when it finishes. |

### Clean & standardise

| Function | Purpose |
|----|----|
| [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md) | Recover missing administrative geography (names and GUIDs) from the EPID through an ordered, provenance-stamped cascade. Fills only blank cells, never overwrites present values, and leaves ambiguous gaps `unresolved` rather than fabricating them. |

See the vignettes and each function’s help page
(e.g. [`?get_polis_data`](https://truenomad.github.io/polished/reference/get_polis_data.md))
for usage, the full cascade, and data-formatting requirements.

## License

MIT © Mohamed A. Yusuf. See
[LICENSE](https://truenomad.github.io/polished/LICENSE) for details.
Issues and pull requests welcome at
<https://github.com/truenomad/polished>.
