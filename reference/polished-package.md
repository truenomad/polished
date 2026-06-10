# polished: streamlined downloader and cleaner for WHO POLIS data

Provides
[`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
for incremental, parallel-friendly downloads from the POLIS OData API, a
set of standalone cleaners
([`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md),
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md),
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md),
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md))
wired together by
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md),
and EPID-driven recovery of missing administrative geography via
[`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md).

## NAMESPACE imports

`dplyr`'s set-operation generics (`setdiff`, `intersect`, `union`) are
imported into the package namespace so unqualified calls inside the
package dispatch on data.frames instead of falling back to base R's
vector-only versions – which would crash the change-log diff with
"argument is of length zero". `.data` is imported to support tidy-eval
NSE inside internal helpers.

## See also

Useful links:

- <https://github.com/truenomad/polished>

- <https://truenomad.github.io/polished/>

- Report bugs at <https://github.com/truenomad/polished/issues>

## Author

**Maintainer**: Mohamed A. Yusuf <mohamedayusuf87@gmail.com>
([ORCID](https://orcid.org/0000-0002-9339-4613))
