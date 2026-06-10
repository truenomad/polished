#' polished: streamlined downloader and cleaner for WHO POLIS data
#'
#' Provides [get_polis_data()] for incremental, parallel-friendly downloads
#' from the POLIS OData API, a set of standalone cleaners
#' ([clean_afp()], [clean_es()], [clean_sia()], [clean_virus()]) wired
#' together by [run_pipeline()], and EPID-driven recovery of missing
#' administrative geography via [impute_geo_from_epid()].
#'
#' @section NAMESPACE imports:
#' `dplyr`'s set-operation generics (`setdiff`, `intersect`, `union`) are
#' imported into the package namespace so unqualified calls inside the
#' package dispatch on data.frames instead of falling back to base R's
#' vector-only versions -- which would crash the change-log diff with
#' "argument is of length zero". `.data` is imported to support tidy-eval
#' NSE inside internal helpers.
#'
#' @keywords internal
#' @importFrom dplyr setdiff intersect union .data
#' @importFrom utils capture.output globalVariables
"_PACKAGE"

# Silence the R CMD check NOTE that lists every bare column name used in
# dplyr/tidyverse NSE expressions across the cleaning pipeline. These are
# source column names (mixed CamelCase, snake_case and dotted) plus a
# handful of helper variables created inside dplyr chains. Kept in sync with
# the names actually referenced in R/ -- unused entries are pruned.
utils::globalVariables(c(
  ".crow",
  ".n",
  ".year_ok",
  ".yr",
  ":=",
  "API_Name",
  "Admin1Name",
  "Admin2Name",
  "DateFrom",
  "Doses",
  "EPID",
  "GUID",
  "Id",
  "SIASubActivityCode",
  "Table",
  "X",
  "accepted",
  "age_months",
  "classification",
  "cluster",
  "collection_date",
  "count",
  "date_from",
  "emergence_group",
  "epid",
  "id",
  "im.loaded",
  "is_char",
  "iso3",
  "k_parent",
  "k_prefix",
  "k_value",
  "k_year",
  "keep",
  "lat",
  "latitude",
  "lead0",
  "location",
  "lon",
  "longitude",
  "lqas.loaded",
  "m_parent",
  "m_prefix",
  "m_year",
  "measurement",
  "month_onset",
  "n",
  "n_distinct_value",
  "n_non_na",
  "n_unique",
  "name",
  "needs_60day_followup",
  "new",
  "old",
  "onset_date_quality",
  "onset_to_followup",
  "onset_to_stool1",
  "onset_to_stool2",
  "paralysis_onset_date",
  "protected",
  "reason",
  "row_id",
  "same",
  "status",
  "stool1_to_stool2",
  "timeliness",
  "unique_ratio",
  "value",
  "variable",
  "virus_date",
  "vtype",
  "x",
  "y",
  "year",
  "year_onset"
))
