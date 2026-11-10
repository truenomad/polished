# =============================================================================
# Environmental surveillance performance
#
# Site and district indicators of ES performance over a reporting window that
# ends at the last collection month whose laboratory results are largely back:
# the share of resolved samples with an enterovirus, the share with a
# poliovirus, site regularity, and the minimum-sample rule.
# =============================================================================

utils::globalVariables(c(
  ".esp_key",
  ".esp_to",
  "adm1",
  "adm2",
  "collection_date",
  "ev",
  "ev_n",
  "ev_pct",
  "first_month",
  "from",
  "is_resolved",
  "months_open",
  "months_sampled",
  "month",
  "pending",
  "pv",
  "pv_n",
  "pv_pct",
  "regularity_pct",
  "resolved",
  "resolved_share",
  "samples",
  "site_guid",
  "site_name",
  "x",
  "y"
))

# ---- public API -------------------------------------------------------------

#' Monthly laboratory resolution of environmental samples
#'
#' Counts the environmental samples collected in each month up to `as_of` and
#' the share of them that carry a laboratory classification. A sample still at
#' the laboratory has no classification and is unresolved. A negative sample
#' has a classification but no virus type, so it counts as resolved.
#'
#' @param samples Cleaned environmental samples, one row per sample, as
#'   returned by [clean_es()].
#' @param as_of Date the analysis runs to. Samples collected after it, or with
#'   no collection date, are left out.
#' @param cols Named list overriding the default column names, in the same
#'   shape as in [calc_polio_indicators()], for example
#'   `list(es = list(vtype = "virus_type"))`. The defaults are the polished ES
#'   columns plus `vtype = "vtype"`, `site_guid = "site_guid"`,
#'   `site_x = "site_x_coordinate"`, `site_y = "site_y_coordinate"` and
#'   `district_key = "adm2_guid"`.
#'
#' @return A tibble with one row per collection month, sorted by month:
#'   `month` (the first day of the month), `samples`, `resolved`, `pending`
#'   and `resolved_share` (resolved over samples, between 0 and 1).
#' @examples
#' es <- tibble::tibble(
#'   collection_date = as.Date(c("2024-01-05", "2024-01-20", "2024-02-03")),
#'   classification_all = c("NPEV", NA, "NEGATIVE")
#' )
#' calc_es_resolution(es, as_of = as.Date("2024-03-01"))
#' @export
calc_es_resolution <- function(samples, as_of, cols = list()) {
  m <- .esp_cols(cols)
  as_of <- .esp_check_date(as_of, "as_of")
  .esp_prepare(samples, as_of, m, c("collection_date", "class")) |>
    dplyr::summarise(
      samples = dplyr::n(),
      resolved = sum(is_resolved),
      pending = sum(!is_resolved),
      .by = month
    ) |>
    dplyr::mutate(resolved_share = resolved / samples) |>
    dplyr::arrange(month)
}

#' Last day of laboratory-complete environmental surveillance
#'
#' Finds the last day of the collection period whose samples have largely come
#' back from the laboratory. A collection month fails when fewer than
#' `resolved_min` of its samples are resolved. The period ends the day before
#' the earliest failing month that is no older than `settle_months` before the
#' month of `as_of`. A month older than that is taken as final even if it never
#' reached `resolved_min`, so it no longer holds the period back. When no month
#' fails, the period ends on the last day of the latest sampled month. The
#' result is never later than `as_of`.
#'
#' @param resolution A monthly summary with columns `month` (the first day of
#'   each collection month) and `resolved_share` (between 0 and 1), as returned
#'   by [calc_es_resolution()].
#' @param as_of Date the analysis runs to.
#' @param resolved_min Share of a month's samples that must be resolved for the
#'   month to count as complete. Default `0.80`.
#' @param settle_months Number of months after which a month still below
#'   `resolved_min` is taken as final. Default `3`.
#'
#' @return A single `Date`, or `NA` when `resolution` has no rows.
#' @examples
#' res <- tibble::tibble(
#'   month = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   resolved_share = c(1, 0.9, 0.4)
#' )
#' es_complete_end(res, as_of = as.Date("2024-04-15"))
#' @export
es_complete_end <- function(
  resolution,
  as_of,
  resolved_min = 0.80,
  settle_months = 3L
) {
  if (!is.data.frame(resolution)) {
    cli::cli_abort("{.arg resolution} must be a data frame.")
  }
  .esp_check_cols(resolution, c("month", "resolved_share"), "resolution")
  as_of <- .esp_check_date(as_of, "as_of")
  .esp_check_share(resolved_min, "resolved_min")
  .esp_check_count(settle_months, "settle_months")
  if (nrow(resolution) == 0) {
    return(as.Date(NA))
  }
  month_idx <- .esp_month_index(as.Date(resolution$month))
  settle_from <- .esp_month_index(as_of) - settle_months
  fails <- month_idx[
    resolution$resolved_share < resolved_min & month_idx >= settle_from
  ]
  fails <- fails[!is.na(fails)]
  end <- if (length(fails) > 0) {
    .esp_month_start(min(fails)) - 1
  } else {
    .esp_month_start(max(month_idx) + 1L) - 1
  }
  min(end, as_of)
}

#' Reporting window for environmental surveillance indicators
#'
#' Sets the window the ES indicators are computed over. The window ends on the
#' last day of the month in which [es_complete_end()] falls, and starts
#' `window_months - 1` months before that month, so it spans `window_months`
#' whole collection months. Running the window to `as_of` instead would count
#' samples still at the laboratory as if they had no enterovirus.
#'
#' @inheritParams calc_es_resolution
#' @inheritParams es_complete_end
#' @param window_months Number of collection months in the window. Default
#'   `12`.
#'
#' @return A one-row tibble with `window_start`, `window_end`, `last_month`
#'   (the first day of the last month in the window) and `lag_months` (how many
#'   whole months the last month in the window sits behind the month of
#'   `as_of`).
#' @examples
#' es <- tibble::tibble(
#'   collection_date = as.Date(c("2024-01-05", "2024-02-03", "2024-03-10")),
#'   classification_all = c("NPEV", "NEGATIVE", NA)
#' )
#' es_reporting_window(es, as_of = as.Date("2024-03-20"), window_months = 2)
#' @export
es_reporting_window <- function(
  samples,
  as_of,
  window_months = 12L,
  resolved_min = 0.80,
  settle_months = 3L,
  cols = list()
) {
  .esp_check_count(window_months, "window_months", min = 1)
  resolution <- calc_es_resolution(samples, as_of, cols)
  as_of <- as.Date(as_of)
  end <- es_complete_end(resolution, as_of, resolved_min, settle_months)
  if (is.na(end) || end < min(resolution$month)) {
    cli::cli_abort(c(
      "No collection month has reached {resolved_min * 100}% resolved.",
      "i" = "Check that {.arg samples} carries laboratory classifications."
    ))
  }
  last_idx <- .esp_month_index(end)
  tibble::tibble(
    window_start = .esp_month_start(last_idx - (window_months - 1L)),
    window_end = .esp_month_start(last_idx + 1L) - 1,
    last_month = .esp_month_start(last_idx),
    lag_months = as.integer(.esp_month_index(as_of) - last_idx)
  )
}

#' Minimum-sample rule for environmental surveillance percentages
#'
#' Places each unit in one of three states from its count of resolved samples.
#' A unit with no resolved samples is absent. A unit with fewer than
#' `min_samples` resolved samples has too few for its percentage to be read as
#' performance. Any other unit is measured.
#'
#' @param n Integer vector of resolved sample counts.
#' @param min_samples Fewest resolved samples for a percentage to be measured.
#'   Default `5`.
#' @param measured_label,thin_label,absent_label Labels for the three states.
#'   Defaults `"Measured"`, `"Too few samples"` and `"No ES site"`.
#'
#' @return A factor the length of `n`, with levels in the order measured, thin,
#'   absent.
#' @examples
#' es_sample_state(c(0, 3, 12))
#' @export
es_sample_state <- function(
  n,
  min_samples = 5L,
  measured_label = "Measured",
  thin_label = "Too few samples",
  absent_label = "No ES site"
) {
  if (!is.numeric(n)) {
    cli::cli_abort("{.arg n} must be numeric.")
  }
  .esp_check_count(min_samples, "min_samples")
  labels <- c(measured_label, thin_label, absent_label)
  if (!is.character(labels) || length(labels) != 3L || anyDuplicated(labels)) {
    cli::cli_abort("The three state labels must be distinct single strings.")
  }
  factor(
    dplyr::case_when(
      is.na(n) | n == 0 ~ absent_label,
      n < min_samples ~ thin_label,
      TRUE ~ measured_label
    ),
    levels = labels
  )
}

#' Environmental surveillance performance by district
#'
#' Computes ES performance for each district over the reporting window.
#'
#' * **Enterovirus share** (`ev_pct`): samples with any virus type recorded
#'   (`ev_n`, a non-polio enterovirus or any poliovirus) over resolved samples
#'   (`resolved`), times 100. The GPEI standard is 50 per cent or more
#'   (`ev_threshold`). This differs from the `ev_rate` indicator in
#'   [calc_polio_indicators()], which divides by all samples, and from
#'   `sites_with_entero_pct`, which counts a site as meeting the standard
#'   above 49 per cent. Here samples still at the laboratory are left out of
#'   the denominator, and the standard is met at 50 per cent or more.
#' * **Poliovirus share** (`pv_pct`): samples whose virus type matches
#'   `pv_pattern` (`pv_n`) over resolved samples, times 100. No threshold.
#' * **Minimum-sample rule** (`ev_state`): a district with fewer than
#'   `min_samples` resolved samples is marked as having too few, and one with
#'   none as absent. See [es_sample_state()].
#'
#' A percentage is `NA` when the district has no resolved samples. Samples are
#' counted by district on `district_key`, so a retired district GUID can be
#' repaired before the call (supply the repaired key as a column and point
#' `cols$es$district_key` at it) or through `guid_lookup`.
#'
#' @inheritParams es_reporting_window
#' @param window The reporting window: a tibble with `window_start` and
#'   `window_end` (as returned by [es_reporting_window()]), or a length-two
#'   `Date` vector. `NULL` (default) computes it from `samples` and `as_of`
#'   with `window_months`, `resolved_min` and `settle_months`.
#' @param districts Optional table of every district to report, with the
#'   columns named by `cols$es$adm1`, `cols$es$adm2` and `cols$es$adm2_guid`.
#'   A district with no sample in the window is kept with zero counts. `NULL`
#'   (default) reports only districts with a sample in the window.
#' @param guid_lookup Optional table with columns `from` and `to` that maps a
#'   retired district key to its current key. Keys not listed are kept.
#' @param pv_pattern Regular expression a virus type must match to count as a
#'   poliovirus. Default `"^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV"`, wild and
#'   vaccine-derived poliovirus, not Sabin or NPEV.
#' @param ev_threshold Enterovirus share, in per cent, at or above which a unit
#'   meets the standard. Default `50`.
#' @inheritParams es_sample_state
#' @param digits Decimal places to round percentages to, or `NULL` (default
#'   for districts) for no rounding.
#'
#' @return A tibble with one row per district: `adm1`, `adm2`, `guid`, `sites`,
#'   `samples`, `resolved`, `pending`, `ev_n`, `pv_n`, `ev_pct`, `pv_pct`,
#'   `ev_state` and `ev_meets_standard`.
#' @examples
#' es <- tibble::tibble(
#'   adm1 = "P1", adm2 = "D1", adm2_guid = "G1", site_guid = "S1",
#'   collection_date = as.Date("2024-01-01") + c(0, 31, 60, 91, 121, 152),
#'   classification_all = "DONE",
#'   vtype = c("NPEV", NA, "NPEV", "cVDPV2", NA, "NPEV")
#' )
#' calc_es_districts(es, as_of = as.Date("2024-07-15"), window_months = 6)
#' @export
calc_es_districts <- function(
  samples,
  as_of,
  window = NULL,
  districts = NULL,
  guid_lookup = NULL,
  pv_pattern = "^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV",
  ev_threshold = 50,
  min_samples = 5L,
  measured_label = "Measured",
  thin_label = "Too few samples",
  absent_label = "No ES site",
  window_months = 12L,
  resolved_min = 0.80,
  settle_months = 3L,
  digits = NULL,
  cols = list()
) {
  m <- .esp_cols(cols)
  as_of <- .esp_check_date(as_of, "as_of")
  .esp_check_args(pv_pattern, ev_threshold, digits)
  bounds <- .esp_bounds(
    window, samples, as_of, window_months, resolved_min, settle_months, cols
  )
  keys <- c(
    "collection_date", "class", "vtype", "adm1", "adm2", "district_key",
    "site_guid"
  )
  in_window <- .esp_prepare(samples, as_of, m, keys, pv_pattern) |>
    dplyr::filter(
      collection_date >= bounds[[1]], collection_date <= bounds[[2]]
    ) |>
    .esp_repair_key(guid_lookup)

  district <- in_window |>
    dplyr::summarise(
      adm1 = dplyr::first(adm1),
      adm2 = dplyr::first(adm2),
      sites = dplyr::n_distinct(site_guid),
      samples = dplyr::n(),
      resolved = sum(is_resolved),
      pending = sum(!is_resolved),
      ev_n = sum(ev),
      pv_n = sum(pv),
      .by = .esp_key
    ) |>
    dplyr::rename(guid = .esp_key)

  if (!is.null(districts)) {
    district <- .esp_spine(districts, m) |>
      dplyr::left_join(
        dplyr::select(district, -adm1, -adm2),
        by = "guid"
      ) |>
      dplyr::mutate(
        dplyr::across(
          c(sites, samples, resolved, pending, ev_n, pv_n),
          \(x) tidyr::replace_na(x, 0L)
        )
      )
  }

  district |>
    dplyr::mutate(
      ev_pct = .esp_pct(ev_n, resolved, digits),
      pv_pct = .esp_pct(pv_n, resolved, digits),
      ev_state = es_sample_state(
        resolved, min_samples, measured_label, thin_label, absent_label
      ),
      ev_meets_standard = ev_pct >= ev_threshold
    ) |>
    dplyr::relocate(adm1, adm2, guid)
}

#' Environmental surveillance performance by site
#'
#' Computes ES performance for each site with a sample in the reporting
#' window. The enterovirus share, poliovirus share, standard and minimum-sample
#' rule are defined as in [calc_es_districts()], with the site as the unit.
#'
#' * **Site regularity** (`regularity_pct`): months in the window with at
#'   least one sample (`months_sampled`) over the months in the window the site
#'   was open (`months_open`), times 100. A site is open from the month of its
#'   first sample on or before `as_of`, so a site opened inside the window is
#'   held only to the months since. The share is banded by
#'   `regularity_breaks` into `regularity_band`.
#' * **Province pooling**: `province_ev_pct` and `province_regularity_pct`
#'   pool the counts of every site in the province, rather than averaging the
#'   site percentages, and `province_sites` counts the sites.
#'
#' Rows are sorted by `ev_pct`, lowest first.
#'
#' @inheritParams calc_es_districts
#' @param regularity_breaks Increasing lower bounds, in per cent, of each band
#'   after the first. Default `c(75, 100)`.
#' @param regularity_labels Band labels, one more than `regularity_breaks`.
#'   Default `c("Fewer than 3 in 4 months", "3 in 4 months or more",
#'   "Every month")`.
#' @param digits Decimal places to round percentages to, or `NULL` for no
#'   rounding. Default `1`. Regularity is banded after rounding.
#'
#' @return A tibble with one row per site: `adm1`, `adm2`, `site_name`,
#'   `site_guid`, `samples`, `months_sampled`, `resolved`, `pending`, `ev_n`,
#'   `pv_n`, `x`, `y`, `ev_pct`, `pv_pct`, `first_month`, `months_open`,
#'   `regularity_pct`, `regularity_band`, `province_ev_pct`,
#'   `province_regularity_pct`, `province_sites`, `ev_state` and
#'   `ev_meets_standard`.
#' @examples
#' es <- tibble::tibble(
#'   adm1 = "P1", adm2 = "D1", site_name = "Site A", site_guid = "S1",
#'   site_x_coordinate = 65.7, site_y_coordinate = 31.6,
#'   collection_date = as.Date("2024-01-10") + c(0, 31, 91, 121, 152),
#'   classification_all = "DONE",
#'   vtype = c("NPEV", NA, "NPEV", "NPEV", NA)
#' )
#' calc_es_sites(es, as_of = as.Date("2024-07-15"), window_months = 6)
#' @export
calc_es_sites <- function(
  samples,
  as_of,
  window = NULL,
  pv_pattern = "^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV",
  ev_threshold = 50,
  min_samples = 5L,
  measured_label = "Measured",
  thin_label = "Too few samples",
  absent_label = "No ES site",
  regularity_breaks = c(75, 100),
  regularity_labels = c(
    "Fewer than 3 in 4 months", "3 in 4 months or more", "Every month"
  ),
  window_months = 12L,
  resolved_min = 0.80,
  settle_months = 3L,
  digits = 1L,
  cols = list()
) {
  m <- .esp_cols(cols)
  as_of <- .esp_check_date(as_of, "as_of")
  .esp_check_args(pv_pattern, ev_threshold, digits)
  .esp_check_bands(regularity_breaks, regularity_labels)
  bounds <- .esp_bounds(
    window, samples, as_of, window_months, resolved_min, settle_months, cols
  )
  keys <- c(
    "collection_date", "class", "vtype", "adm1", "adm2", "site", "site_guid",
    "site_x", "site_y"
  )
  prepared <- .esp_prepare(samples, as_of, m, keys, pv_pattern)

  opened <- prepared |>
    dplyr::summarise(first_month = min(month), .by = site_guid)
  start_idx <- .esp_month_index(bounds[[1]])
  end_idx <- .esp_month_index(bounds[[2]])

  prepared |>
    dplyr::filter(
      collection_date >= bounds[[1]], collection_date <= bounds[[2]]
    ) |>
    dplyr::summarise(
      samples = dplyr::n(),
      months_sampled = dplyr::n_distinct(month),
      resolved = sum(is_resolved),
      pending = sum(!is_resolved),
      ev_n = sum(ev),
      pv_n = sum(pv),
      x = dplyr::first(x),
      y = dplyr::first(y),
      .by = c(adm1, adm2, site_name, site_guid)
    ) |>
    dplyr::mutate(
      ev_pct = .esp_pct(ev_n, resolved, digits),
      pv_pct = .esp_pct(pv_n, resolved, digits)
    ) |>
    dplyr::left_join(opened, by = "site_guid") |>
    dplyr::mutate(
      months_open = as.integer(pmax(
        0L, end_idx - pmax(.esp_month_index(first_month), start_idx) + 1L
      )),
      regularity_pct = .esp_pct(months_sampled, months_open, digits),
      regularity_band = factor(
        regularity_labels[findInterval(regularity_pct, regularity_breaks) + 1L],
        levels = regularity_labels
      )
    ) |>
    dplyr::mutate(
      province_ev_pct = .esp_pct(sum(ev_n), sum(resolved), digits),
      province_regularity_pct = .esp_pct(
        sum(months_sampled), sum(months_open), digits
      ),
      province_sites = dplyr::n(),
      .by = adm1
    ) |>
    dplyr::mutate(
      ev_state = es_sample_state(
        resolved, min_samples, measured_label, thin_label, absent_label
      ),
      ev_meets_standard = ev_pct >= ev_threshold
    ) |>
    dplyr::arrange(ev_pct)
}

# ---- internal helpers -------------------------------------------------------

#' Default ES column map: the polished ES columns plus those this module needs.
#' @keywords internal
#' @noRd
.esp_default_cols <- function() {
  utils::modifyList(
    .polio_default_cols()$es,
    list(
      vtype = "vtype",
      site_guid = "site_guid",
      site_x = "site_x_coordinate",
      site_y = "site_y_coordinate",
      district_key = "adm2_guid"
    )
  )
}

#' Merge user `cols$es` over the module defaults.
#' @keywords internal
#' @noRd
.esp_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort(
      "{.arg cols} must be a list, such as {.code list(es = ...)}."
    )
  }
  utils::modifyList(.esp_default_cols(), cols$es %||% list())
}

#' Canonical output names for the mapped source columns.
#' @keywords internal
#' @noRd
.esp_canonical <- function() {
  c(
    collection_date = "collection_date", class = "class", vtype = "vtype",
    adm1 = "adm1", adm2 = "adm2", district_key = ".esp_key",
    site = "site_name", site_guid = "site_guid", site_x = "x", site_y = "y"
  )
}

#' Select, rename and flag the samples collected on or before `as_of`.
#' @keywords internal
#' @noRd
.esp_prepare <- function(samples, as_of, m, keys, pv_pattern = NULL) {
  if (!is.data.frame(samples)) {
    cli::cli_abort("{.arg samples} must be a data frame or tibble.")
  }
  src <- unlist(m[keys])
  .esp_check_cols(samples, src, "samples")
  out <- samples |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(
      stats::setNames(src, .esp_canonical()[keys])
    )) |>
    dplyr::mutate(collection_date = as.Date(collection_date)) |>
    dplyr::filter(!is.na(collection_date), collection_date <= as_of) |>
    dplyr::mutate(
      is_resolved = !is.na(class),
      month = lubridate::floor_date(collection_date, "month")
    )
  if (is.null(pv_pattern)) {
    return(out)
  }
  dplyr::mutate(
    out,
    ev = !is.na(vtype),
    pv = ev & stringr::str_detect(vtype, pv_pattern)
  )
}

#' Swap retired district keys for current ones through a from/to lookup.
#' @keywords internal
#' @noRd
.esp_repair_key <- function(df, guid_lookup) {
  if (is.null(guid_lookup)) {
    return(df)
  }
  if (!is.data.frame(guid_lookup)) {
    cli::cli_abort("{.arg guid_lookup} must be a data frame.")
  }
  .esp_check_cols(guid_lookup, c("from", "to"), "guid_lookup")
  if (anyDuplicated(guid_lookup$from)) {
    cli::cli_abort("{.arg guid_lookup} must list each {.field from} key once.")
  }
  lookup <- guid_lookup |>
    dplyr::transmute(.esp_key = as.character(from), .esp_to = as.character(to))
  df |>
    dplyr::mutate(.esp_key = as.character(.esp_key)) |>
    dplyr::left_join(lookup, by = ".esp_key") |>
    dplyr::mutate(.esp_key = dplyr::coalesce(.esp_to, .esp_key)) |>
    dplyr::select(-.esp_to)
}

#' Distinct district spine named adm1, adm2, guid.
#' @keywords internal
#' @noRd
.esp_spine <- function(districts, m) {
  if (!is.data.frame(districts)) {
    cli::cli_abort("{.arg districts} must be a data frame.")
  }
  src <- c(adm1 = m$adm1, adm2 = m$adm2, guid = m$adm2_guid)
  .esp_check_cols(districts, src, "districts")
  districts |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(src)) |>
    dplyr::distinct()
}

#' Window bounds as a length-two Date vector.
#' @keywords internal
#' @noRd
.esp_bounds <- function(window, samples, as_of, window_months, resolved_min,
                        settle_months, cols) {
  if (is.null(window)) {
    window <- es_reporting_window(
      samples, as_of, window_months, resolved_min, settle_months, cols
    )
  }
  if (is.data.frame(window)) {
    .esp_check_cols(window, c("window_start", "window_end"), "window")
    window <- c(window$window_start[1], window$window_end[1])
  }
  bounds <- suppressWarnings(as.Date(window))
  if (length(bounds) != 2L || anyNA(bounds) || bounds[[1]] > bounds[[2]]) {
    cli::cli_abort(c(
      "{.arg window} must give a start date on or before an end date.",
      "i" = "Pass the result of {.fn es_reporting_window} or two dates."
    ))
  }
  bounds
}

#' Percentage with NA for a zero denominator, optionally rounded.
#' @keywords internal
#' @noRd
.esp_pct <- function(num, den, digits) {
  out <- dplyr::if_else(den > 0, 100 * num / den, NA_real_)
  if (is.null(digits)) out else round(out, digits)
}

.esp_month_index <- function(d) {
  lubridate::year(d) * 12L + lubridate::month(d)
}

.esp_month_start <- function(idx) {
  as.Date(sprintf("%d-%02d-01", (idx - 1L) %/% 12L, (idx - 1L) %% 12L + 1L))
}

# ---- validation -------------------------------------------------------------

.esp_check_cols <- function(df, needed, arg) {
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {arg}} is missing column{?s} {.field {missing}}.")
  }
  invisible(TRUE)
}

.esp_check_date <- function(x, arg) {
  out <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(e) as.Date(NA)
  )
  if (length(out) != 1L || is.na(out)) {
    cli::cli_abort("{.arg {arg}} must be a single date.")
  }
  out
}

.esp_check_share <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    cli::cli_abort("{.arg {arg}} must be a single number between 0 and 1.")
  }
  invisible(TRUE)
}

.esp_check_count <- function(x, arg, min = 0) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x >= min && x == round(x)
  if (!ok) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number of {min} or more."
    )
  }
  invisible(TRUE)
}

.esp_check_args <- function(pv_pattern, ev_threshold, digits) {
  if (!is.character(pv_pattern) || length(pv_pattern) != 1L ||
        is.na(pv_pattern)) {
    cli::cli_abort("{.arg pv_pattern} must be a single regular expression.")
  }
  if (!is.numeric(ev_threshold) || length(ev_threshold) != 1L ||
        is.na(ev_threshold) || ev_threshold < 0 || ev_threshold > 100) {
    cli::cli_abort("{.arg ev_threshold} must be a percentage from 0 to 100.")
  }
  if (!is.null(digits)) {
    .esp_check_count(digits, "digits")
  }
  invisible(TRUE)
}

.esp_check_bands <- function(breaks, labels) {
  ok_breaks <- is.numeric(breaks) && length(breaks) >= 1L &&
    !anyNA(breaks) && !is.unsorted(breaks, strictly = TRUE)
  if (!ok_breaks) {
    cli::cli_abort("{.arg regularity_breaks} must be strictly increasing.")
  }
  if (!is.character(labels) || length(labels) != length(breaks) + 1L ||
        anyDuplicated(labels)) {
    cli::cli_abort(
      "{.arg regularity_labels} must hold {length(breaks) + 1} distinct labels."
    )
  }
  invisible(TRUE)
}
