# Environmental surveillance site scorecard ------------------------------------

utils::globalVariables(c(
  "active", "axis", "bin", "background_pct", "campaign_response_pct",
  "date", "declined", "drop_pts", "earlier_pct", "era", "hi", "iso",
  "judged_basis", "judged_pct", "k", "k_earlier", "k_recent", "last_sample",
  "latitude", "lo", "longitude", "mid", "mid_n", "month", "months_sampled",
  "n_campaign_response", "n_earlier", "n_neighbour_recovery", "n_recent",
  "neighbour_recovery_pct", "network_rate", "new_site", "ev_pct", "other",
  "other_month", "p_value", "panel_samples", "rate",
  "rate_campaign_response", "rate_neighbour_recovery", "recent_pct",
  "resolved", "retention_pct", "sabin", "sample_id", "samples_per_month",
  "site", "site_code", "site_name", "span_months", "start", "target",
  "verdict", "weeks", "x", "y", "floor_n", "hit", "km", "first_date",
  "adm1", "adm2", "vaccine", "closest"
))

# ---- settings ----------------------------------------------------------------

#' Settings for the environmental site scorecard
#'
#' Collects every window, distance, threshold, vaccine list, status list and
#' verdict label that [calc_es_scorecard()], [calc_es_score_network()] and
#' [calc_es_score_profile()] use. The defaults are the programme standards the
#' Afghanistan weekly review applies today.
#'
#' @param opv_types Campaign vaccine types that shed Sabin virus. A campaign
#'   counts only when its vaccine type is one of these. Default `"bOPV"`,
#'   `"tOPV"`, `"mOPV1"`, `"mOPV3"`, `"mOPV2"` and `"IPV + bOPV"`.
#' @param campaign_statuses Campaign statuses that count as a campaign that
#'   took place. Default `"Done"` and `"Confirmed"`.
#' @param peak_weeks Weeks after the start of the latest campaign in the site's
#'   own district within which a sample counts towards campaign response. The
#'   window includes both ends, from week 0 to this value. Default `4`.
#' @param mid_weeks Two numbers giving the retention window in weeks after a
#'   campaign. A sample counts when its weeks since the campaign are above the
#'   first value and at most the second. Default `c(6, 12)`.
#' @param floor_weeks Weeks after a campaign beyond which a sample counts
#'   towards the between-campaign background level. Default `24`.
#' @param neighbour_km Largest distance in kilometres between two sites for
#'   one to count as the other's neighbour. Default `20`.
#' @param window_days Largest gap in days between the month a site was sampled
#'   and the month a neighbour recovered virus for the two to count as the same
#'   occasion. Months are compared by their first day. Default `62`.
#' @param active_months A site is judged only if its last sample falls within
#'   this many months of the latest sample in the network. Default `12`.
#' @param min_n Fewest samples or occasions a measure needs. Below it the site
#'   is not given a verdict, and the measure is left out of the network rate.
#'   Default `5`.
#' @param floor_pct The detection floor: the campaign response percentage
#'   below which a site is judged below the floor. Default `30`.
#' @param trend_years Years before the latest sample at which a site's record
#'   is split into an earlier and a recent part. Default `3`.
#' @param trend_min_n Fewest campaign response samples each part needs before
#'   the two parts are compared. Default `5`.
#' @param new_site_months For a site whose first sample falls after the split,
#'   the recent part is instead its last this many months. Default `12`.
#' @param trend_drop_pts Smallest fall in percentage points from the earlier
#'   to the recent part that counts as a decline. Default `15`.
#' @param trend_alpha Significance level of the Fisher's exact test that a
#'   decline must also pass. Default `0.05`.
#' @param isolation_min_samples Fewest samples with a final result a site
#'   needs before its enterovirus isolation is judged. Default `5`.
#' @param isolation_min_pct Enterovirus isolation percentage below which a
#'   site is judged to have low isolation. Default `50`.
#' @param verdict_labels Named character vector of the five verdict labels,
#'   with names `below_floor`, `falling`, `low_isolation`, `working` and
#'   `unjudged`. The first four are listed worst first and set the order of the
#'   verdict factor; `unjudged` comes last.
#' @param profile_breaks,profile_labels Bands of weeks since a campaign used by
#'   [calc_es_score_profile()], passed to [cut()] with intervals closed on the
#'   right. Default bands are 0 to 2, 2 to 4, 4 to 6, 6 to 8, 8 to 12, 12 to 24
#'   and 24 to 52 weeks, and more than a year.
#' @param days_per_month Days used to convert a gap in days into months.
#'   Default `30.44`.
#' @param crs Projection used to measure the distance between sites, as a
#'   PROJ string or EPSG code. The default is a Lambert azimuthal equal-area
#'   projection centred on Afghanistan and Pakistan, so a kilometre means the
#'   same across that network.
#'
#' @return A list of class `es_score_settings`.
#' @examples
#' es_score_settings(neighbour_km = 25)
#' @export
es_score_settings <- function(
  opv_types = c("bOPV", "tOPV", "mOPV1", "mOPV3", "mOPV2", "IPV + bOPV"),
  campaign_statuses = c("Done", "Confirmed"),
  peak_weeks = 4,
  mid_weeks = c(6, 12),
  floor_weeks = 24,
  neighbour_km = 20,
  window_days = 62,
  active_months = 12,
  min_n = 5,
  floor_pct = 30,
  trend_years = 3,
  trend_min_n = 5,
  new_site_months = 12,
  trend_drop_pts = 15,
  trend_alpha = 0.05,
  isolation_min_samples = 5,
  isolation_min_pct = 50,
  verdict_labels = c(
    below_floor = "Below the floor",
    falling = "Falling",
    low_isolation = "Low isolation",
    working = "Working",
    unjudged = "Too few samples"
  ),
  profile_breaks = c(-1, 2, 4, 6, 8, 12, 24, 52, Inf),
  profile_labels = c(
    "0-2w", "2-4w", "4-6w", "6-8w", "8-12w", "12-24w", "24-52w", ">1y"
  ),
  days_per_month = 30.44,
  crs = "+proj=laea +lat_0=31 +lon_0=68 +datum=WGS84 +units=m +no_defs"
) {
  .essc_check_chr(opv_types, "opv_types")
  .essc_check_chr(campaign_statuses, "campaign_statuses")
  numbers <- list(
    peak_weeks = peak_weeks, floor_weeks = floor_weeks,
    neighbour_km = neighbour_km, window_days = window_days,
    active_months = active_months, min_n = min_n, floor_pct = floor_pct,
    trend_years = trend_years, trend_min_n = trend_min_n,
    new_site_months = new_site_months, trend_drop_pts = trend_drop_pts,
    isolation_min_samples = isolation_min_samples,
    isolation_min_pct = isolation_min_pct, days_per_month = days_per_month
  )
  for (nm in names(numbers)) {
    .essc_check_number(numbers[[nm]], nm)
  }
  .essc_check_number(trend_alpha, "trend_alpha")
  if (trend_alpha <= 0 || trend_alpha >= 1) {
    cli::cli_abort("{.arg trend_alpha} must lie between 0 and 1.")
  }
  if (!is.numeric(mid_weeks) || length(mid_weeks) != 2L ||
        anyNA(mid_weeks) || mid_weeks[1] >= mid_weeks[2]) {
    cli::cli_abort(c(
      "{.arg mid_weeks} must be two increasing numbers.",
      "i" = "The default is {.code c(6, 12)}."
    ))
  }
  label_names <- c(
    "below_floor", "falling", "low_isolation", "working", "unjudged"
  )
  if (!is.character(verdict_labels) ||
        !setequal(names(verdict_labels) %||% "", label_names) ||
        anyNA(verdict_labels) || anyDuplicated(verdict_labels) > 0) {
    cli::cli_abort(c(
      "{.arg verdict_labels} must be five distinct, named labels.",
      "i" = "Name them {.val {label_names}}."
    ))
  }
  if (!is.numeric(profile_breaks) || is.unsorted(profile_breaks) ||
        length(profile_labels) != length(profile_breaks) - 1L) {
    cli::cli_abort(c(
      "{.arg profile_breaks} must be increasing, with one more value than
      {.arg profile_labels}."
    ))
  }
  structure(
    c(
      list(
        opv_types = opv_types, campaign_statuses = campaign_statuses,
        mid_weeks = mid_weeks, trend_alpha = trend_alpha,
        verdict_labels = verdict_labels[label_names],
        profile_breaks = profile_breaks, profile_labels = profile_labels,
        crs = crs
      ),
      numbers
    ),
    class = "es_score_settings"
  )
}

# ---- exported indicator tables -----------------------------------------------

#' Environmental site scorecard
#'
#' Judges whether each environmental surveillance site detects virus when
#' virus is there, on two measures, and combines them with the site's own
#' trend and its enterovirus isolation into one verdict.
#'
#' @section Measures:
#' Every sample is placed against the latest campaign that started on or before
#' its collection date in the site's own district (matched on the district
#' GUID). Only campaigns with a vaccine type in `opv_types` and a status in
#' `campaign_statuses` count. A site in a district with no such campaign has
#' no samples on this measure.
#'
#' * **Campaign response.** The denominator is the number of the site's
#'   samples collected between 0 and `peak_weeks` weeks (default 4) after the
#'   start of the latest campaign in its district. The numerator is the number
#'   of those samples in which Sabin virus was detected.
#' * **Neighbour recovery.** An occasion is a month in which the site was
#'   sampled and another site within `neighbour_km` kilometres (default 20)
#'   recovered target virus (by default WPV1 or VDPV2) in a month no more than
#'   `window_days` days away (default 62, which is about two months either
#'   side). The denominator is the number of such occasions. The numerator is
#'   the number of those months in which the site itself recovered target
#'   virus. Distances are straight-line distances in the projection `crs`.
#'   Two sites at the same coordinates are not counted as neighbours.
#'
#' Each rate carries an exact (Clopper-Pearson) 95% interval. Two further
#' measures are reported beside the verdict but do not decide it: retention,
#' the share of samples between `mid_weeks` weeks after a campaign with Sabin
#' detected, as a percentage of campaign response; and background, the share
#' of samples more than `floor_weeks` weeks after a campaign with Sabin
#' detected. Retention is `NA` when the site's campaign response rate is zero
#' or missing, since it cannot be expressed as a percentage of nothing.
#'
#' @section Trend:
#' A site's campaign response samples are split at `trend_years` years before
#' the latest sample in the network (default 3). For a site whose first sample
#' falls after that date, the split is instead `new_site_months` months
#' (default 12) before the latest sample. Both parts need at least
#' `trend_min_n` samples (default 5). A site has declined when its recent
#' percentage is at least `trend_drop_pts` points (default 15) below its
#' earlier percentage and a two-sided Fisher's exact test gives a p-value below
#' `trend_alpha` (default 0.05).
#'
#' @section Verdict:
#' Only active sites are judged: those whose last sample falls within
#' `active_months` months (default 12) of the latest sample in the network.
#' The judged percentage is the recent campaign response percentage where the
#' trend could be tested, and the whole-record campaign response percentage
#' otherwise. Rules are applied in this order and the first that holds wins:
#'
#' 1. Fewer than `min_n` campaign response samples (default 5), including none:
#'    `unjudged` ("Too few samples").
#' 2. Judged percentage below `floor_pct` (default 30): `below_floor`.
#' 3. A decline as defined above: `falling`.
#' 4. At least `isolation_min_samples` samples with a final result (default 5)
#'    and enterovirus isolation below `isolation_min_pct` percent (default 50):
#'    `low_isolation`. This rule needs `isolation`.
#' 5. Otherwise: `working`.
#'
#' @param samples Cleaned environmental sample table, one row per sample,
#'   such as `polished_es`. Duplicate sample identifiers are kept once.
#' @param campaigns Cleaned campaign table, such as `polished_sia`.
#' @param isolation Optional site table carrying each site's enterovirus
#'   isolation percentage (`ev_pct`, on a scale of 0 to 100), the number of
#'   samples with a final result (`resolved`) and the months sampled
#'   (`months_sampled`), one row per site, such as the result of
#'   [calc_es_site_performance()]. Without it, the low isolation rule is not
#'   applied and those columns are `NA`.
#' @param country_iso ISO3 codes of the countries whose sites are reported.
#'   `NULL` (default) reports every country in `samples`.
#' @param benchmark_iso ISO3 codes of the countries whose samples, campaigns
#'   and sites make up the network: the pool for neighbours, the latest
#'   sample and the network rates. Defaults to `country_iso`.
#' @param from First collection date read. Default 1 January 2016.
#' @param as_of The reporting date, as a `Date` or a string that `as.Date()`
#'   reads. Defaults to `Sys.Date()`. It is the last collection date read.
#' @param settings Windows, distances, thresholds and labels, built with
#'   [es_score_settings()].
#' @param cols Named list of column overrides keyed by table (`samples`,
#'   `campaigns`, `isolation`), in the style of [calc_polio_indicators()].
#'   See [es_score_cols()] for the keys and defaults.
#'
#' @return A tibble with one row per active site in `country_iso`, sorted by
#'   verdict and judged percentage. For each measure (`campaign_response`,
#'   `neighbour_recovery`) it carries `n_`, `k_`, `rate_`, `lo_` and `hi_`
#'   columns, followed by the isolation, retention, background and trend
#'   columns, the percentages, `judged_pct`, `judged_basis` and the `verdict`
#'   factor. The `rate_`, `lo_` and `hi_` columns are proportions on a scale
#'   of 0 to 1. Every `_pct` column is on a scale of 0 to 100, and
#'   `drop_pts` is in percentage points. `retention_pct` is a percentage of
#'   campaign response, so it can exceed 100.
#' @examples
#' \donttest{
#' es_score_cols()
#' }
#' @export
calc_es_scorecard <- function(
  samples,
  campaigns,
  isolation = NULL,
  country_iso = NULL,
  benchmark_iso = country_iso,
  from = as.Date("2016-01-01"),
  as_of = Sys.Date(),
  settings = es_score_settings(),
  cols = list()
) {
  .essc_check_settings(settings)
  m <- .essc_merge_cols(cols)
  if (!is.null(country_iso)) .essc_check_chr(country_iso, "country_iso")
  base <- .essc_base(
    samples, campaigns, benchmark_iso, from, as_of, settings, m
  )
  measured <- .essc_measured(base, settings)
  network <- .essc_network(measured, settings)
  s <- base$samples

  retention <- s |>
    dplyr::filter(
      weeks > settings$mid_weeks[1], weeks <= settings$mid_weeks[2]
    ) |>
    dplyr::summarise(mid_n = dplyr::n(), mid = mean(sabin), .by = site)

  background <- s |>
    dplyr::filter(weeks > settings$floor_weeks) |>
    dplyr::summarise(floor_n = dplyr::n(), floor = mean(sabin), .by = site)

  trend <- .essc_trend(s, base$latest, settings)

  judged <- measured |>
    dplyr::filter(active) |>
    dplyr::mutate(
      rate = k / n,
      lo = round(.essc_binom_ci(k, n, 1L), 3),
      hi = round(.essc_binom_ci(k, n, 2L), 3),
      .by = c(site, axis)
    )

  wide <- judged |>
    dplyr::select(
      iso, site, site_code, site_name, adm1, adm2, x, y, last_sample,
      samples_per_month, panel_samples, axis, n, k, rate, lo, hi
    ) |>
    tidyr::pivot_wider(
      names_from = axis, values_from = c(n, k, rate, lo, hi),
      names_expand = TRUE
    )
  if (!is.null(country_iso)) {
    wide <- dplyr::filter(wide, iso %in% country_iso)
  }

  .essc_assemble(
    wide, .essc_isolation(isolation, m$isolation), retention, background,
    trend, settings
  )
}

#' Network rate of each scorecard measure
#'
#' Pools the two scorecard measures over every active site in the network that
#' has at least `min_n` samples or occasions on the measure. The rate is the
#' sum of the numerators over the sum of the denominators, so a site sampled a
#' few times weighs less than one sampled many times. Measures are defined in
#' [calc_es_scorecard()].
#'
#' @inheritParams calc_es_scorecard
#' @param benchmark_iso ISO3 codes of the countries that make up the network.
#'   `NULL` (default) uses every country in `samples`.
#'
#' @return A tibble with one row per measure (`axis`) and columns `sites`,
#'   `occasions` (the pooled denominator) and `network_rate` (a proportion on
#'   a scale of 0 to 1).
#' @export
calc_es_score_network <- function(
  samples,
  campaigns,
  benchmark_iso = NULL,
  from = as.Date("2016-01-01"),
  as_of = Sys.Date(),
  settings = es_score_settings(),
  cols = list()
) {
  .essc_check_settings(settings)
  m <- .essc_merge_cols(cols)
  base <- .essc_base(
    samples, campaigns, benchmark_iso, from, as_of, settings, m
  )
  .essc_network(.essc_measured(base, settings), settings)
}

#' Sabin detection by weeks since a campaign
#'
#' The network response profile behind the campaign response window. Samples
#' that follow a campaign in their own district are grouped by country and by
#' band of weeks since the start of that campaign (`profile_breaks` in
#' [es_score_settings()]). For each band, the denominator is the number of
#' samples and the numerator the number with Sabin virus detected.
#'
#' @inheritParams calc_es_score_network
#'
#' @return A tibble with columns `iso`, `bin` (a factor), `samples` and
#'   `sabin_pct` (on a scale of 0 to 100, rounded to one decimal place),
#'   sorted by country and band.
#' @export
calc_es_score_profile <- function(
  samples,
  campaigns,
  benchmark_iso = NULL,
  from = as.Date("2016-01-01"),
  as_of = Sys.Date(),
  settings = es_score_settings(),
  cols = list()
) {
  .essc_check_settings(settings)
  m <- .essc_merge_cols(cols)
  base <- .essc_base(
    samples, campaigns, benchmark_iso, from, as_of, settings, m
  )
  base$samples |>
    dplyr::filter(!is.na(weeks)) |>
    dplyr::mutate(
      bin = cut(
        weeks, breaks = settings$profile_breaks,
        labels = settings$profile_labels
      )
    ) |>
    dplyr::summarise(
      samples = dplyr::n(),
      sabin_pct = round(100 * mean(sabin), 1),
      .by = c(iso, bin)
    ) |>
    dplyr::arrange(iso, bin)
}

#' Default columns read by the environmental site scorecard
#'
#' Lists, by table, the column each scorecard input is read from. Keys shared
#' with [calc_polio_indicators()] take the same defaults. Override any of them
#' through the `cols` argument, for example
#' `cols = list(campaigns = list(vaccine_type = "vaccine_type"))`.
#'
#' The `samples` keys `sabin` and `target` each take several columns. A sample
#' has Sabin (or target) virus detected when any of those columns is `TRUE`, a
#' non-zero number or a non-blank string. By default target virus is WPV1 or
#' VDPV2.
#'
#' The `isolation` defaults are the column names that
#' [calc_es_site_performance()] returns, so its result can be passed as
#' `isolation` directly.
#'
#' @return A named list with elements `samples`, `campaigns` and `isolation`.
#' @examples
#' es_score_cols()$campaigns
#' @export
es_score_cols <- function() {
  base <- .polio_default_cols()
  list(
    samples = list(
      country = "country_iso3code",
      sample_id = "sample_id",
      site_guid = "site_guid",
      site_code = "site_code",
      site_name = base$es$site,
      adm1 = base$es$adm1,
      adm2 = base$es$adm2,
      adm2_guid = base$es$adm2_guid,
      longitude = "site_x_coordinate",
      latitude = "site_y_coordinate",
      collection_date = base$es$collection_date,
      sabin = c("vaccine1", "vaccine2", "vaccine3"),
      target = c("wild1", "vdpv2")
    ),
    campaigns = list(
      country = "country_iso3code",
      adm2_guid = base$sia$adm2_guid,
      status = "activity_status",
      vaccine_type = "activity_vaccine_type",
      start_date = "activity_date_from"
    ),
    isolation = list(
      site_guid = "site_guid",
      ev_pct = "ev_pct",
      resolved = "resolved",
      months_sampled = "months_sampled"
    )
  )
}

# ---- internal: inputs --------------------------------------------------------

#' Merge user column overrides into the scorecard defaults.
#' @keywords internal
#' @noRd
.essc_merge_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort("{.arg cols} must be a named list.")
  }
  base <- es_score_cols()
  unknown <- setdiff(names(cols), names(base))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown source{?s} in {.arg cols}: {.val {unknown}}.",
      "i" = "Use {.val {names(base)}}."
    ))
  }
  for (src in names(cols)) {
    base[[src]] <- utils::modifyList(base[[src]], cols[[src]])
  }
  base
}

#' Abort unless every mapped column is in the table.
#' @keywords internal
#' @noRd
.essc_require_cols <- function(df, m, arg) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg {arg}} must be a data frame or tibble.")
  }
  missing <- setdiff(unlist(m, use.names = FALSE), names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg {arg}} lacks column{?s} {.val {missing}}.",
      "i" = "Map them with {.arg cols}; see {.fn es_score_cols}."
    ))
  }
  invisible(df)
}

#' District GUID as a join key: braces stripped, upper case.
#' @keywords internal
#' @noRd
.essc_guid_key <- function(x) {
  toupper(gsub("[{}]", "", as.character(x)))
}

#' Whether a detection column records a detection.
#' @keywords internal
#' @noRd
.essc_detected <- function(x) {
  if (is.logical(x)) {
    return(x %in% TRUE)
  }
  if (is.numeric(x)) {
    return(!is.na(x) & x != 0)
  }
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x)
}

#' One validated date.
#' @keywords internal
#' @noRd
.essc_as_date <- function(x, arg) {
  out <- tryCatch(as.Date(x), error = function(e) NA)
  if (length(out) != 1L || is.na(out)) {
    cli::cli_abort("{.arg {arg}} must be a single date.")
  }
  out
}

#' Samples placed against their district's campaigns, and the latest sample.
#' @keywords internal
#' @noRd
.essc_base <- function(samples, campaigns, benchmark_iso, from, as_of,
                       settings, m) {
  .essc_require_cols(samples, m$samples, "samples")
  .essc_require_cols(campaigns, m$campaigns, "campaigns")
  if (!is.null(benchmark_iso)) .essc_check_chr(benchmark_iso, "benchmark_iso")
  from <- .essc_as_date(from, "from")
  as_of <- .essc_as_date(as_of, "as_of")
  if (from > as_of) {
    cli::cli_abort("{.arg from} must not fall after {.arg as_of}.")
  }
  e <- m$samples

  samples <- samples |>
    dplyr::transmute(
      iso = as.character(.data[[e$country]]),
      sample_id = .data[[e$sample_id]],
      site = as.character(.data[[e$site_guid]]),
      site_code = .data[[e$site_code]],
      site_name = .data[[e$site_name]],
      adm1 = .data[[e$adm1]],
      adm2 = .data[[e$adm2]],
      guid = .essc_guid_key(.data[[e$adm2_guid]]),
      x = as.numeric(.data[[e$longitude]]),
      y = as.numeric(.data[[e$latitude]]),
      date = as.Date(.data[[e$collection_date]]),
      sabin = as.integer(
        dplyr::if_any(dplyr::all_of(e$sabin), .essc_detected)
      ),
      target = as.integer(
        dplyr::if_any(dplyr::all_of(e$target), .essc_detected)
      )
    ) |>
    dplyr::filter(
      is.null(benchmark_iso) | iso %in% benchmark_iso,
      !is.na(date), date >= from, date <= as_of, !is.na(site),
      !is.na(x), !is.na(y)
    ) |>
    dplyr::distinct(sample_id, .keep_all = TRUE) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month"))

  if (nrow(samples) == 0) {
    cli::cli_abort(c(
      "No environmental samples remain to score.",
      "i" = "Check {.arg benchmark_iso}, {.arg from}, {.arg as_of} and the
      site coordinates."
    ))
  }

  a <- m$campaigns
  campaigns <- campaigns |>
    dplyr::transmute(
      iso = as.character(.data[[a$country]]),
      status = .data[[a$status]],
      vaccine = .data[[a$vaccine_type]],
      guid = .essc_guid_key(.data[[a$adm2_guid]]),
      start = as.Date(.data[[a$start_date]])
    ) |>
    dplyr::filter(
      is.null(benchmark_iso) | iso %in% benchmark_iso,
      status %in% settings$campaign_statuses,
      vaccine %in% settings$opv_types,
      !is.na(start), !is.na(guid)
    ) |>
    dplyr::distinct(guid, start)

  samples <- samples |>
    dplyr::left_join(
      campaigns,
      by = dplyr::join_by(guid, closest(date >= start)),
      na_matches = "never"
    ) |>
    dplyr::mutate(weeks = as.numeric(date - start) / 7) |>
    dplyr::select(-start)

  list(samples = samples, latest = max(samples$date))
}

# ---- internal: measures ------------------------------------------------------

#' Site details, activity and both measures, one row per site and measure.
#' @keywords internal
#' @noRd
.essc_measured <- function(base, settings) {
  s <- base$samples

  site_meta <- s |>
    dplyr::summarise(
      site_code = dplyr::first(site_code),
      site_name = dplyr::first(site_name),
      adm1 = dplyr::first(adm1),
      adm2 = dplyr::first(adm2),
      x = dplyr::first(x),
      y = dplyr::first(y),
      last_sample = max(date),
      panel_samples = dplyr::n(),
      span_months = as.numeric(diff(range(date))) / settings$days_per_month,
      .by = c(iso, site)
    ) |>
    dplyr::mutate(
      samples_per_month = panel_samples / pmax(span_months, 1),
      active = as.numeric(base$latest - last_sample) /
        settings$days_per_month <= settings$active_months
    )

  sabin_axis <- s |>
    dplyr::filter(weeks <= settings$peak_weeks) |>
    dplyr::summarise(n = dplyr::n(), k = sum(sabin), .by = site) |>
    dplyr::mutate(axis = "campaign_response")

  neighbours <- s |>
    dplyr::summarise(x = dplyr::first(x), y = dplyr::first(y), .by = site) |>
    .essc_neighbours(settings$neighbour_km, settings$crs)

  found <- s |>
    dplyr::filter(target == 1L) |>
    dplyr::distinct(other = site, other_month = month)

  caught <- s |>
    dplyr::filter(target == 1L) |>
    dplyr::distinct(site, month) |>
    dplyr::mutate(hit = 1L)

  target_axis <- s |>
    dplyr::distinct(site, month) |>
    dplyr::inner_join(neighbours, by = "site", relationship = "many-to-many") |>
    dplyr::inner_join(found, by = "other", relationship = "many-to-many") |>
    dplyr::filter(
      abs(as.numeric(month - other_month)) <= settings$window_days
    ) |>
    dplyr::distinct(site, month) |>
    dplyr::left_join(caught, by = c("site", "month")) |>
    dplyr::summarise(
      n = dplyr::n(), k = sum(dplyr::coalesce(hit, 0L)), .by = site
    ) |>
    dplyr::mutate(axis = "neighbour_recovery")

  dplyr::bind_rows(sabin_axis, target_axis) |>
    dplyr::mutate(
      axis = factor(axis, levels = c("campaign_response", "neighbour_recovery"))
    ) |>
    dplyr::inner_join(site_meta, by = "site")
}

#' Pairs of distinct sites within `max_km` of each other, in both directions.
#' @keywords internal
#' @noRd
.essc_neighbours <- function(points, max_km, crs) {
  n_sites <- nrow(points)
  km <- points |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
    sf::st_transform(crs) |>
    sf::st_distance() |>
    as.numeric()
  tibble::tibble(
    site = rep(points$site, times = n_sites),
    other = rep(points$site, each = n_sites),
    km = km / 1000
  ) |>
    dplyr::filter(km <= max_km, km > 0) |>
    dplyr::select(site, other)
}

#' Pooled rate per measure over active sites with enough occasions.
#' @keywords internal
#' @noRd
.essc_network <- function(measured, settings) {
  measured |>
    dplyr::filter(active, n >= settings$min_n) |>
    dplyr::summarise(
      sites = dplyr::n(), occasions = sum(n), network_rate = sum(k) / sum(n),
      .by = axis
    ) |>
    dplyr::mutate(axis = as.character(axis)) |>
    dplyr::arrange(axis)
}

#' Earlier against recent campaign response, with Fisher's exact test.
#' @keywords internal
#' @noRd
.essc_trend <- function(s, latest, settings) {
  trend_cut <- lubridate::add_with_rollback(
    latest, -lubridate::years(settings$trend_years)
  )
  new_site_cut <- lubridate::add_with_rollback(
    latest, -lubridate::period(settings$new_site_months, "months")
  )
  site_start <- dplyr::summarise(s, first_date = min(date), .by = site)

  s |>
    dplyr::filter(weeks <= settings$peak_weeks) |>
    dplyr::left_join(site_start, by = "site") |>
    dplyr::mutate(
      new_site = first_date >= trend_cut,
      era = dplyr::if_else(
        date >= dplyr::if_else(new_site, new_site_cut, trend_cut),
        "recent", "earlier"
      ),
      era = factor(era, levels = c("earlier", "recent"))
    ) |>
    dplyr::summarise(
      n = dplyr::n(), k = sum(sabin), .by = c(site, new_site, era)
    ) |>
    tidyr::pivot_wider(
      names_from = era, values_from = c(n, k), names_expand = TRUE
    ) |>
    dplyr::filter(
      !is.na(n_recent), !is.na(n_earlier),
      n_recent >= settings$trend_min_n, n_earlier >= settings$trend_min_n
    ) |>
    dplyr::mutate(
      earlier_pct = round(100 * k_earlier / n_earlier, 1),
      recent_pct = round(100 * k_recent / n_recent, 1),
      drop_pts = round(earlier_pct - recent_pct, 1),
      p_value = .essc_fisher_p(k_recent, n_recent, k_earlier, n_earlier),
      .by = site
    ) |>
    dplyr::mutate(
      declined = drop_pts >= settings$trend_drop_pts &
        p_value < settings$trend_alpha
    )
}

#' Exact binomial interval bound for one site and measure.
#' @keywords internal
#' @noRd
.essc_binom_ci <- function(k, n, side) {
  if (length(k) != 1L) {
    return(rep(NA_real_, length(k)))
  }
  stats::binom.test(k, n)$conf.int[side]
}

#' Two-sided Fisher's exact p-value for one site's earlier and recent counts.
#' @keywords internal
#' @noRd
.essc_fisher_p <- function(kr, nr, ke, ne) {
  if (length(kr) != 1L) {
    return(rep(NA_real_, length(kr)))
  }
  counts <- matrix(c(kr, nr - kr, ke, ne - ke), nrow = 2)
  stats::fisher.test(counts)$p.value
}

#' Standardise the optional isolation table to site-level columns.
#' @keywords internal
#' @noRd
.essc_isolation <- function(isolation, m) {
  if (is.null(isolation)) {
    return(tibble::tibble(
      site = character(), ev_pct = numeric(), resolved = integer(),
      months_sampled = integer()
    ))
  }
  .essc_require_cols(isolation, m, "isolation")
  out <- dplyr::transmute(
    isolation,
    site = as.character(.data[[m$site_guid]]),
    ev_pct = .data[[m$ev_pct]],
    resolved = .data[[m$resolved]],
    months_sampled = .data[[m$months_sampled]]
  )
  if (anyDuplicated(out$site) > 0) {
    cli::cli_abort("{.arg isolation} must hold one row per site.")
  }
  out
}

#' Join the site table, derive the percentages and apply the verdict rule.
#' @keywords internal
#' @noRd
.essc_assemble <- function(wide, isolation, retention, background, trend,
                           settings) {
  lab <- settings$verdict_labels
  min_trend <- settings$trend_min_n
  wide |>
    dplyr::left_join(isolation, by = "site") |>
    dplyr::left_join(retention, by = "site") |>
    dplyr::left_join(background, by = "site") |>
    dplyr::left_join(
      dplyr::select(
        trend, site, new_site, earlier_pct, recent_pct, drop_pts, p_value,
        declined, n_earlier, n_recent, k_earlier, k_recent
      ),
      by = "site"
    ) |>
    dplyr::mutate(
      retention_pct = dplyr::if_else(
        rate_campaign_response > 0,
        round(100 * mid / rate_campaign_response, 1),
        NA_real_
      ),
      background_pct = round(100 * floor, 1),
      campaign_response_pct = round(100 * rate_campaign_response, 1),
      neighbour_recovery_pct = round(100 * rate_neighbour_recovery, 1),
      judged_pct = dplyr::if_else(
        !is.na(n_recent) & n_recent >= min_trend,
        recent_pct, campaign_response_pct
      ),
      judged_basis = dplyr::if_else(
        !is.na(n_recent) & n_recent >= min_trend,
        "recent window", "whole record"
      ),
      verdict = dplyr::case_when(
        dplyr::coalesce(n_campaign_response, 0L) < settings$min_n ~
          lab[["unjudged"]],
        judged_pct < settings$floor_pct ~ lab[["below_floor"]],
        declined %in% TRUE ~ lab[["falling"]],
        resolved >= settings$isolation_min_samples &
          ev_pct < settings$isolation_min_pct ~ lab[["low_isolation"]],
        .default = lab[["working"]]
      ),
      verdict = factor(verdict, levels = unname(lab))
    ) |>
    dplyr::arrange(verdict, judged_pct)
}

# ---- internal: argument checks -----------------------------------------------

#' Abort unless `x` is one non-negative number.
#' @keywords internal
#' @noRd
.essc_check_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    cli::cli_abort("{.arg {arg}} must be a single non-negative number.")
  }
  invisible(x)
}

#' Abort unless `x` is a non-empty character vector.
#' @keywords internal
#' @noRd
.essc_check_chr <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must be a non-empty character vector.")
  }
  invisible(x)
}

#' Abort unless `settings` came from es_score_settings().
#' @keywords internal
#' @noRd
.essc_check_settings <- function(settings) {
  if (!inherits(settings, "es_score_settings")) {
    cli::cli_abort(
      "{.arg settings} must be built with {.fn es_score_settings}."
    )
  }
  invisible(settings)
}
