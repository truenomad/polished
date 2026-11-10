# =============================================================================
# Virus detections in comparison windows
#
# Counts poliovirus detections in fixed windows of days before a reporting
# date, compares each window with the one before it, and names the direction.
# =============================================================================

#' Default comparison windows for virus detections
#'
#' Returns the four windows the detection panels use, each set in days back
#' from the reporting date. A detection falls in a window when its virus date
#' lies between `as_of - from` and `as_of - to`, both ends included.
#'
#' The 6-month and 12-month windows each carry a reporting lag of about two
#' months, so they run longer than their names suggest.
#' \itemize{
#'   \item `last_6` runs from 218 days before `as_of` to `as_of`.
#'   \item `preceding_6` runs from 402 days before `as_of` to 219 days before.
#'   \item `last_12` runs from 400 days before `as_of` to `as_of`.
#'   \item `preceding_12` runs from 766 days before `as_of` to 401 days
#'     before.
#' }
#'
#' @return A tibble with one row per window and the columns `window`
#'   (character), `from` and `to` (integer days before the reporting date,
#'   with `from` at least `to`).
#' @examples
#' detection_default_windows()
#' @seealso [calc_detection_windows()], [detection_default_pairs()]
#' @export
detection_default_windows <- function() {
  tibble::tibble(
    window = c("last_6", "preceding_6", "last_12", "preceding_12"),
    from = c(218L, 402L, 400L, 766L),
    to = c(0L, 219L, 0L, 401L)
  )
}

#' Default window pairs for the previous-period comparison
#'
#' Pairs each current window with the window it is compared against:
#' `last_6` with `preceding_6`, and `last_12` with `preceding_12`.
#'
#' @return A tibble with the columns `current` and `previous`, each naming a
#'   window in [detection_default_windows()].
#' @examples
#' detection_default_pairs()
#' @export
detection_default_pairs <- function() {
  tibble::tibble(
    current = c("last_6", "last_12"),
    previous = c("preceding_6", "preceding_12")
  )
}

#' Select the virus detections that count
#'
#' Keeps the poliovirus detections in a set of countries whose virus type
#' matches a pattern, optionally from a set of surveillance sources, and
#' keeps one row per detection.
#'
#' The virus type is first written in one form: white space is removed and a
#' leading `WILD` becomes `WPV`, so that the raw POLIS label `WILD1` and the
#' cleaned label `WPV 1` both read `WPV1`. The pattern is matched against
#' this form, and the returned table carries it in the virus type column.
#'
#' A detection is identified by the columns in `key`. By default one EPID
#' with two serotypes is two detections, and repeated records of one EPID and
#' serotype are one. When `latest_by` is given, rows are sorted on those
#' columns in descending order before duplicates are dropped, so the most
#' recently edited record is kept. Otherwise the first record in the input is
#' kept.
#'
#' The function does not add laboratory-confirmed detections that POLIS does
#' not yet hold. A caller that needs them adds them to `detections` first.
#'
#' @param detections A data frame of virus detections, one row per record,
#'   such as the output of [clean_virus()].
#' @param countries ISO3 codes of the countries to keep. `NULL` keeps every
#'   country with a code. Default `c("AFG", "PAK")`.
#' @param vtype_pattern Regular expression a virus type must match to count.
#'   Default `"^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV"`, which keeps wild poliovirus
#'   and every kind of vaccine-derived poliovirus.
#' @param sources Surveillance sources to keep, matched against the source
#'   column. `NULL` (the default) keeps every source.
#' @param key Columns that identify one detection. Default
#'   `c("epid", "vtype")`. Give the names as they appear in `detections`.
#' @param latest_by Columns to sort on, in descending order, before duplicates
#'   are dropped. Default `NULL`, which keeps the first record in the input.
#' @param cols Named list overriding the default column names, keyed by
#'   table, for example `list(detections = list(vtype = "VirusTypeName"))`.
#'   This function reads the `detections` roles `epid`, `vtype`, `country`,
#'   `source` and `virus_date`, by default the [clean_virus()] names `epid`,
#'   `vtype`, `country_iso3code`, `source` and `virus_date`.
#'
#' @return A tibble with the columns of `detections`, one row per detection,
#'   ordered by virus date, country and EPID.
#' @examples
#' detections <- tibble::tibble(
#'   epid = c("A-1", "A-1", "A-2", "B-1"),
#'   vtype = c("WPV 1", "WPV 1", "cVDPV 2", "WPV 1"),
#'   country_iso3code = c("AFG", "AFG", "AFG", "NGA"),
#'   source = "AFP",
#'   virus_date = as.Date(c("2024-01-05", "2024-01-05", "2024-02-01",
#'                          "2024-03-01"))
#' )
#' detection_slice(detections)
#' detection_slice(detections, vtype_pattern = "^WPV1$")
#' @export
detection_slice <- function(
  detections,
  countries = c("AFG", "PAK"),
  vtype_pattern = "^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV",
  sources = NULL,
  key = c("epid", "vtype"),
  latest_by = NULL,
  cols = list()
) {
  .detw_check_df(detections, "detections")
  col <- .detw_merge_cols(cols)
  if (!is.character(vtype_pattern) || length(vtype_pattern) != 1 ||
        is.na(vtype_pattern)) {
    cli::cli_abort("{.arg vtype_pattern} must be a single regular expression.")
  }
  if (!is.null(countries) && !is.character(countries)) {
    cli::cli_abort("{.arg countries} must be a character vector or NULL.")
  }
  if (!is.null(sources) && !is.character(sources)) {
    cli::cli_abort("{.arg sources} must be a character vector or NULL.")
  }
  if (!is.character(key) || length(key) == 0) {
    cli::cli_abort("{.arg key} must name at least one column.")
  }
  needed <- c(
    col$vtype, col$country, col$virus_date, key, latest_by,
    if (!is.null(sources)) col$source
  )
  .detw_require(detections, unique(needed), "detections")

  out <- detections |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(col$vtype), .detw_normalise_vtype)
    ) |>
    dplyr::filter(
      !is.na(.data[[col$country]]),
      is.null(countries) | .data[[col$country]] %in% countries,
      stringr::str_detect(.data[[col$vtype]], vtype_pattern) %in% TRUE
    )

  if (!is.null(sources)) {
    out <- dplyr::filter(out, .data[[col$source]] %in% sources)
  }

  if (!is.null(latest_by)) {
    out <- dplyr::arrange(
      out, dplyr::across(dplyr::all_of(latest_by), dplyr::desc)
    )
  }

  order_cols <- intersect(
    c(col$virus_date, col$country, col$epid), names(out)
  )

  out |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key)), .keep_all = TRUE) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(order_cols))) |>
    tibble::as_tibble()
}

#' Count virus detections in each comparison window
#'
#' Counts the detections whose virus date falls in each window, overall or
#' by administrative unit. A detection falls in a window when its virus date
#' lies between `as_of - from` and `as_of - to` days, both ends included. A
#' detection with no virus date falls in no window. Every unit present in
#' `detections` gets a row for every window, with zero where nothing was
#' detected.
#'
#' Five counts are returned for each window:
#' \itemize{
#'   \item `detections`, every detection in the window;
#'   \item `reported_cases`, detections whose source is not in `env_sources`,
#'     so a missing source counts as a case;
#'   \item `positive_es`, detections whose source is in `env_sources`;
#'   \item `provinces`, the number of distinct non-missing provinces;
#'   \item `clusters`, the number of distinct clusters, leaving out missing
#'     and empty cluster names.
#' }
#'
#' @param detections A data frame with one row per detection, such as the
#'   output of [detection_slice()].
#' @param as_of The reporting date, as a `Date` or a string that `as.Date()`
#'   reads. Defaults to `Sys.Date()`. The windows are counted back from it.
#' @param windows A data frame with the columns `window`, `from` and `to`,
#'   where `from` and `to` are whole days before `as_of` and `from` is at
#'   least `to`. Default [detection_default_windows()].
#' @param level Administrative level to count by: `NULL` (the default) for
#'   one row per window over all detections, or `"adm0"` (country), `"adm1"`
#'   (province) or `"adm2"` (district). Detections with no identifier at that
#'   level are left out.
#' @param env_sources Source values that mark an environmental sample.
#'   Default `"Environmental"`.
#' @param cols Named list overriding the default column names, keyed by
#'   table, for example `list(detections = list(adm1 = "province"))`. This
#'   function reads the `detections` roles `virus_date`, `source`, `adm1`
#'   (the province counted in `provinces`) and `cluster`, by default the
#'   [clean_virus()] names `virus_date`, `source`, `adm1` and
#'   `virus_cluster`, and with `level` the GUID and name roles of that level
#'   (`adm0_guid` and `adm0`, `adm1_guid` and `adm1`, or `adm2_guid` and
#'   `adm2`).
#'
#' @return A tibble with one row per unit and window, in the order of
#'   `windows`: with `level`, the columns `level`, `guid` and `name`; then
#'   `window`, `from`, `to`, `window_start`, `window_end` and the five
#'   counts.
#' @examples
#' detections <- tibble::tibble(
#'   virus_date = as.Date(c("2024-06-01", "2024-01-15", "2023-05-01")),
#'   source = c("AFP", "Environmental", "AFP"),
#'   adm1 = c("Kandahar", "Kandahar", "Helmand"),
#'   virus_cluster = c("C1", "C2", NA)
#' )
#' calc_detection_windows(detections, as_of = "2024-07-01")
#' @export
calc_detection_windows <- function(
  detections,
  as_of = Sys.Date(),
  windows = detection_default_windows(),
  level = NULL,
  env_sources = "Environmental",
  cols = list()
) {
  .detw_check_df(detections, "detections")
  as_of <- .detw_check_as_of(as_of)
  windows <- .detw_check_windows(windows)
  .detw_check_level(level)
  col <- .detw_merge_cols(cols)
  if (!is.character(env_sources)) {
    cli::cli_abort("{.arg env_sources} must be a character vector.")
  }
  keys <- .detw_level_keys(level)
  .detw_require(
    detections,
    unique(c(
      unlist(col[keys], use.names = FALSE), col$virus_date, col$source,
      col$adm1, col$cluster
    )),
    "detections"
  )

  slim <- tibble::tibble(
    .date = lubridate::as_date(detections[[col$virus_date]]),
    .is_env = detections[[col$source]] %in% env_sources,
    .province = detections[[col$adm1]],
    .cluster = detections[[col$cluster]]
  )
  by <- character(0)
  groups <- tibble::tibble(.rows = 1)
  if (!is.null(level)) {
    by <- c("guid", "name")
    slim <- slim |>
      dplyr::mutate(
        guid = as.character(detections[[col[[keys[1]]]]]),
        name = as.character(detections[[col[[keys[2]]]]])
      ) |>
      dplyr::filter(!is.na(.data$guid)) |>
      dplyr::mutate(name = .polio_consensus_name(.data$name), .by = "guid")
    groups <- dplyr::distinct(dplyr::select(slim, "guid", "name"))
  }
  slim <- dplyr::filter(slim, !is.na(.date))

  window_dates <- windows |>
    dplyr::mutate(
      window_start = as_of - .data$from,
      window_end = as_of - .data$to,
      .order = dplyr::row_number()
    )

  counts <- dplyr::cross_join(window_dates, slim) |>
    dplyr::filter(
      .date >= .data$window_start,
      .date <= .data$window_end
    ) |>
    dplyr::summarise(
      detections = dplyr::n(),
      reported_cases = sum(!.is_env),
      positive_es = sum(.is_env),
      provinces = dplyr::n_distinct(.province, na.rm = TRUE),
      clusters = dplyr::n_distinct(
        .cluster[!is.na(.cluster) & .cluster != ""]
      ),
      .by = dplyr::all_of(c(by, "window"))
    )

  dplyr::cross_join(groups, window_dates) |>
    dplyr::left_join(counts, by = c(by, "window")) |>
    dplyr::mutate(
      dplyr::across(
        c("detections", "reported_cases", "positive_es", "provinces",
          "clusters"),
        \(x) dplyr::coalesce(as.integer(x), 0L)
      )
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(rev(by))), .data$.order) |>
    .detw_add_level(level) |>
    dplyr::select(
      dplyr::any_of("level"), dplyr::all_of(by), "window", "from", "to",
      "window_start", "window_end", "detections", "reported_cases",
      "positive_es", "provinces", "clusters"
    )
}

#' Compare detection counts with the previous period
#'
#' Counts detections in each window with [calc_detection_windows()], then sets
#' each current window against its previous window and names the direction of
#' the change with [detection_direction()].
#'
#' The percentage change is the difference divided by the previous count,
#' times 100, so it is in per cent and can be negative or above 100. When the
#' previous count is zero it is `Inf` if the current count is above zero, and
#' zero otherwise.
#'
#' @inheritParams calc_detection_windows
#' @param pairs A data frame with the columns `current` and `previous`, each
#'   naming a window in `windows`. Default [detection_default_pairs()].
#' @param threshold Size of change, in percent, at or below which a move is
#'   called flat. Default `15`.
#' @param metrics The counts to compare, from `"detections"`,
#'   `"reported_cases"`, `"positive_es"`, `"provinces"` and `"clusters"`.
#'   Default all five.
#'
#' @return A tibble with one row per unit, window pair and metric: with
#'   `level`, the columns `level`, `guid` and `name`; then `current_window`,
#'   `previous_window`, `metric`, `current`, `previous`, `difference`,
#'   `pct_change` and `direction`.
#' @examples
#' detections <- tibble::tibble(
#'   virus_date = as.Date(c("2024-06-01", "2024-01-15", "2023-05-01")),
#'   source = c("AFP", "Environmental", "AFP"),
#'   adm1 = c("Kandahar", "Kandahar", "Helmand"),
#'   virus_cluster = c("C1", "C2", NA)
#' )
#' calc_detection_comparison(detections, as_of = "2024-07-01")
#' @export
calc_detection_comparison <- function(
  detections,
  as_of = Sys.Date(),
  windows = detection_default_windows(),
  pairs = detection_default_pairs(),
  level = NULL,
  threshold = 15,
  metrics = c(
    "detections", "reported_cases", "positive_es", "provinces", "clusters"
  ),
  env_sources = "Environmental",
  cols = list()
) {
  .detw_check_threshold(threshold)
  .detw_check_level(level)
  by <- if (is.null(level)) character(0) else c("guid", "name")
  windows <- .detw_check_windows(windows)
  pairs <- .detw_check_pairs(pairs, windows$window)
  all_metrics <- c(
    "detections", "reported_cases", "positive_es", "provinces", "clusters"
  )
  if (!is.character(metrics) || length(metrics) == 0 ||
        !all(metrics %in% all_metrics)) {
    cli::cli_abort(c(
      "{.arg metrics} must be drawn from {.val {all_metrics}}.",
      "x" = "Not recognised: {.val {setdiff(metrics, all_metrics)}}."
    ))
  }

  long <- calc_detection_windows(
    detections,
    as_of = as_of,
    windows = windows,
    level = level,
    env_sources = env_sources,
    cols = cols
  ) |>
    tidyr::pivot_longer(
      dplyr::all_of(metrics), names_to = "metric", values_to = "value"
    ) |>
    dplyr::select(dplyr::all_of(by), "window", "metric", "value")

  now <- dplyr::rename(long, current_window = "window", current = "value")
  before <- dplyr::rename(long, previous_window = "window", previous = "value")

  pairs |>
    dplyr::mutate(.pair = dplyr::row_number()) |>
    dplyr::rename(current_window = "current", previous_window = "previous") |>
    dplyr::inner_join(
      now, by = "current_window", relationship = "one-to-many"
    ) |>
    dplyr::inner_join(
      before, by = c(by, "previous_window", "metric")
    ) |>
    dplyr::mutate(
      .metric_order = match(.data$metric, metrics),
      difference = .data$current - .data$previous,
      pct_change = .detw_pct_change(.data$current, .data$previous),
      direction = detection_direction(
        .data$current, .data$previous, threshold = threshold
      )
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(rev(by))), .data$.pair, .data$.metric_order
    ) |>
    .detw_add_level(level) |>
    dplyr::select(
      dplyr::any_of("level"), dplyr::all_of(by), "current_window",
      "previous_window", "metric", "current", "previous", "difference",
      "pct_change", "direction"
    )
}

#' Direction of change between two counts
#'
#' Names the direction of a change from a previous count to a current count
#' as `"up"`, `"down"` or `"flat"`. The rules are applied in this order:
#' \enumerate{
#'   \item both counts zero is `"flat"`;
#'   \item a previous count of zero with a current count above zero is
#'     `"up"`, so any detection where there was none is a rise;
#'   \item a change of `threshold` percent or less of the previous count,
#'     either way, is `"flat"`;
#'   \item otherwise the change is `"up"` when the current count is the
#'     larger and `"down"` when it is not.
#' }
#' A missing count in either position gives `NA`. The comparison with the
#' threshold allows a tolerance of `1e-9`, so a change of exactly `threshold`
#' percent is flat despite rounding.
#'
#' @param current,previous Numeric vectors of counts, of the same length or
#'   of length one.
#' @param threshold Size of change, in percent, at or below which a move is
#'   called flat. Default `15`.
#'
#' @return A character vector of `"up"`, `"down"` and `"flat"`.
#' @examples
#' detection_direction(c(0, 1, 23, 24, 2), c(0, 0, 20, 20, 4))
#' @export
detection_direction <- function(current, previous, threshold = 15) {
  if (!is.numeric(current) || !is.numeric(previous)) {
    cli::cli_abort("{.arg current} and {.arg previous} must be numeric.")
  }
  n <- max(length(current), length(previous))
  if (!length(current) %in% c(1L, n) || !length(previous) %in% c(1L, n)) {
    cli::cli_abort(
      "{.arg current} and {.arg previous} must have the same length or
       length one."
    )
  }
  .detw_check_threshold(threshold)

  pct <- (current - previous) / previous * 100

  dplyr::case_when(
    is.na(current) | is.na(previous) ~ NA_character_,
    current == 0 & previous == 0 ~ "flat",
    previous == 0 ~ "up",
    abs(pct) <= threshold + 1e-9 ~ "flat",
    current > previous ~ "up",
    .default = "down"
  )
}

# ---- internal helpers -------------------------------------------------------

#' Default column roles for the detections table
#' @noRd
.detw_default_cols <- function() {
  base <- .polio_default_cols()$virus
  list(
    epid = "epid",
    vtype = "vtype",
    country = "country_iso3code",
    source = "source",
    virus_date = base$virus_date,
    cluster = "virus_cluster",
    adm0_guid = base$adm0_guid,
    adm0 = base$adm0,
    adm1_guid = base$adm1_guid,
    adm1 = base$adm1,
    adm2_guid = base$adm2_guid,
    adm2 = base$adm2
  )
}

#' Merge `cols$detections` onto the defaults, rejecting unknown roles
#' @noRd
.detw_merge_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort("{.arg cols} must be a named list.")
  }
  if (length(cols) > 0 && (is.null(names(cols)) || any(names(cols) == ""))) {
    cli::cli_abort("Every entry in {.arg cols} must be named.")
  }
  sources <- setdiff(names(cols), "detections")
  if (length(sources) > 0) {
    cli::cli_abort(c(
      "Unknown source{?s} in {.arg cols}: {.val {sources}}.",
      "i" = "Use {.val detections}."
    ))
  }
  defaults <- .detw_default_cols()
  roles <- as.list(cols$detections %||% list())
  if (length(roles) > 0 &&
        (is.null(names(roles)) || any(names(roles) == ""))) {
    cli::cli_abort("Every entry in {.arg cols$detections} must be named.")
  }
  unknown <- setdiff(names(roles), names(defaults))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg cols$detections} names {length(unknown)} unknown role{?s}:
       {.val {unknown}}.",
      "i" = "Known roles: {.val {names(defaults)}}."
    ))
  }
  utils::modifyList(defaults, roles)
}

#' Abort unless `level` is NULL or one of the admin levels
#' @noRd
.detw_check_level <- function(level) {
  ok <- is.null(level) || (is.character(level) && length(level) == 1 &&
                             level %in% c("adm0", "adm1", "adm2"))
  if (!ok) {
    cli::cli_abort(
      "{.arg level} must be {.val NULL} or one of {.val adm0}, {.val adm1}
       or {.val adm2}."
    )
  }
  invisible(level)
}

#' GUID and name roles of a level, or none for NULL
#' @noRd
.detw_level_keys <- function(level) {
  if (is.null(level)) {
    return(character(0))
  }
  c(paste0(level, "_guid"), level)
}

#' Add the level column when counting by an admin level
#' @noRd
.detw_add_level <- function(df, level) {
  if (is.null(level)) {
    return(df)
  }
  dplyr::mutate(df, level = level)
}

#' Abort unless `x` is a data frame
#' @noRd
.detw_check_df <- function(x, arg) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data.frame or tibble.")
  }
  invisible(x)
}

#' Abort when columns are missing from a data frame
#' @noRd
.detw_require <- function(df, needed, arg) {
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    cli::cli_abort(
      "{cli::qty(miss)}Column{?s} missing from {.arg {arg}}: {.var {miss}}."
    )
  }
  invisible(df)
}

#' Validate and coerce the reporting date
#' @noRd
.detw_check_as_of <- function(as_of) {
  if (missing(as_of) || length(as_of) != 1) {
    cli::cli_abort("{.arg as_of} must be a single date.")
  }
  out <- tryCatch(
    lubridate::as_date(as_of),
    error = function(e) lubridate::NA_Date_,
    warning = function(w) lubridate::NA_Date_
  )
  if (is.na(out)) {
    cli::cli_abort("{.arg as_of} must be a single date.")
  }
  out
}

#' Validate the windows table
#' @noRd
.detw_check_windows <- function(windows) {
  if (!is.data.frame(windows)) {
    cli::cli_abort("{.arg windows} must be a data frame.")
  }
  .detw_require(windows, c("window", "from", "to"), "windows")
  if (nrow(windows) == 0) {
    cli::cli_abort("{.arg windows} has no rows.")
  }
  if (anyNA(windows$window) || anyDuplicated(windows$window) > 0) {
    cli::cli_abort("{.arg windows} must name each window once.")
  }
  whole <- function(x) {
    is.numeric(x) && !anyNA(x) && all(x >= 0) && all(x == round(x))
  }
  if (!whole(windows$from) || !whole(windows$to)) {
    cli::cli_abort(
      "{.field from} and {.field to} in {.arg windows} must be whole numbers
       of days of zero or more."
    )
  }
  if (any(windows$from < windows$to)) {
    cli::cli_abort(
      "Each window in {.arg windows} must have {.field from} at least
       {.field to}."
    )
  }
  windows |>
    dplyr::select("window", "from", "to") |>
    dplyr::mutate(
      window = as.character(.data$window),
      from = as.integer(.data$from),
      to = as.integer(.data$to)
    ) |>
    tibble::as_tibble()
}

#' Validate the window pairs against the window names
#' @noRd
.detw_check_pairs <- function(pairs, window_names) {
  if (!is.data.frame(pairs)) {
    cli::cli_abort("{.arg pairs} must be a data frame.")
  }
  .detw_require(pairs, c("current", "previous"), "pairs")
  if (nrow(pairs) == 0) {
    cli::cli_abort("{.arg pairs} has no rows.")
  }
  named <- unique(c(pairs$current, pairs$previous))
  unknown <- setdiff(named, window_names)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.arg pairs} names window{?s} not in {.arg windows}: {.val {unknown}}."
    )
  }
  dplyr::select(tibble::as_tibble(pairs), "current", "previous")
}

#' Validate the direction threshold
#' @noRd
.detw_check_threshold <- function(threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1 ||
        is.na(threshold) || threshold < 0) {
    cli::cli_abort("{.arg threshold} must be a single number of zero or more.")
  }
  invisible(threshold)
}

#' Percentage change, with Inf for a rise from zero and 0 for zero to zero
#' @noRd
.detw_pct_change <- function(current, previous) {
  dplyr::if_else(
    previous != 0,
    (current - previous) / previous * 100,
    dplyr::if_else(current > 0, Inf, 0)
  )
}

#' Write a virus type without white space and with WILD as WPV
#' @noRd
.detw_normalise_vtype <- function(x) {
  x |>
    as.character() |>
    stringr::str_remove_all("\\s+") |>
    stringr::str_replace("^WILD", "WPV")
}

utils::globalVariables(c(
  ".cluster",
  ".date",
  ".is_env",
  ".province"
))
