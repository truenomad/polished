# =============================================================================
# AFP case quality indicators
#
# Timeliness of notification, investigation and first stool collection; the
# stool specimen pathway and the interval from collection to laboratory
# receipt; and the 60-day follow-up of cases with inadequate stools. One stool
# adequacy classifier serves all three, with the definition chosen by argument.
# Every threshold, window, class list and column name is an argument.
# =============================================================================

utils::globalVariables(c(
  "adequate",
  "adequate_stool_polis",
  "age",
  "apart",
  "both_in_window",
  "collection_month",
  "compatible",
  "date_collected",
  "date_received",
  "days",
  "days_since",
  "doses_calculated",
  "doses_opv_routine",
  "doses_opvsia",
  "doses_reported",
  "due",
  "finding",
  "first_in_window",
  "followed",
  "followup_date",
  "followup_days",
  "followup_findings",
  "inadequate",
  "invest_n",
  "invest_ok",
  "invest_to_stool1",
  "late",
  "met",
  "no_followup",
  "notify_n",
  "notify_ok",
  "notify_to_invest",
  "on_time",
  "one_or_none",
  "onset_date",
  "onset_month",
  "onset_to_notify",
  "overdue",
  "received",
  "review",
  "specimens",
  "step",
  "stool_n",
  "stool_ok",
  "stool1_date",
  "stool2_date",
  "two",
  ".afpq_class",
  ".afpq_d1",
  ".afpq_d2",
  ".afpq_row"
))

# ---- stool adequacy ---------------------------------------------------------

#' Classify stool adequacy for AFP cases
#'
#' Returns one logical value per case: `TRUE` when the stools are adequate,
#' `FALSE` when they are not, and `NA` when the definition cannot assess the
#' case. Three definitions are in use and `definition` selects one.
#'
#' * `"polished"` (the default) is the rule behind the `adq_code` that
#'   [calc_polio_indicators()] uses for `stool_adequacy_cond_pct`. A case is
#'   adequate when stool 1 was collected 0 to 13 days after onset, stool 2 was
#'   collected 1 to 14 days after onset, the two were at least one day apart,
#'   and both were recorded in good condition. A stool outside its window, a
#'   missing stool 1 to stool 2 interval, stools less than a day apart or a
#'   stool in poor condition makes the case inadequate. A case is not assessed
#'   (`NA`) when either condition is missing or unknown, or when the onset date
#'   quality is recorded as anything other than good. `TRUE` here corresponds
#'   to code 1 and `FALSE` to code 0 of that engine.
#' * `"polis_field"` reads the adequacy POLIS writes itself, in the field
#'   `adequate_stool_with_condition`: a case is adequate when the field equals
#'   `adequate_value`, inadequate when it holds any other value, and `NA` when
#'   it is empty. The rise-afg stool collection page and its SPI code used this
#'   definition.
#' * `"followup"` is the rule of the CDC desk review that the rise-afg 60-day
#'   follow-up page used. The timing windows are the same as `"polished"`, and
#'   a stool outside them or a missing stool makes the case inadequate. A
#'   condition counts as good when it matches `good_condition` (ignoring case)
#'   or is blank, so a case is never left unassessed.
#'
#' Timing uses the interval columns [clean_afp()] writes (`onset_to_stool1`,
#' `onset_to_stool2`, `stool1_to_stool2`), in days.
#'
#' @param cases Cleaned AFP cases, one row per case (data.frame or tibble).
#' @param definition One of `"polished"`, `"polis_field"` or `"followup"`.
#' @param cols Named list overriding default column names, in the style of
#'   [calc_polio_indicators()], for example
#'   `list(cases = list(stool1_condition = "s1_cond"))`. The defaults are the
#'   polished defaults plus the module columns listed under Details of
#'   [calc_afp_timeliness()].
#' @param first_stool_days Earliest and latest day after onset at which stool
#'   1 counts (default `c(0, 13)`).
#' @param second_stool_days Earliest and latest day after onset at which stool
#'   2 counts (default `c(1, 14)`).
#' @param min_days_apart Fewest days between stool 1 and stool 2 (default 1).
#' @param good_condition,poor_condition,unknown_condition Condition values for
#'   a stool in good condition, in poor condition and of unknown condition
#'   (defaults `"Good"`, `"Poor"`, `"Unknown"`).
#' @param good_onset_quality Onset date quality value that lets a case be
#'   assessed under `"polished"` (default `"Good"`). A missing onset date
#'   quality column or value does not exclude the case.
#' @param adequate_value Value of the POLIS field that means adequate under
#'   `"polis_field"` (default `"Yes"`).
#'
#' @return A logical vector with one element per row of `cases`.
#' @examples
#' cases <- tibble::tibble(
#'   onset_to_stool1 = c(2, 2, 20),
#'   onset_to_stool2 = c(3, 3, 21),
#'   stool1_to_stool2 = c(1, 1, 1),
#'   stool1condition = c("Good", NA, "Good"),
#'   stool2condition = c("Good", "Good", "Good"),
#'   adequate_stool_with_condition = c("Yes", "Yes", "No")
#' )
#' afp_stool_adequacy(cases)
#' afp_stool_adequacy(cases, definition = "followup")
#' afp_stool_adequacy(cases, definition = "polis_field")
#' @export
afp_stool_adequacy <- function(
  cases,
  definition = c("polished", "polis_field", "followup"),
  cols = list(),
  first_stool_days = c(0, 13),
  second_stool_days = c(1, 14),
  min_days_apart = 1,
  good_condition = "Good",
  poor_condition = "Poor",
  unknown_condition = "Unknown",
  good_onset_quality = "Good",
  adequate_value = "Yes"
) {
  .afpq_check_df(cases, "cases")
  definition <- .afpq_match(
    definition,
    c("polished", "polis_field", "followup"),
    "definition"
  )
  .afpq_check_range(first_stool_days, "first_stool_days")
  .afpq_check_range(second_stool_days, "second_stool_days")
  .afpq_check_number(min_days_apart, "min_days_apart")
  .afpq_check_string(good_condition, "good_condition")
  .afpq_check_string(poor_condition, "poor_condition")
  .afpq_check_string(unknown_condition, "unknown_condition")
  .afpq_check_string(good_onset_quality, "good_onset_quality")
  .afpq_check_string(adequate_value, "adequate_value")
  m <- .afpq_cols(cols)$cases

  if (definition == "polis_field") {
    d <- .afpq_take(cases, m, "adequate_stool_polis")
    return(as.character(d$adequate_stool_polis) == adequate_value)
  }

  d <- .afpq_take(
    cases,
    m,
    c(
      "onset_to_stool1",
      "onset_to_stool2",
      "stool1_to_stool2",
      "stool1_condition",
      "stool2_condition"
    ),
    optional = "onset_date_quality"
  )
  ots1 <- .afpq_num(d$onset_to_stool1)
  ots2 <- .afpq_num(d$onset_to_stool2)
  gap <- .afpq_num(d$stool1_to_stool2)
  c1 <- as.character(d$stool1_condition)
  c2 <- as.character(d$stool2_condition)
  in_first <- !is.na(ots1) &
    ots1 >= first_stool_days[1] &
    ots1 <= first_stool_days[2]
  in_second <- !is.na(ots2) &
    ots2 >= second_stool_days[1] &
    ots2 <= second_stool_days[2]
  apart <- !is.na(gap) & gap >= min_days_apart

  if (definition == "followup") {
    good <- \(x) .afpq_blank(x) | toupper(trimws(x)) == toupper(good_condition)
    return(in_first & in_second & apart & good(c1) & good(c2))
  }

  quality <- as.character(d$onset_date_quality)
  out_first <- !is.na(ots1) &
    (ots1 > first_stool_days[2] | ots1 < first_stool_days[1])
  out_second <- !is.na(ots2) &
    (ots2 > second_stool_days[2] | ots2 < second_stool_days[1])
  poor <- (!is.na(c1) & c1 == poor_condition) |
    (!is.na(c2) & c2 == poor_condition)
  both_good <- !is.na(c1) &
    c1 == good_condition &
    !is.na(c2) &
    c2 == good_condition
  dplyr::case_when(
    !is.na(quality) & quality != good_onset_quality ~ NA,
    out_first | is.na(gap) | out_second | !apart | poor ~ FALSE,
    in_first & in_second & apart & both_good ~ TRUE,
    .default = NA
  )
}

# ---- timeliness -------------------------------------------------------------

#' Calculate AFP timeliness by step
#'
#' For each administrative unit, the share of AFP cases that met each of
#' three timeliness standards: onset to notification, notification to
#' investigation, and investigation to first stool collection. Each step has
#' its own denominator, since a case can carry one date and not another.
#'
#' * Numerator: cases whose interval is at or under the step's threshold
#'   (`notify_days`, `invest_days`, `stool_days`; defaults 7, 2 and 3 days).
#' * Denominator: cases where that interval is not missing.
#' * The percentage is `NA` when the denominator is zero.
#'
#' The cases counted are those with onset between `from` and `to`. A case
#' without an onset date is left out whenever either bound is given.
#'
#' This differs from `inv_timeliness_pct` in [calc_polio_indicators()], whose
#' denominator is every AFP case (so a missing interval counts as late) and
#' whose numerator also requires the interval to be zero or more. Here a
#' missing interval leaves the case out of that step, and a negative interval
#' counts as on time; [clean_afp()] already sets intervals outside 0 to 365
#' days to `NA`, except investigation to first stool, which may be negative.
#'
#' Columns used beyond the defaults of [calc_polio_indicators()]: in `cases`,
#' `epid`, `onset_to_notify`, `invest_to_stool1`, `stool1collection_date`,
#' `adequate_stool_polis` (`adequate_stool_with_condition`),
#' `followup_findings`, `doses_reported` (`total_number_of_doses`) and
#' `doses_calculated` (`calc_doses_risi`); in `lab`, `epid`, `date_collected`
#' (`date_stool_collected`) and `date_received` (`date_stool_received_in_lab`).
#'
#' @inheritParams afp_stool_adequacy
#' @param level Administrative level to group by: `"adm0"` (country, the
#'   default), `"adm1"` (province) or `"adm2"` (district). Cases with no
#'   identifier at that level are left out.
#' @param admin_units Optional data frame listing every unit to report, with
#'   the level's GUID column (for example `adm2_guid`) and optionally its name
#'   column (for example `adm2`). A unit with no cases is kept with zero counts
#'   and `NA` percentages. Without it, only units with cases are reported.
#' @param from,to First and last onset date of the reporting window, or `NULL`
#'   for no bound.
#' @param notify_days Most days from onset to notification that count as on
#'   time (default 7).
#' @param invest_days Most days from notification to investigation that count
#'   as on time (default 2).
#' @param stool_days Most days from investigation to first stool collection
#'   that count as on time (default 3).
#'
#' @return A tibble with one row per unit: `level`, `guid`, `name`, `cases`,
#'   then for each step (`notify`, `invest`, `stool`) the denominator `_n`, the
#'   numerator `_ok` and the percentage `_pct`.
#' @examples
#' cases <- tibble::tibble(
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date("2025-03-01") + 0:3,
#'   onset_to_notify = c(3, 9, NA, 7),
#'   notify_to_invest = c(1, 2, 3, NA),
#'   invest_to_stool1 = c(0, 4, 1, 2)
#' )
#' calc_afp_timeliness(cases)
#' @export
calc_afp_timeliness <- function(
  cases,
  level = "adm0",
  admin_units = NULL,
  from = NULL,
  to = NULL,
  notify_days = 7,
  invest_days = 2,
  stool_days = 3,
  cols = list()
) {
  .afpq_check_df(cases, "cases")
  .afpq_check_level(level)
  .afpq_check_number(notify_days, "notify_days")
  .afpq_check_number(invest_days, "invest_days")
  .afpq_check_number(stool_days, "stool_days")
  from <- .afpq_check_date(from, "from")
  to <- .afpq_check_date(to, "to")
  m <- .afpq_cols(cols)

  counts <- cases |>
    .afpq_take(
      m$cases,
      c(
        "onset_date",
        "onset_to_notify",
        "notify_to_invest",
        "invest_to_stool1"
      ),
      level = level
    ) |>
    .afpq_window(from, to) |>
    dplyr::mutate(
      dplyr::across(
        c(onset_to_notify, notify_to_invest, invest_to_stool1),
        .afpq_num
      )
    ) |>
    dplyr::summarise(
      cases = dplyr::n(),
      notify_n = sum(!is.na(onset_to_notify)),
      notify_ok = sum(onset_to_notify <= notify_days, na.rm = TRUE),
      invest_n = sum(!is.na(notify_to_invest)),
      invest_ok = sum(notify_to_invest <= invest_days, na.rm = TRUE),
      stool_n = sum(!is.na(invest_to_stool1)),
      stool_ok = sum(invest_to_stool1 <= stool_days, na.rm = TRUE),
      .by = c(guid, name)
    )

  counts |>
    .afpq_spine(admin_units, m$cases, level) |>
    dplyr::mutate(
      dplyr::across(
        c(cases, notify_n, notify_ok, invest_n, invest_ok, stool_n, stool_ok),
        \(x) tidyr::replace_na(x, 0L)
      ),
      notify_pct = .afpq_pct(notify_ok, notify_n),
      invest_pct = .afpq_pct(invest_ok, invest_n),
      stool_pct = .afpq_pct(stool_ok, stool_n),
      level = level,
      .before = 1
    ) |>
    dplyr::relocate(level, guid, name)
}

# ---- specimen pathway -------------------------------------------------------

#' Flag the stool collection components of each AFP case
#'
#' Adds one logical column per component of stool collection, computed from
#' the collection dates rather than the cleaned intervals, so a stool
#' collected long after onset is still seen as late.
#'
#' * `two`: both collection dates are recorded.
#' * `first_in_window`: stool 1 was collected 0 to `collection_days` days
#'   after onset.
#' * `both_in_window`: both stools were collected 0 to `collection_days` days
#'   after onset.
#' * `apart`: both stools are recorded at least `min_days_apart` days apart.
#' * `adequate`: the result of [afp_stool_adequacy()] under `adequacy`.
#' * `late`: either stool was collected more than `collection_days` days
#'   after onset.
#' * `one_or_none`: fewer than two collection dates are recorded.
#'
#' @inheritParams calc_afp_timeliness
#' @param collection_days Latest day after onset at which a stool counts as
#'   collected in time (default 14).
#' @param min_days_apart Fewest days between the two collection dates for
#'   `apart` (default 1).
#' @param adequacy Stool adequacy definition passed to [afp_stool_adequacy()]
#'   (default `"polis_field"`, which the rise-afg stool collection page used).
#' @param adequacy_args Named list of further arguments for
#'   [afp_stool_adequacy()], for example `list(adequate_value = "YES")`.
#'
#' @return `cases` as a tibble with the seven columns above added; existing
#'   columns of the same names are replaced.
#' @examples
#' cases <- tibble::tibble(
#'   paralysis_onset_date = as.Date("2025-03-01"),
#'   stool1collection_date = as.Date("2025-03-03"),
#'   stool2collection_date = as.Date(c("2025-03-04", NA)),
#'   adequate_stool_with_condition = c("Yes", "No")
#' )
#' afp_stool_components(cases)
#' @export
afp_stool_components <- function(
  cases,
  collection_days = 14,
  min_days_apart = 1,
  adequacy = "polis_field",
  adequacy_args = list(),
  cols = list()
) {
  .afpq_check_df(cases, "cases")
  .afpq_check_number(collection_days, "collection_days")
  .afpq_check_number(min_days_apart, "min_days_apart")
  m <- .afpq_cols(cols)$cases
  adequate <- .afpq_adequacy(cases, adequacy, adequacy_args, cols)
  d <- .afpq_take(
    cases,
    m,
    c("onset_date", "stool1collection_date", "stool2collection_date")
  )
  onset <- .afpq_date(d$onset_date)
  s1 <- .afpq_date(d$stool1collection_date)
  s2 <- .afpq_date(d$stool2collection_date)
  d1 <- as.integer(s1 - onset)
  d2 <- as.integer(s2 - onset)
  two <- !is.na(s1) & !is.na(s2)
  first_in_window <- !is.na(d1) & d1 >= 0 & d1 <= collection_days

  tibble::as_tibble(cases) |>
    dplyr::mutate(
      two = two,
      first_in_window = first_in_window,
      both_in_window = two &
        first_in_window &
        d2 >= 0 &
        d2 <= collection_days,
      apart = two & as.integer(s2 - s1) >= min_days_apart,
      adequate = adequate,
      late = (!is.na(d1) & d1 > collection_days) |
        (!is.na(d2) & d2 > collection_days),
      one_or_none = !two
    )
}

#' Calculate the stool specimen pathway for AFP cases
#'
#' The share of AFP cases meeting each stool collection component, in the
#' order the adequacy definition builds it, ending on adequacy itself. The
#' components are those of [afp_stool_components()].
#'
#' * Steps: `first_in_window`, `both_in_window`, `apart` and `adequate`.
#' * Numerator (`met`): cases meeting the step.
#' * Denominator (`cases`): every case in the unit and window.
#' * Threshold: stools count within `collection_days` (default 14) of onset.
#'
#' A case that `adequacy` cannot assess counts as not adequate here. The
#' default adequacy is the POLIS field, as on the rise-afg page; this differs
#' from `stool_adequacy_cond_pct` in [calc_polio_indicators()], which applies
#' the `"polished"` rule and leaves unassessed cases out of its denominator.
#'
#' @inheritParams afp_stool_components
#' @inheritParams calc_afp_timeliness
#' @return A tibble with one row per unit and step: `level`, `guid`, `name`,
#'   `step`, `met`, `cases` and `pct`.
#' @examples
#' cases <- tibble::tibble(
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date("2025-03-01"),
#'   stool1collection_date = as.Date("2025-03-03"),
#'   stool2collection_date = as.Date(c("2025-03-04", NA)),
#'   adequate_stool_with_condition = c("Yes", "No")
#' )
#' calc_specimen_pathway(cases)
#' @export
calc_specimen_pathway <- function(
  cases,
  level = "adm0",
  from = NULL,
  to = NULL,
  collection_days = 14,
  min_days_apart = 1,
  adequacy = "polis_field",
  adequacy_args = list(),
  cols = list()
) {
  .afpq_components_by_unit(
    cases,
    level,
    from,
    to,
    collection_days,
    min_days_apart,
    adequacy,
    adequacy_args,
    cols
  ) |>
    dplyr::summarise(
      first_in_window = sum(first_in_window),
      both_in_window = sum(both_in_window),
      apart = sum(apart),
      adequate = sum(adequate %in% TRUE),
      cases = dplyr::n(),
      .by = c(guid, name)
    ) |>
    tidyr::pivot_longer(
      c(first_in_window, both_in_window, apart, adequate),
      names_to = "step",
      values_to = "met"
    ) |>
    dplyr::mutate(pct = .afpq_pct(met, cases), level = level) |>
    dplyr::select(level, guid, name, step, met, cases, pct)
}

#' Calculate the reasons AFP cases had inadequate stools
#'
#' Among cases whose stools were inadequate, the share with each of two
#' reasons. A case can carry both, so the shares need not sum to 100.
#'
#' * Reasons: `late` (a stool collected more than `collection_days` days after
#'   onset) and `one_or_none` (fewer than two stools recorded).
#' * Numerator (`n`): inadequate cases with the reason.
#' * Denominator (`inadequate`): cases that `adequacy` classes as inadequate.
#'   Cases it cannot assess are left out.
#' * The percentage is `NA` when there are no inadequate cases.
#'
#' @inheritParams afp_stool_components
#' @inheritParams calc_afp_timeliness
#' @return A tibble with one row per unit and reason: `level`, `guid`, `name`,
#'   `reason`, `n`, `inadequate` and `pct`. Units with no inadequate case are
#'   left out.
#' @examples
#' cases <- tibble::tibble(
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date("2025-03-01"),
#'   stool1collection_date = as.Date("2025-03-03"),
#'   stool2collection_date = as.Date(c("2025-03-04", NA)),
#'   adequate_stool_with_condition = c("Yes", "No")
#' )
#' calc_specimen_reasons(cases)
#' @export
calc_specimen_reasons <- function(
  cases,
  level = "adm0",
  from = NULL,
  to = NULL,
  collection_days = 14,
  min_days_apart = 1,
  adequacy = "polis_field",
  adequacy_args = list(),
  cols = list()
) {
  .afpq_components_by_unit(
    cases,
    level,
    from,
    to,
    collection_days,
    min_days_apart,
    adequacy,
    adequacy_args,
    cols
  ) |>
    dplyr::filter(adequate %in% FALSE) |>
    dplyr::summarise(
      late = sum(late),
      one_or_none = sum(one_or_none),
      inadequate = dplyr::n(),
      .by = c(guid, name)
    ) |>
    tidyr::pivot_longer(
      c(late, one_or_none),
      names_to = "reason",
      values_to = "n"
    ) |>
    dplyr::mutate(pct = .afpq_pct(n, inadequate), level = level) |>
    dplyr::select(level, guid, name, reason, n, inadequate, pct)
}

# ---- collection to laboratory receipt ---------------------------------------

#' Link stool specimens to AFP cases with the days to laboratory receipt
#'
#' One row per stool specimen from the supplied AFP cases, with the days from
#' collection to receipt in the laboratory. A specimen without a collection
#' date is dropped. A receipt dated before collection is treated as an entry
#' error and its interval set to `NA`. Each specimen takes the administrative
#' unit of the first case row with its EPID.
#'
#' @inheritParams calc_afp_timeliness
#' @param specimens Cleaned human specimen table from [clean_human_spec()],
#'   one row per specimen.
#' @param cases AFP cases whose specimens are wanted. Only specimens whose
#'   EPID appears here are kept, so filter `cases` to the window first or use
#'   `from` and `to`.
#'
#' @return A tibble with `epid`, `collected`, `received`, `days`,
#'   `collection_month`, `level`, `guid` and `name`.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("E1", "E2"),
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date("2025-03-01")
#' )
#' specimens <- tibble::tibble(
#'   epid = c("E1", "E1", "E2"),
#'   date_stool_collected = as.Date(c("2025-03-02", "2025-03-03", NA)),
#'   date_stool_received_in_lab = as.Date(c("2025-03-09", NA, NA))
#' )
#' afp_specimen_receipt(specimens, cases)
#' @export
afp_specimen_receipt <- function(
  specimens,
  cases,
  level = "adm0",
  from = NULL,
  to = NULL,
  cols = list()
) {
  .afpq_check_df(specimens, "specimens")
  .afpq_check_df(cases, "cases")
  .afpq_check_level(level)
  from <- .afpq_check_date(from, "from")
  to <- .afpq_check_date(to, "to")
  m <- .afpq_cols(cols)

  units <- cases |>
    .afpq_take(m$cases, c("epid", "onset_date"), level = level, drop = FALSE) |>
    .afpq_window(from, to) |>
    dplyr::distinct(epid, guid, name) |>
    dplyr::slice_head(n = 1, by = epid)

  specimens |>
    .afpq_take(m$lab, c("epid", "date_collected", "date_received")) |>
    dplyr::filter(epid %in% units$epid) |>
    dplyr::transmute(
      epid,
      collected = .afpq_date(date_collected),
      received = .afpq_date(date_received),
      days = as.integer(received - collected)
    ) |>
    dplyr::filter(!is.na(collected)) |>
    dplyr::left_join(units, by = "epid") |>
    dplyr::mutate(
      collection_month = lubridate::floor_date(collected, "month"),
      days = dplyr::if_else(!is.na(days) & days < 0, NA_integer_, days),
      level = level
    )
}

#' Calculate days from stool collection to laboratory receipt by month
#'
#' For each month of collection, the number of specimens, the number received
#' and percentiles of the days from collection to receipt among those
#' received. There is no threshold. Specimens not yet received are counted in
#' `awaiting`, since their intervals can only lengthen the month once they
#' arrive.
#'
#' A month is provisional while the overall `tail_prob` percentile interval
#' (default the 90th, across all months and units) has not yet passed since
#' its last day, as of `as_of`.
#'
#' @inheritParams afp_specimen_receipt
#' @param as_of Date against which months are judged provisional (default
#'   today).
#' @param probs Named percentiles to report (default
#'   `c(p50 = 0.5, p75 = 0.75, p90 = 0.9)`), as quantile type 7.
#' @param tail_prob Percentile whose interval decides when a month is no
#'   longer provisional (default 0.9).
#'
#' @return A tibble with one row per unit and collection month: `level`,
#'   `guid`, `name`, `collection_month`, `specimens`, `received`, one column
#'   per element of `probs`, `awaiting` and `provisional`.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("E1", "E2"),
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date("2025-03-01")
#' )
#' specimens <- tibble::tibble(
#'   epid = c("E1", "E1", "E2"),
#'   date_stool_collected = as.Date(
#'     c("2025-03-02", "2025-03-03", "2025-03-04")
#'   ),
#'   date_stool_received_in_lab = as.Date(c("2025-03-09", NA, "2025-03-06"))
#' )
#' calc_specimen_receipt_monthly(specimens, cases, as_of = "2025-06-30")
#' @export
calc_specimen_receipt_monthly <- function(
  specimens,
  cases,
  level = "adm0",
  from = NULL,
  to = NULL,
  as_of = Sys.Date(),
  probs = c(p50 = 0.5, p75 = 0.75, p90 = 0.9),
  tail_prob = 0.9,
  cols = list()
) {
  .afpq_check_probs(probs)
  .afpq_check_number(tail_prob, "tail_prob")
  as_of <- .afpq_check_date(as_of, "as_of", allow_null = FALSE)
  spec <- afp_specimen_receipt(specimens, cases, level, from, to, cols)
  tail_days <- stats::quantile(
    spec$days,
    tail_prob,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  spec |>
    dplyr::summarise(
      specimens = dplyr::n(),
      received = sum(!is.na(days)),
      .afpq_quantiles(days, probs),
      .by = c(level, guid, name, collection_month)
    ) |>
    dplyr::mutate(
      awaiting = specimens - received,
      provisional = lubridate::ceiling_date(collection_month, "month") -
        1 +
        tail_days >
        as_of
    ) |>
    dplyr::arrange(name, collection_month)
}

#' Calculate days from stool collection to laboratory receipt by area
#'
#' For each administrative unit, the number of specimens received and
#' percentiles of the days from collection to receipt. Units with fewer than
#' `min_received` received specimens are left out. Rows are sorted from the
#' longest interval at the highest percentile in `probs`, then at the lowest.
#'
#' @inheritParams calc_specimen_receipt_monthly
#' @param level Administrative level to group by (default `"adm1"`, province).
#' @param min_received Fewest received specimens a unit needs to be listed
#'   (default 20).
#'
#' @return A tibble with one row per unit: `level`, `guid`, `name`,
#'   `received` and one column per element of `probs`.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("E1", "E2"),
#'   adm1_guid = c("P1", "P2"),
#'   adm1 = c("Prov A", "Prov B"),
#'   paralysis_onset_date = as.Date("2025-03-01")
#' )
#' specimens <- tibble::tibble(
#'   epid = c("E1", "E1", "E2"),
#'   date_stool_collected = as.Date(
#'     c("2025-03-02", "2025-03-03", "2025-03-04")
#'   ),
#'   date_stool_received_in_lab = as.Date(c("2025-03-09", NA, "2025-03-06"))
#' )
#' calc_specimen_receipt_by_area(specimens, cases, min_received = 1)
#' @export
calc_specimen_receipt_by_area <- function(
  specimens,
  cases,
  level = "adm1",
  from = NULL,
  to = NULL,
  probs = c(p50 = 0.5, p75 = 0.75, p90 = 0.9),
  min_received = 20,
  cols = list()
) {
  .afpq_check_probs(probs)
  .afpq_check_number(min_received, "min_received")
  hi <- names(probs)[which.max(probs)]
  lo <- names(probs)[which.min(probs)]

  afp_specimen_receipt(specimens, cases, level, from, to, cols) |>
    dplyr::filter(!is.na(days)) |>
    dplyr::summarise(
      received = dplyr::n(),
      .afpq_quantiles(days, probs),
      .by = c(level, guid, name)
    ) |>
    dplyr::filter(received >= min_received) |>
    dplyr::arrange(dplyr::desc(.data[[hi]]), dplyr::desc(.data[[lo]]))
}

# ---- 60-day follow-up -------------------------------------------------------

#' Flag where each AFP case stands on 60-day follow-up
#'
#' Adds the stool adequacy, the follow-up status and the review screen of the
#' CDC desk review to each case. Every day count runs to `as_of`.
#'
#' * `adequate`: [afp_stool_adequacy()] under `adequacy` (default
#'   `"followup"`, as on the rise-afg follow-up page).
#' * `days_since`: days from onset to `as_of`.
#' * `followup_days`: days from onset to the follow-up date.
#' * `finding`: the follow-up finding, `NA` when blank.
#' * `due`: inadequate, and at least `due_days` (default 60) days since onset.
#' * `followed`: a follow-up date or a finding is recorded.
#' * `on_time`: followed up `on_time_days[1]` to `on_time_days[2]` days after
#'   onset (default 60 to 90).
#' * `no_followup`: due and not followed up.
#' * `overdue`: no follow-up and more than `on_time_days[2]` days since onset.
#' * `compatible`: classified in `compatible_classes`.
#' * `dose_reason`: why the dose history keeps the case under review, or `NA`.
#'   No OPV dose recorded keeps it; where the routine and campaign OPV counts
#'   are both missing, fewer than `review_min_doses` doses by the reported
#'   total, or failing that by the calculated total, keeps it.
#' * `review`: due, not compatible, classified in `review_classes` or not yet
#'   classified, with a finding in `review_findings` or overdue, with a
#'   `dose_reason`, and younger than `review_max_age_months` (or of unknown
#'   age).
#'
#' A case `adequacy` cannot assess is never due.
#'
#' @inheritParams afp_stool_components
#' @param as_of Date the day counts run to (default today).
#' @param due_days Days after onset from which follow-up of an inadequate case
#'   is due (default 60).
#' @param on_time_days First and last day after onset at which a follow-up is
#'   on time (default `c(60, 90)`).
#' @param adequacy Stool adequacy definition passed to [afp_stool_adequacy()]
#'   (default `"followup"`).
#' @param compatible_classes Classifications counted as compatible, matched
#'   ignoring case (default `"COMPATIBLE"`).
#' @param review_classes Classifications the review screen keeps, matched
#'   ignoring case (default `c("NPAFP", "PENDING", "LAB PENDING")`). A blank
#'   classification is always kept.
#' @param review_findings Follow-up findings that keep a case under review
#'   (default residual weakness or paralysis, died before follow-up, and lost
#'   to follow-up, in POLIS wording).
#' @param review_max_age_months Age in months at and above which the screen
#'   drops a case (default 60).
#' @param review_min_doses Fewest doses that count as vaccinated when routine
#'   and campaign OPV counts are missing (default 3).
#' @param dose_unknown Dose value that means unknown (default 99).
#' @param dose_reason_labels Named character vector of the `dose_reason`
#'   texts, with elements `no_opv`, `no_routine`, `no_sia`, `few_doses` and
#'   `no_history`.
#'
#' @return `cases` as a tibble with `adequate`, `onset_month`, `days_since`,
#'   `followup_days`, `finding`, `due`, `followed`, `on_time`, `no_followup`,
#'   `overdue`, `compatible`, `dose_reason` and `review` added.
#' @examples
#' cases <- tibble::tibble(
#'   paralysis_onset_date = as.Date("2025-01-10"),
#'   onset_to_stool1 = 20,
#'   onset_to_stool2 = 21,
#'   stool1_to_stool2 = 1,
#'   stool1condition = "Good",
#'   stool2condition = "Good",
#'   classification_all = "NPAFP",
#'   followup_date = as.Date(NA),
#'   followup_findings = NA_character_,
#'   age_months = 24,
#'   doses_opv_routine = 0,
#'   doses_opvsia = 0,
#'   total_number_of_doses = 0,
#'   calc_doses_risi = 0
#' )
#' afp_followup_status(cases, as_of = "2025-06-30")
#' @export
afp_followup_status <- function(
  cases,
  as_of = Sys.Date(),
  due_days = 60,
  on_time_days = c(60, 90),
  adequacy = "followup",
  adequacy_args = list(),
  compatible_classes = "COMPATIBLE",
  review_classes = c("NPAFP", "PENDING", "LAB PENDING"),
  review_findings = c(
    "Residual weakness/paralysis",
    "Died before follow-up",
    "Lost to follow-up"
  ),
  review_max_age_months = 60,
  review_min_doses = 3,
  dose_unknown = 99,
  dose_reason_labels = c(
    no_opv = "0 OPV doses",
    no_routine = "0 routine OPV doses, SIA doses missing",
    no_sia = "0 SIA OPV doses, routine doses missing",
    few_doses = paste0("Fewer than ", review_min_doses, " doses"),
    no_history = "Dose history missing"
  ),
  cols = list()
) {
  .afpq_check_df(cases, "cases")
  as_of <- .afpq_check_date(as_of, "as_of", allow_null = FALSE)
  .afpq_check_number(due_days, "due_days")
  .afpq_check_range(on_time_days, "on_time_days")
  .afpq_check_number(review_max_age_months, "review_max_age_months")
  .afpq_check_number(review_min_doses, "review_min_doses")
  .afpq_check_number(dose_unknown, "dose_unknown")
  .afpq_check_chr(compatible_classes, "compatible_classes")
  .afpq_check_chr(review_classes, "review_classes")
  .afpq_check_chr(review_findings, "review_findings")
  label_names <- c("no_opv", "no_routine", "no_sia", "few_doses", "no_history")
  if (!is.character(dose_reason_labels) ||
        !all(label_names %in% names(dose_reason_labels))) {
    cli::cli_abort(
      "{.arg dose_reason_labels} must be a character vector naming \\
      {.val {label_names}}."
    )
  }
  m <- .afpq_cols(cols)$cases
  adequate <- .afpq_adequacy(cases, adequacy, adequacy_args, cols)
  d <- .afpq_take(
    cases,
    m,
    c("onset_date", "followup_date", "followup_findings", "class"),
    optional = c(
      "age",
      "doses_opv_routine",
      "doses_opvsia",
      "doses_reported",
      "doses_calculated"
    )
  )
  dose <- \(x) {
    x <- .afpq_num(x)
    dplyr::if_else(x == dose_unknown, NA_real_, x)
  }
  onset <- .afpq_date(d$onset_date)
  days_since <- as.integer(as_of - onset)
  followup_days <- as.integer(.afpq_date(d$followup_date) - onset)
  finding <- dplyr::if_else(
    .afpq_blank(d$followup_findings),
    NA_character_,
    trimws(as.character(d$followup_findings))
  )
  class_raw <- as.character(d$class)
  class_upper <- toupper(trimws(class_raw))
  dose_reason <- .afpq_dose_reason(
    dose(d$doses_opv_routine),
    dose(d$doses_opvsia),
    dose(d$doses_reported),
    dose(d$doses_calculated),
    review_min_doses,
    dose_reason_labels
  )
  age <- .afpq_num(d$age)

  due <- adequate %in% FALSE & !is.na(days_since) & days_since >= due_days
  followed <- !is.na(.afpq_date(d$followup_date)) | !is.na(finding)
  on_time <- followed &
    !is.na(followup_days) &
    followup_days >= on_time_days[1] &
    followup_days <= on_time_days[2]
  no_followup <- due & !followed
  overdue <- no_followup & days_since > on_time_days[2]
  compatible <- class_upper %in% toupper(compatible_classes)
  review <- due &
    !compatible &
    (class_upper %in% toupper(review_classes) | .afpq_blank(class_raw)) &
    (finding %in% review_findings | overdue) &
    !is.na(dose_reason) &
    (is.na(age) | age < review_max_age_months)

  tibble::as_tibble(cases) |>
    dplyr::mutate(
      adequate = adequate,
      onset_month = lubridate::floor_date(onset, "month"),
      days_since = days_since,
      followup_days = followup_days,
      finding = finding,
      due = due,
      followed = followed,
      on_time = on_time,
      no_followup = no_followup,
      overdue = overdue,
      compatible = compatible,
      dose_reason = dose_reason,
      review = review
    )
}

#' Calculate 60-day follow-up of inadequate AFP cases by month of onset
#'
#' For each month of onset in the window, and each unit at `level`, counts of
#' cases with inadequate stools that are due for follow-up and where each
#' stands. The window is the `window_months` months of onset ending with the
#' last month holding a case already due, that is the month of
#' `as_of - due_days`, so recent months with nothing yet due are not listed.
#' The case flags are those of [afp_followup_status()].
#'
#' * `records`: cases with onset in the month.
#' * `inadequate`: cases classed inadequate by `adequacy`.
#' * `due`: inadequate cases at least `due_days` (default 60) days past onset.
#' * `completed`: due cases followed up. Completion is `completed / due`.
#' * `mature`: due cases more than `on_time_days[2]` (default 90) days past
#'   onset, whose follow-up window has closed; `mature_completed` is those
#'   followed up. Completion read as `mature_completed / mature` is not pulled
#'   down by recent months.
#' * `on_time`: due cases followed up `on_time_days[1]` to `on_time_days[2]`
#'   days after onset. Timeliness is `on_time / due`.
#' * `no_followup` and `overdue`: due cases not followed up, and those of them
#'   past `on_time_days[2]` days.
#' * `compatible` and `review`: compatible cases, and cases the review screen
#'   keeps.
#' * `partial`: the month's last day has not yet reached `due_days`.
#'
#' This differs from `fup_insa_cases_pct` in [calc_polio_indicators()], which
#' takes every case with code 0 under the `"polished"` adequacy rule as its
#' denominator whether or not follow-up is yet due, and counts a case as
#' followed up when a follow-up interval is recorded, at any day.
#'
#' @inheritParams afp_followup_status
#' @inheritParams calc_afp_timeliness
#' @param window_months Months of onset to report (default 6).
#'
#' @return A tibble with one row per unit and month of onset: `level`, `guid`,
#'   `name`, `onset_month`, the counts above and `partial`. Every month of the
#'   window appears for every unit with a case, with zero counts where the
#'   month has none.
#' @examples
#' cases <- tibble::tibble(
#'   adm0_guid = "G0",
#'   adm0 = "Country",
#'   paralysis_onset_date = as.Date(c("2025-03-10", "2025-04-02")),
#'   onset_to_stool1 = c(20, 2),
#'   onset_to_stool2 = c(21, 3),
#'   stool1_to_stool2 = 1,
#'   stool1condition = "Good",
#'   stool2condition = "Good",
#'   classification_all = "NPAFP",
#'   followup_date = as.Date(c("2025-05-20", NA)),
#'   followup_findings = NA_character_
#' )
#' calc_afp_followup(cases, as_of = "2025-07-15", window_months = 3)
#' @export
calc_afp_followup <- function(
  cases,
  as_of = Sys.Date(),
  window_months = 6,
  level = "adm0",
  due_days = 60,
  on_time_days = c(60, 90),
  adequacy = "followup",
  adequacy_args = list(),
  compatible_classes = "COMPATIBLE",
  review_classes = c("NPAFP", "PENDING", "LAB PENDING"),
  review_findings = c(
    "Residual weakness/paralysis",
    "Died before follow-up",
    "Lost to follow-up"
  ),
  review_max_age_months = 60,
  review_min_doses = 3,
  dose_unknown = 99,
  cols = list()
) {
  .afpq_check_level(level)
  .afpq_check_number(window_months, "window_months")
  if (window_months < 1) {
    cli::cli_abort("{.arg window_months} must be at least 1.")
  }
  as_of <- .afpq_check_date(as_of, "as_of", allow_null = FALSE)
  m <- .afpq_cols(cols)$cases

  status <- afp_followup_status(
    cases,
    as_of = as_of,
    due_days = due_days,
    on_time_days = on_time_days,
    adequacy = adequacy,
    adequacy_args = adequacy_args,
    compatible_classes = compatible_classes,
    review_classes = review_classes,
    review_findings = review_findings,
    review_max_age_months = review_max_age_months,
    review_min_doses = review_min_doses,
    dose_unknown = dose_unknown,
    cols = cols
  )
  units <- .afpq_take(cases, m, character(0), level = level, drop = FALSE)
  status <- status |>
    dplyr::select(
      adequate,
      onset_month,
      days_since,
      due,
      followed,
      on_time,
      no_followup,
      overdue,
      compatible,
      review
    ) |>
    dplyr::bind_cols(dplyr::select(units, guid, name)) |>
    dplyr::filter(!is.na(guid))

  last <- lubridate::floor_date(as_of - due_days, "month")
  window <- rev(seq(last, by = "-1 month", length.out = window_months))
  hi <- on_time_days[2]

  counts <- status |>
    dplyr::filter(onset_month %in% window) |>
    dplyr::summarise(
      records = dplyr::n(),
      inadequate = sum(adequate %in% FALSE),
      completed = sum(due & followed),
      mature = sum(due & days_since > hi),
      mature_completed = sum(due & days_since > hi & followed),
      on_time = sum(due & on_time),
      no_followup = sum(no_followup),
      overdue = sum(overdue),
      compatible = sum(compatible),
      review = sum(review),
      due = sum(due),
      .by = c(guid, name, onset_month)
    ) |>
    dplyr::relocate(due, .after = inadequate)

  status |>
    dplyr::distinct(guid, name) |>
    tidyr::expand_grid(onset_month = window) |>
    dplyr::left_join(counts, by = c("guid", "name", "onset_month")) |>
    dplyr::mutate(
      dplyr::across(
        -c(guid, name, onset_month),
        \(x) tidyr::replace_na(x, 0L)
      ),
      partial = lubridate::ceiling_date(onset_month, "month") - 1 >
        as_of - due_days,
      level = level
    ) |>
    dplyr::relocate(level, guid, name) |>
    dplyr::arrange(name, guid, onset_month)
}

# ---- internal helpers -------------------------------------------------------

#' Module column defaults: the polished defaults plus the columns this module
#' needs that `.polio_default_cols()` lacks.
#' @keywords internal
#' @noRd
.afpq_default_cols <- function() {
  base <- .polio_default_cols()
  list(
    cases = utils::modifyList(
      base$cases,
      list(
        epid = "epid",
        onset_to_notify = "onset_to_notify",
        invest_to_stool1 = "invest_to_stool1",
        stool1collection_date = "stool1collection_date",
        adequate_stool_polis = "adequate_stool_with_condition",
        followup_findings = "followup_findings",
        doses_reported = "total_number_of_doses",
        doses_calculated = "calc_doses_risi"
      )
    ),
    lab = utils::modifyList(
      base$lab,
      list(
        epid = "epid",
        date_collected = "date_stool_collected",
        date_received = "date_stool_received_in_lab"
      )
    )
  )
}

#' Merge user `cols` into the module defaults.
#' @keywords internal
#' @noRd
.afpq_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort("{.arg cols} must be a named list.")
  }
  base <- .afpq_default_cols()
  bad <- setdiff(names(cols), names(base))
  if (length(bad) > 0 || (length(cols) > 0 && is.null(names(cols)))) {
    cli::cli_abort(
      "{.arg cols} may only name the sources {.val {names(base)}}."
    )
  }
  for (src in names(cols)) {
    base[[src]] <- utils::modifyList(base[[src]], cols[[src]])
  }
  base
}

#' Pull canonical columns from a table through its column map.
#'
#' Required keys must exist; optional keys absent from the table become `NA`.
#' With `level`, adds `guid` and `name` for that level with one name per GUID,
#' and drops rows with no GUID unless `drop = FALSE`.
#' @keywords internal
#' @noRd
.afpq_take <- function(
  df,
  m,
  keys,
  optional = character(0),
  level = NULL,
  drop = TRUE
) {
  if (!is.null(level)) {
    keys <- c(keys, paste0(level, "_guid"))
    optional <- c(optional, level)
  }
  src <- unlist(m[keys])
  if (length(src) != length(keys)) {
    unknown <- setdiff(keys, names(m))
    cli::cli_abort("No column mapping for {.val {unknown}}.")
  }
  missing <- setdiff(src, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Input is missing column{?s} {.val {missing}}.",
      "i" = "Map other names through {.arg cols}."
    ))
  }
  out <- list()
  for (k in keys) {
    out[[k]] <- df[[m[[k]]]]
  }
  for (k in optional) {
    nm <- m[[k]]
    out[[k]] <- if (!is.null(nm) && nm %in% names(df)) {
      df[[nm]]
    } else {
      rep(NA, nrow(df))
    }
  }
  out <- tibble::as_tibble(out, .rows = nrow(df))
  if (is.null(level)) {
    return(out)
  }
  out <- out |>
    dplyr::mutate(
      guid = as.character(.data[[paste0(level, "_guid")]]),
      name = as.character(.data[[level]])
    ) |>
    dplyr::mutate(name = .polio_consensus_name(name), .by = guid)
  if (drop) {
    out <- dplyr::filter(out, !is.na(guid))
  }
  out
}

#' Keep rows with onset between `from` and `to`.
#' @keywords internal
#' @noRd
.afpq_window <- function(d, from, to) {
  onset <- .afpq_date(d$onset_date)
  keep <- rep(TRUE, nrow(d))
  if (!is.null(from)) {
    keep <- keep & !is.na(onset) & onset >= from
  }
  if (!is.null(to)) {
    keep <- keep & !is.na(onset) & onset <= to
  }
  dplyr::filter(d, keep)
}

#' Join unit counts onto the reporting universe, when one is supplied.
#' @keywords internal
#' @noRd
.afpq_spine <- function(counts, admin_units, m, level) {
  if (is.null(admin_units)) {
    return(dplyr::arrange(counts, name, guid))
  }
  .afpq_check_df(admin_units, "admin_units")
  spine <- admin_units |>
    .afpq_take(m, character(0), level = level) |>
    dplyr::distinct(guid, name)
  spine |>
    dplyr::left_join(counts, by = "guid", suffix = c("", ".cases")) |>
    dplyr::mutate(name = dplyr::coalesce(name, .data[["name.cases"]])) |>
    dplyr::select(-"name.cases")
}

#' Stool components joined to units and filtered to the window.
#' @keywords internal
#' @noRd
.afpq_components_by_unit <- function(
  cases,
  level,
  from,
  to,
  collection_days,
  min_days_apart,
  adequacy,
  adequacy_args,
  cols
) {
  .afpq_check_df(cases, "cases")
  .afpq_check_level(level)
  from <- .afpq_check_date(from, "from")
  to <- .afpq_check_date(to, "to")
  m <- .afpq_cols(cols)$cases
  comp <- afp_stool_components(
    cases,
    collection_days = collection_days,
    min_days_apart = min_days_apart,
    adequacy = adequacy,
    adequacy_args = adequacy_args,
    cols = cols
  )
  units <- .afpq_take(cases, m, "onset_date", level = level, drop = FALSE)
  dplyr::bind_cols(
    dplyr::select(
      comp,
      first_in_window,
      both_in_window,
      apart,
      adequate,
      late,
      one_or_none
    ),
    units
  ) |>
    dplyr::filter(!is.na(guid)) |>
    .afpq_window(from, to)
}

#' Run the adequacy classifier with the caller's definition and arguments.
#' @keywords internal
#' @noRd
.afpq_adequacy <- function(cases, adequacy, adequacy_args, cols) {
  if (!is.list(adequacy_args)) {
    cli::cli_abort("{.arg adequacy_args} must be a named list.")
  }
  do.call(
    afp_stool_adequacy,
    c(
      list(cases = cases, definition = adequacy, cols = cols),
      adequacy_args
    )
  )
}

#' The dose screen of the CDC desk review.
#' @keywords internal
#' @noRd
.afpq_dose_reason <- function(routine, sia, total, calculated, min_doses,
                              labels) {
  dplyr::case_when(
    (!is.na(routine) & routine > 0) | (!is.na(sia) & sia > 0) ~ NA_character_,
    !is.na(routine) & !is.na(sia) ~ labels[["no_opv"]],
    !is.na(routine) ~ labels[["no_routine"]],
    !is.na(sia) ~ labels[["no_sia"]],
    !is.na(total) & total < min_doses ~ labels[["few_doses"]],
    !is.na(total) ~ NA_character_,
    !is.na(calculated) & calculated < min_doses ~ labels[["few_doses"]],
    !is.na(calculated) ~ NA_character_,
    .default = labels[["no_history"]]
  )
}

#' Percentiles of a day count as a one-row tibble.
#' @keywords internal
#' @noRd
.afpq_quantiles <- function(days, probs) {
  q <- stats::quantile(days, probs, na.rm = TRUE, names = FALSE, type = 7)
  tibble::as_tibble(as.list(stats::setNames(q, names(probs))))
}

.afpq_pct <- function(ok, n) dplyr::if_else(n > 0, 100 * ok / n, NA_real_)
.afpq_num <- function(x) suppressWarnings(as.numeric(x))
.afpq_date <- function(x) {
  force(x)
  tryCatch(
    suppressWarnings(as.Date(x)),
    error = \(e) rep(as.Date(NA), length(x))
  )
}
.afpq_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

# ---- argument checks --------------------------------------------------------

.afpq_check_df <- function(x, arg) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data.frame or tibble.")
  }
  invisible(x)
}

.afpq_check_level <- function(level) {
  if (!is.character(level) || length(level) != 1 ||
        !level %in% c("adm0", "adm1", "adm2")) {
    cli::cli_abort(
      "{.arg level} must be one of {.val {c('adm0', 'adm1', 'adm2')}}."
    )
  }
  invisible(level)
}

.afpq_check_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    cli::cli_abort("{.arg {arg}} must be a single number of zero or more.")
  }
  invisible(x)
}

.afpq_check_range <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 2 || anyNA(x) || x[1] > x[2]) {
    cli::cli_abort(
      "{.arg {arg}} must be two numbers, the lower bound first."
    )
  }
  invisible(x)
}

.afpq_check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string.")
  }
  invisible(x)
}

.afpq_check_chr <- function(x, arg) {
  if (!is.character(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector.")
  }
  invisible(x)
}

.afpq_check_probs <- function(probs) {
  if (!is.numeric(probs) || length(probs) == 0 || anyNA(probs) ||
        any(probs < 0 | probs > 1) || is.null(names(probs)) ||
        any(names(probs) == "")) {
    cli::cli_abort(
      "{.arg probs} must be named numbers between 0 and 1."
    )
  }
  invisible(probs)
}

.afpq_check_date <- function(x, arg, allow_null = TRUE) {
  if (is.null(x) && allow_null) {
    return(NULL)
  }
  out <- if (length(x) == 1) .afpq_date(x) else NA
  if (length(x) != 1 || is.na(out)) {
    cli::cli_abort("{.arg {arg}} must be a single date.")
  }
  out
}

.afpq_match <- function(x, choices, arg) {
  if (identical(x, choices)) {
    return(choices[1])
  }
  if (!is.character(x) || length(x) != 1 || !x %in% choices) {
    cli::cli_abort("{.arg {arg}} must be one of {.val {choices}}.")
  }
  x
}
