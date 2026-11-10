# =============================================================================
# AFP cases awaiting classification
#
# The pending-classification stage ladder: which AFP cases are still open,
# the dates each case has reached, the furthest laboratory rung any of its
# specimens has climbed, and a table that counts open cases by stage and
# month of onset, with the number that have waited past a stale limit.
# =============================================================================

utils::globalVariables(c(
  "is_open",
  "positive",
  "onset",
  "onset_month",
  "stage",
  "rung",
  "specimens",
  "own_stool",
  "collected",
  "received",
  "resulted",
  "days",
  "records",
  "open",
  "share",
  "median_days",
  "over_stale",
  "open_share",
  ".pend_n",
  ".pend_epid"
))

# ---- column maps ------------------------------------------------------------

#' Default column map for the pending module.
#'
#' Keys shared with the indicator engine are taken from
#' `.polio_default_cols()`; the rest are columns that map lacks.
#' @keywords internal
#' @noRd
.pend_default_cols <- function() {
  base <- .polio_default_cols()$cases
  shared <- c(
    "class", "onset_date", "surveillance_type", "stool2collection_date",
    "adm0_guid", "adm1_guid", "adm2_guid", "adm0", "adm1", "adm2"
  )
  list(
    cases = c(
      base[shared],
      list(
        epid = "epid",
        stool1collection_date = "stool1collection_date",
        notification_date = "notification_date",
        itd_date = "epi_date_itd_results_received",
        hq_date = "date_notificationto_hq",
        isolation_date = "epi_date_isolation_results_received"
      )
    ),
    specimens = list(
      epid = "epid",
      date_collected = "date_stool_collected",
      date_received = "date_stool_received_in_lab",
      culture_date = "date_final_cell_culture_results",
      itd_date = "date_itd_result_reported",
      sequencing_date = "dateof_sequencing"
    )
  )
}

#' Merge user `cols` over the module defaults.
#' @keywords internal
#' @noRd
.pend_merge_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort("{.arg cols} must be a named list.")
  }
  base <- .pend_default_cols()
  bad <- setdiff(names(cols), names(base))
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Unknown {.arg cols} source{?s}: {.val {bad}}.",
      "i" = "Use {.val cases} or {.val specimens}."
    ))
  }
  for (src in names(cols)) {
    base[[src]] <- utils::modifyList(base[[src]], cols[[src]])
  }
  base
}

# ---- validation helpers -----------------------------------------------------

.pend_check_df <- function(x, arg) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {arg}} must be a data.frame or tibble.")
  }
  invisible(x)
}

.pend_need <- function(df, m, keys, arg) {
  wanted <- unlist(m[keys], use.names = FALSE)
  missing <- setdiff(wanted, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg {arg}} lacks column{?s} {.val {missing}}.",
      "i" = "Map other names through {.arg cols}."
    ))
  }
  invisible(df)
}

.pend_check_count <- function(x, arg, min = 0) {
  ok <- is.numeric(x) && length(x) == 1 && !is.na(x) &&
    x >= min && x == round(x)
  if (!ok) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number of {min} or more."
    )
  }
  invisible(x)
}

.pend_check_stages <- function(stages) {
  ok <- is.character(stages) && length(stages) == 4 &&
    !anyNA(stages) && !anyDuplicated(stages)
  if (!ok) {
    cli::cli_abort(
      "{.arg stages} must be four distinct names, one for each rung 0 to 3."
    )
  }
  invisible(stages)
}

.pend_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  suppressWarnings(as.Date(x))
}

.pend_month <- function(x) lubridate::floor_date(x, "month")

# ---- case-level classifiers -------------------------------------------------

#' Flag AFP cases that are still awaiting classification
#'
#' A case is open, meaning still awaiting classification, when POLIS has not
#' written a final classification for it. That is the case when the
#' classification field is missing or blank, or when it holds one of the
#' pending labels in `pending_classes`. Labels are matched after trimming
#' white space and ignoring case.
#'
#' @param classification A vector of case classifications, for example the
#'   `classification_all` column of a cleaned AFP table.
#' @param pending_classes Classification labels that mean the case is still
#'   pending. The default, `c("PENDING", "LAB PENDING")`, is the programme
#'   standard and matches `calc_polio_indicators()`.
#'
#' @return A logical vector the length of `classification`, `TRUE` where the
#'   case is open. It is never `NA`.
#' @seealso [calc_pending_stage()] for the table built on this flag.
#' @examples
#' afp_pending_open(c("NPAFP", "PENDING", "", NA, "Lab Pending", "WPV1"))
#' @export
afp_pending_open <- function(classification,
                             pending_classes = c("PENDING", "LAB PENDING")) {
  if (!is.atomic(classification)) {
    cli::cli_abort("{.arg classification} must be an atomic vector.")
  }
  if (!is.character(pending_classes)) {
    cli::cli_abort("{.arg pending_classes} must be a character vector.")
  }
  value <- trimws(as.character(classification))
  is.na(value) | value == "" |
    toupper(value) %in% toupper(trimws(pending_classes))
}

#' Add the dates an AFP case has reached
#'
#' Adds the open flag and the dates a case has reached to a cleaned AFP table.
#' No date is imputed.
#'
#' The columns added are:
#'
#' * `is_open`: the case is still awaiting classification, as defined by
#'   [afp_pending_open()].
#' * `positive`: the case is classified and its classification names a wild
#'   poliovirus or a vaccine-derived poliovirus (it contains `"WPV"` or
#'   `"VDPV"`, ignoring case).
#' * `notified`: the notification date.
#' * `known`: for a positive case only, the date the polio-positive result
#'   reached the programme. Under `known_rule = "itd"` this is the date the
#'   ITD result was received, or the date of notification to headquarters
#'   where that is missing. Under `known_rule = "hq"` the order is reversed.
#'   It is `NA` for every case that is not positive.
#' * `resolved`: the date the case stopped being pending. It is `NA` while
#'   the case is open, equals `known` for a positive case, and is the date
#'   the isolation result was received for any other classified case.
#'
#' @param cases A cleaned AFP table, one row per case (from [clean_afp()]).
#' @param known_rule Which date marks a positive result as known: `"itd"`
#'   (the default and the programme standard) or `"hq"`.
#' @param cols A named list overriding the default column names, in the same
#'   style as [calc_polio_indicators()], for example
#'   `list(cases = list(itd_date = "my_itd"))`. The keys this function reads
#'   from `cases` are `class` (`"classification_all"`), `notification_date`
#'   (`"notification_date"`), `itd_date`
#'   (`"epi_date_itd_results_received"`), `hq_date`
#'   (`"date_notificationto_hq"`) and `isolation_date`
#'   (`"epi_date_isolation_results_received"`).
#' @inheritParams afp_pending_open
#'
#' @return `cases` as a tibble with the columns `is_open`, `positive`,
#'   `notified`, `known` and `resolved` added or replaced.
#' @seealso [calc_pending_stage()].
#' @examples
#' cases <- tibble::tibble(
#'   classification_all = c("WPV1", "NPAFP", "PENDING"),
#'   notification_date = as.Date("2025-01-05"),
#'   epi_date_itd_results_received = as.Date(c("2025-02-01", NA, NA)),
#'   date_notificationto_hq = as.Date(c("2025-02-03", NA, NA)),
#'   epi_date_isolation_results_received = as.Date(c(NA, "2025-01-30", NA))
#' )
#' afp_pending_dates(cases)
#' @export
afp_pending_dates <- function(cases,
                              known_rule = c("itd", "hq"),
                              pending_classes = c("PENDING", "LAB PENDING"),
                              cols = list()) {
  .pend_check_df(cases, "cases")
  if (!is.character(known_rule) || !all(known_rule %in% c("itd", "hq"))) {
    cli::cli_abort("{.arg known_rule} must be {.val itd} or {.val hq}.")
  }
  known_rule <- known_rule[1]
  m <- .pend_merge_cols(cols)$cases
  .pend_need(
    cases, m,
    c("class", "notification_date", "itd_date", "hq_date", "isolation_date"),
    "cases"
  )
  first <- if (known_rule == "hq") m$hq_date else m$itd_date
  second <- if (known_rule == "hq") m$itd_date else m$hq_date

  cases |>
    tibble::as_tibble() |>
    dplyr::mutate(
      is_open = afp_pending_open(.data[[m$class]], pending_classes),
      positive = !is_open & stringr::str_detect(
        toupper(as.character(.data[[m$class]])), "WPV|VDPV"
      ),
      notified = .pend_as_date(.data[[m$notification_date]]),
      known = dplyr::if_else(
        positive,
        dplyr::coalesce(
          .pend_as_date(.data[[first]]), .pend_as_date(.data[[second]])
        ),
        as.Date(NA)
      ),
      resolved = dplyr::case_when(
        is_open ~ as.Date(NA),
        positive ~ known,
        TRUE ~ .pend_as_date(.data[[m$isolation_date]])
      )
    )
}

#' Place each stool specimen on the laboratory ladder
#'
#' Returns one row per stool specimen with its collection, receipt and result
#' dates and the rung of the laboratory ladder it has reached. The rungs are:
#'
#' * 0: no collection date is recorded.
#' * 1, collected: the stool collection date is recorded, and no later date.
#' * 2, received: the date the stool was received in the laboratory is
#'   recorded, and no result date.
#' * 3, resulted: a result date is recorded. A date at any of the final cell
#'   culture result, the ITD result or the sequencing date counts, since each
#'   means the laboratory has tested the specimen.
#'
#' A specimen takes the highest rung whose date is present, so a received
#' date without a collection date still places the specimen on rung 2.
#'
#' @param specimens A cleaned human-specimen table, one row per specimen
#'   (from [clean_human_spec()]).
#' @param cols A named list overriding the default column names, for example
#'   `list(specimens = list(date_collected = "my_date"))`. The keys this
#'   function reads from `specimens` are `epid` (`"epid"`), `date_collected`
#'   (`"date_stool_collected"`), `date_received`
#'   (`"date_stool_received_in_lab"`), `culture_date`
#'   (`"date_final_cell_culture_results"`), `itd_date`
#'   (`"date_itd_result_reported"`) and `sequencing_date`
#'   (`"dateof_sequencing"`).
#'
#' @return A tibble with one row per specimen and the columns `epid`,
#'   `collected`, `received`, `resulted` (dates) and `rung` (integer 0 to 3).
#' @seealso [afp_specimen_receipt()] for the days from collection to
#'   laboratory receipt of each specimen; [afp_pending_stage()] for the rung
#'   of a case.
#' @examples
#' spec <- tibble::tibble(
#'   epid = c("A-1", "A-2", "A-3"),
#'   date_stool_collected = as.Date(c("2025-01-02", "2025-01-02", NA)),
#'   date_stool_received_in_lab = as.Date(c(NA, "2025-01-06", NA)),
#'   date_final_cell_culture_results = as.Date(NA),
#'   date_itd_result_reported = as.Date(c(NA, "2025-01-20", NA)),
#'   dateof_sequencing = as.Date(NA)
#' )
#' afp_specimen_rung(spec)
#' @export
afp_specimen_rung <- function(specimens, cols = list()) {
  .pend_check_df(specimens, "specimens")
  m <- .pend_merge_cols(cols)$specimens
  .pend_need(
    specimens, m,
    c(
      "epid", "date_collected", "date_received", "culture_date", "itd_date",
      "sequencing_date"
    ),
    "specimens"
  )
  specimens |>
    tibble::as_tibble() |>
    dplyr::transmute(
      epid = .data[[m$epid]],
      collected = .pend_as_date(.data[[m$date_collected]]),
      received = .pend_as_date(.data[[m$date_received]]),
      resulted = dplyr::coalesce(
        .pend_as_date(.data[[m$culture_date]]),
        .pend_as_date(.data[[m$itd_date]]),
        .pend_as_date(.data[[m$sequencing_date]])
      )
    ) |>
    dplyr::mutate(
      rung = dplyr::case_when(
        !is.na(resulted) ~ 3L,
        !is.na(received) ~ 2L,
        !is.na(collected) ~ 1L,
        TRUE ~ 0L
      )
    )
}

#' Place each AFP case on the laboratory ladder
#'
#' Gives each case the stage of the laboratory ladder it has reached. A case
#' sits on the furthest rung any of its specimens reached (see
#' [afp_specimen_rung()]), so a second stool collected late cannot pull a
#' case back down the ladder. Specimens are linked to cases by EPID number;
#' specimens with no EPID are ignored. Where no specimen links, or the
#' linked specimens are all on rung 0, a stool collection date on the case
#' record itself (first or second stool) places the case on rung 1.
#'
#' The stage names follow the rungs in order. With the default `stages`:
#'
#' * `none`: no specimen has been collected.
#' * `collected`: a specimen has been collected and is awaiting receipt at
#'   the laboratory.
#' * `at_lab`: a specimen is at the laboratory and awaiting its isolation
#'   result.
#' * `resulted`: a laboratory result is recorded and the case is awaiting
#'   final classification.
#'
#' The stage is assigned to every case, open or classified. A classified
#' case can still lack a receipt or result date, so the stage reports what
#' POLIS has recorded rather than where a specimen physically is.
#'
#' @inheritParams afp_pending_dates
#' @param specimens An optional cleaned human-specimen table (from
#'   [clean_human_spec()]). When `NULL`, cases are placed from their own
#'   stool collection dates only.
#' @param stages Four distinct stage names for rungs 0 to 3. The default is
#'   `c("none", "collected", "at_lab", "resulted")`.
#' @param cols A named list overriding the default column names. Besides the
#'   `specimens` keys listed in [afp_specimen_rung()], this function reads
#'   the `cases` keys `epid` (`"epid"`), `stool1collection_date`
#'   (`"stool1collection_date"`) and `stool2collection_date`
#'   (`"stool2collection_date"`).
#'
#' @return `cases` as a tibble with the columns `specimens` (the number of
#'   linked specimens), `rung` (integer 0 to 3) and `stage` (a factor with
#'   levels `stages`) added or replaced.
#' @seealso [calc_pending_stage()].
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("A-1", "A-2"),
#'   stool1collection_date = as.Date(c(NA, "2025-01-02")),
#'   stool2collection_date = as.Date(NA)
#' )
#' afp_pending_stage(cases)
#' @export
afp_pending_stage <- function(cases,
                              specimens = NULL,
                              stages = c(
                                "none", "collected", "at_lab", "resulted"
                              ),
                              cols = list()) {
  .pend_check_df(cases, "cases")
  .pend_check_stages(stages)
  m <- .pend_merge_cols(cols)$cases
  .pend_need(
    cases, m, c("epid", "stool1collection_date", "stool2collection_date"),
    "cases"
  )

  case_rung <- if (is.null(specimens)) {
    tibble::tibble(
      .pend_epid = character(), specimens = integer(), rung = integer()
    )
  } else {
    specimens |>
      afp_specimen_rung(cols = cols) |>
      dplyr::filter(!is.na(epid)) |>
      dplyr::summarise(specimens = dplyr::n(), rung = max(rung), .by = epid) |>
      dplyr::rename(.pend_epid = "epid")
  }

  cases |>
    tibble::as_tibble() |>
    dplyr::select(-dplyr::any_of(c("specimens", "rung", "stage"))) |>
    dplyr::mutate(.pend_epid = .data[[m$epid]]) |>
    dplyr::left_join(case_rung, by = ".pend_epid") |>
    dplyr::mutate(
      specimens = tidyr::replace_na(specimens, 0L),
      own_stool = as.integer(
        !is.na(.pend_as_date(.data[[m$stool1collection_date]])) |
          !is.na(.pend_as_date(.data[[m$stool2collection_date]]))
      ),
      rung = pmax(tidyr::replace_na(rung, 0L), own_stool),
      stage = factor(stages[rung + 1L], levels = stages)
    ) |>
    dplyr::select(-"own_stool", -".pend_epid")
}

# ---- the stage table --------------------------------------------------------

#' Months of onset in the reporting window, oldest first.
#' @keywords internal
#' @noRd
.pend_window <- function(end, months) {
  rev(seq(.pend_month(end), by = "-1 month", length.out = months))
}

#' Cut the window back to the oldest month still holding an open case.
#' @keywords internal
#' @noRd
.pend_trim_window <- function(staged, window) {
  open_months <- staged |>
    dplyr::filter(is_open, onset_month %in% window) |>
    dplyr::pull(onset_month)
  if (length(open_months) == 0) {
    return(window)
  }
  window[window >= min(open_months)]
}

#' Admin columns kept for a reporting level, guid then name per level.
#' @keywords internal
#' @noRd
.pend_level_keys <- function(level) {
  depth <- match(level, c("adm0", "adm1", "adm2")) - 1L
  as.vector(rbind(
    paste0("adm", 0:depth, "_guid"),
    paste0("adm", 0:depth)
  ))
}

#' Count AFP cases awaiting classification by stage and month of onset
#'
#' Builds the pending-classification stage table: one row per area and month
#' of onset in the reporting window, giving how many AFP cases the month
#' holds, how many are still open, how the open cases split across the
#' laboratory ladder, and how long they have been waiting.
#'
#' @section Which cases count:
#' Cases are kept when their surveillance type is in `surveillance_type` and
#' they have a paralysis onset date. Each case is flagged open with
#' [afp_pending_open()] and placed on the ladder with [afp_pending_stage()].
#'
#' @section The reporting window:
#' The window holds `window_months` calendar months of onset, ending with the
#' month of `to`. By default `to` is the latest month of onset among the kept
#' cases, not `as_of`, because the running month holds few onsets yet. With `trim_window = TRUE` the window is then cut back to the
#' oldest month that still holds an open case, so months the laboratory has
#' fully cleared at the start of the window are dropped. Every month left in
#' the window keeps a row in every area, including months with no cases.
#' Open cases with an onset before the window are not counted.
#'
#' @section Counts:
#' * `records`: AFP cases with onset in the month.
#' * `open`: those cases still awaiting classification.
#' * `open_share`: `open / records` on a scale of 0 to 1, or `NA` when the
#'   month has no cases.
#' * `<stage>_n`: open cases on each stage of the ladder, one column per
#'   entry of `stages`. Every stage keeps a column even when no case
#'   reached it.
#' * `<stage>_share`: each stage's share of the month's open cases on a
#'   scale of 0 to 1, or `0` when the month has no open case.
#' * `median_days`: the median number of days from onset to `as_of` among
#'   the month's open cases, or `NA` when it has none.
#' * `over_stale`: open cases that have waited more than `stale_days` days
#'   from onset to `as_of`. A case that has waited exactly `stale_days`
#'   days is not counted.
#' * `partial`: `TRUE` when the month of onset is the month of `as_of` or
#'   later, so it is still being reported.
#' * `month_label`: the month of onset written as, for example, `"Jan 2025"`.
#'
#' @section Difference from `unclass_cases_pct`:
#' [calc_polio_indicators()] reports `unclass_cases_pct`, the share of AFP
#' cases that are unclassified. That indicator counts cases aged under 15
#' years, excludes `NOT-AFP`, treats `UNKNOWN` as unclassified, does not
#' count a blank classification, and is reported by year of onset. The
#' `open_share` here counts every case of the surveillance type whatever its
#' age or classification, treats a blank classification as open, does not
#' treat `UNKNOWN` as open, and is reported by month of onset within the
#' window.
#'
#' @inheritParams afp_pending_stage
#' @inheritParams afp_pending_open
#' @param cases A cleaned AFP table, one row per case (from [clean_afp()]).
#'   It should already be limited to the country or countries of interest.
#' @param as_of The reporting date, as a `Date` or a string that `as.Date()`
#'   reads. Defaults to `Sys.Date()`. The waiting time is measured to it, and
#'   it decides which months are marked `partial`.
#' @param window_months How many months of onset the window holds before
#'   trimming. The default is 12.
#' @param to Any date in the last month of onset in the window. When `NULL`
#'   (the default) it is the latest month of onset among the kept cases.
#' @param trim_window If `TRUE` (the default), cut the start of the window
#'   back to the oldest month that still holds an open case.
#' @param stale_days Waiting time in days beyond which an open case is
#'   counted in `over_stale`. The default is 120, the point after which a
#'   result arrives too late to change the response.
#' @param level The reporting level: `"adm0"` (country, the default),
#'   `"adm1"` (province) or `"adm2"` (district). The table has one row per
#'   area that holds at least one kept case, and month.
#' @param surveillance_type Surveillance types to keep, matched against the
#'   `surveillance_type` column (`"surveillance_type_name"`). The default
#'   `"AFP"` leaves out contact and healthy-child samples. Use `NULL` to
#'   keep every row.
#' @param cols A named list overriding the default column names, in the
#'   style of [calc_polio_indicators()]. Besides the keys listed in
#'   [afp_pending_stage()], this function reads the `cases` keys `class`,
#'   `onset_date`, `surveillance_type` and the admin guid and name columns
#'   of the chosen level.
#'
#' @return A tibble with one row per area and month of onset, sorted by area
#'   and month: `level`, `guid` and `name` for the chosen level, the GUID and
#'   name columns of every level above it (for example `adm0_guid` and
#'   `adm0` when `level = "adm1"`), `onset_month` and the counts above.
#' @seealso [afp_pending_open()], [afp_pending_stage()],
#'   [afp_pending_dates()].
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("A-1", "A-2", "A-3"),
#'   classification_all = c("NPAFP", "PENDING", ""),
#'   paralysis_onset_date = as.Date(
#'     c("2025-01-10", "2025-02-03", "2025-02-20")
#'   ),
#'   surveillance_type_name = "AFP",
#'   stool1collection_date = as.Date(c("2025-01-12", "2025-02-05", NA)),
#'   stool2collection_date = as.Date(NA),
#'   adm0_guid = "G0",
#'   adm0 = "COUNTRY"
#' )
#' calc_pending_stage(cases, as_of = as.Date("2025-03-15"))
#' @export
calc_pending_stage <- function(cases,
                               specimens = NULL,
                               as_of = Sys.Date(),
                               window_months = 12L,
                               to = NULL,
                               trim_window = TRUE,
                               stale_days = 120L,
                               level = "adm0",
                               surveillance_type = "AFP",
                               pending_classes = c("PENDING", "LAB PENDING"),
                               stages = c(
                                 "none", "collected", "at_lab", "resulted"
                               ),
                               cols = list()) {
  # ---- validate ------------------------------------------------------------
  .pend_check_df(cases, "cases")
  if (!is.null(specimens)) {
    .pend_check_df(specimens, "specimens")
  }
  as_of <- .pend_as_date(as_of)
  if (length(as_of) != 1 || is.na(as_of)) {
    cli::cli_abort("{.arg as_of} must be a single date.")
  }
  .pend_check_count(window_months, "window_months", min = 1)
  .pend_check_count(stale_days, "stale_days", min = 0)
  if (!isTRUE(trim_window) && !isFALSE(trim_window)) {
    cli::cli_abort("{.arg trim_window} must be {.val TRUE} or {.val FALSE}.")
  }
  if (!is.character(level) || length(level) != 1 ||
        !level %in% c("adm0", "adm1", "adm2")) {
    cli::cli_abort(
      "{.arg level} must be one of {.val adm0}, {.val adm1} or {.val adm2}."
    )
  }
  .pend_check_stages(stages)
  m <- .pend_merge_cols(cols)$cases
  keys <- .pend_level_keys(level)
  need <- c("class", "onset_date", keys)
  if (!is.null(surveillance_type)) {
    need <- c(need, "surveillance_type")
  }
  .pend_need(cases, m, need, "cases")

  # ---- the cases the table counts ------------------------------------------
  kept <- tibble::as_tibble(cases)
  if (!is.null(surveillance_type)) {
    kept <- dplyr::filter(
      kept, .data[[m$surveillance_type]] %in% surveillance_type
    )
  }
  kept <- dplyr::filter(kept, !is.na(.pend_as_date(.data[[m$onset_date]])))
  if (nrow(kept) == 0) {
    cli::cli_abort(
      "{.arg cases} holds no case of the surveillance type with an onset date."
    )
  }

  staged <- kept |>
    afp_pending_stage(specimens = specimens, stages = stages, cols = cols) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(unlist(m[keys], use.names = FALSE))),
      onset = .pend_as_date(.data[[m$onset_date]]),
      onset_month = .pend_month(onset),
      is_open = afp_pending_open(.data[[m$class]], pending_classes),
      stage
    ) |>
    dplyr::rename(dplyr::all_of(stats::setNames(
      unlist(m[keys], use.names = FALSE), keys
    )))

  # ---- window --------------------------------------------------------------
  end <- if (is.null(to)) {
    max(staged$onset_month)
  } else {
    .pend_as_date(to)
  }
  if (length(end) != 1 || is.na(end)) {
    cli::cli_abort("{.arg to} must be a single date or {.val NULL}.")
  }
  window <- .pend_window(end, window_months)
  if (trim_window) {
    window <- .pend_trim_window(staged, window)
  }

  # ---- counts --------------------------------------------------------------
  by <- c(keys, "onset_month")
  grid <- staged |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) |>
    tidyr::expand_grid(onset_month = window)

  in_window <- dplyr::filter(staged, onset_month %in% window)

  totals <- in_window |>
    dplyr::summarise(
      records = dplyr::n(), open = sum(is_open), .by = dplyr::all_of(by)
    )

  queue <- dplyr::filter(in_window, is_open)

  # every stage keeps a column even where no case reached it
  split <- grid |>
    tidyr::expand_grid(stage = factor(stages, levels = stages)) |>
    dplyr::left_join(
      dplyr::count(queue, dplyr::across(dplyr::all_of(c(by, "stage"))),
                   name = ".pend_n"),
      by = c(by, "stage")
    ) |>
    dplyr::mutate(
      n = tidyr::replace_na(.pend_n, 0L),
      share = n / max(sum(n), 1L),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::select(-".pend_n") |>
    tidyr::pivot_wider(
      names_from = "stage", values_from = c("n", "share"),
      names_glue = "{stage}_{.value}"
    )

  waiting <- queue |>
    dplyr::mutate(days = as.integer(as_of - onset)) |>
    dplyr::summarise(
      median_days = as.numeric(stats::median(days)),
      over_stale = sum(days > stale_days),
      .by = dplyr::all_of(by)
    )

  grid |>
    dplyr::left_join(totals, by = by) |>
    dplyr::left_join(split, by = by) |>
    dplyr::left_join(waiting, by = by) |>
    dplyr::mutate(
      dplyr::across(
        c("records", "open", "over_stale"),
        \(x) tidyr::replace_na(x, 0L)
      ),
      open_share = dplyr::if_else(records > 0, open / records, NA_real_),
      partial = onset_month >= .pend_month(as_of),
      month_label = format(onset_month, "%b %Y")
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by))) |>
    dplyr::rename(dplyr::all_of(c(
      guid = paste0(level, "_guid"), name = level
    ))) |>
    dplyr::mutate(level = level) |>
    dplyr::relocate("level", "guid", "name")
}
