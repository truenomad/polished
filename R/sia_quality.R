# =============================================================================
# SIA campaign-quality indicators: LQAS and Independent Monitoring (IM)
#
# polished downloads the `Lqas` and `Im` OData tables but preprocess() does
# not turn them into anything -- it only carries `lqas.loaded` / `im.loaded`
# flags on the SIA rows. This module processes the raw tables into analytic
# outputs, reproducing the well-defined POLIS rules:
#
#   * LQAS: default lot size of 60; from 2019 a lot whose sample size is not a
#     multiple of 60 is INVALID; classification recoded to 2-level (Pass/Fail)
#     and 3-level (Pass/Intermediate/Fail); per-district pass% excluding
#     INVALID lots.
#   * IM: missed-children fraction = `1 - sum(marked)/sum(checked)` (or
#     `mean(result)` when no children were checked), aggregated to district /
#     parent, with a Valid/Invalid status flag.
#
# Caveat (LQAS classification thresholds): POLIS classifies lots against
# `REF_LQASThresholds` (a sample-size x decision-value lookup that is not
# exposed by the OData API). Here the Pass/Fail decision uses a transparent,
# documented coverage cut-off (`pass_threshold`, default 0.90) that you can
# override or replace with your own classifier. The default-60 rule, the
# multiple-of-60 INVALID rule, the 2/3-level recodes and the roll-ups are
# faithful to POLIS; only the accept/reject cut-off is a stand-in.
# =============================================================================

utils::globalVariables(c(
  "checked",
  "unvacc",
  "marked",
  "result",
  "coverage",
  "invalid",
  "lqas_class",
  "lqas2",
  "lqas3",
  "adm2_guid",
  "adm2_name",
  "adm1_name",
  "adm0_name",
  "parent_id",
  "lot",
  "year",
  "n_lots",
  "n_pass",
  "n_fail",
  "n_invalid",
  "pass_pct",
  "missed_frac",
  "n_checked",
  "n_marked",
  "im_status"
))

# ---- LQAS -------------------------------------------------------------------

#' Process raw LQAS lots into classifications and district pass rates
#'
#' Turns the raw `Lqas` table into lot-level classifications and a per-district
#' roll-up. Reproduces the POLIS rules that are well defined (default lot size
#' 60; the 2019+ "sample size must be a multiple of 60 or the lot is INVALID"
#' rule; 2-level and 3-level recodes; pass% excluding INVALID lots).
#'
#' @param lqas Raw LQAS table (data.frame/tibble), one row per lot.
#' @param adm2_guid_var,adm2_name_var Column names for the district GUID/name.
#' @param adm1_name_var,adm0_name_var Optional province / country name columns
#'   (set to `NULL` if absent).
#' @param date_var Column with the lot's planned/assessment date (used to apply
#'   the 2019 rule by year).
#' @param checked_var Column with the number of children checked / sample size.
#' @param unvacc_var Column with the number of children found unvaccinated.
#' @param lot_var Optional lot-identifier column (set `NULL` to auto-number).
#' @param default_checked Lot size assumed when `checked_var` is missing
#'   (default `60`, per POLIS).
#' @param multiple_of Sample-size modulus enforced from `enforce_since`
#'   (default `60`).
#' @param enforce_since Year from which the multiple-of rule makes a lot INVALID
#'   (default `2019`).
#' @param pass_threshold Coverage (vaccinated fraction) at/above which a lot is
#'   a Pass (default `0.90`). **Transparent stand-in for `REF_LQASThresholds`.**
#' @param warn_threshold Optional coverage band for the 3-level "Intermediate"
#'   class (default `0.80`). Lots in `[warn_threshold, pass_threshold)` are
#'   Intermediate (3-level) / Fail (2-level).
#' @param verbose Emit a cli summary (default `TRUE`).
#'
#' @return A list with `lots` (lot-level tibble with `coverage`, `invalid`,
#'   `lqas_class`, `lqas2`, `lqas3`), `district` (per-district roll-up with
#'   `n_lots`, `n_pass`, `n_fail`, `n_invalid`, `pass_pct`), and `meta`.
#' @export
process_lqas <- function(
  lqas,
  adm2_guid_var = "Admin2Guid",
  adm2_name_var = "Admin2Name",
  adm1_name_var = "Admin1Name",
  adm0_name_var = "Admin0Name",
  date_var = "ActivityStart",
  checked_var = "ChildrenChecked",
  unvacc_var = "ChildrenUnvaccinated",
  lot_var = NULL,
  default_checked = 60,
  multiple_of = 60,
  enforce_since = 2019,
  pass_threshold = 0.90,
  warn_threshold = 0.80,
  verbose = TRUE
) {
  if (!is.data.frame(lqas)) cli::cli_abort("{.arg lqas} must be a data.frame.")
  if (nrow(lqas) == 0) cli::cli_abort("{.arg lqas} is empty.")

  required <- c(adm2_guid_var, checked_var, unvacc_var, date_var)
  required <- required[
    !vapply(
      list(adm2_guid_var, checked_var, unvacc_var, date_var),
      is.null,
      logical(1)
    )
  ]
  missing_cols <- setdiff(required, names(lqas))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required column(s) in {.arg lqas}:",
      "x" = "{.var {missing_cols}}",
      "i" = "Set the matching `*_var` argument(s) to your column names."
    ))
  }

  std <- lqas |>
    dplyr::transmute(
      adm2_guid = as.character(.data[[adm2_guid_var]]),
      adm2_name = if (
        !is.null(adm2_name_var) && adm2_name_var %in% names(lqas)
      ) {
        as.character(.data[[adm2_name_var]])
      } else {
        NA_character_
      },
      adm1_name = if (
        !is.null(adm1_name_var) && adm1_name_var %in% names(lqas)
      ) {
        as.character(.data[[adm1_name_var]])
      } else {
        NA_character_
      },
      adm0_name = if (
        !is.null(adm0_name_var) && adm0_name_var %in% names(lqas)
      ) {
        as.character(.data[[adm0_name_var]])
      } else {
        NA_character_
      },
      year = lubridate::year(.polis_to_date(.data[[date_var]])),
      checked = suppressWarnings(as.numeric(.data[[checked_var]])),
      unvacc = suppressWarnings(as.numeric(.data[[unvacc_var]])),
      lot = if (!is.null(lot_var) && lot_var %in% names(lqas)) {
        as.character(.data[[lot_var]])
      } else {
        as.character(dplyr::row_number())
      }
    )

  lots <- std |>
    dplyr::mutate(
      checked = dplyr::if_else(
        is.na(checked) | checked <= 0,
        default_checked,
        checked
      ),
      coverage = dplyr::if_else(
        !is.na(unvacc) & checked > 0,
        1 - unvacc / checked,
        NA_real_
      ),
      invalid = !is.na(year) &
        year >= enforce_since &
        (checked %% multiple_of != 0),
      lqas_class = .lqas_classify(
        coverage,
        invalid,
        pass_threshold,
        warn_threshold
      ),
      lqas2 = dplyr::case_when(
        lqas_class == "INVALID" ~ "INVALID",
        lqas_class == "Pass" ~ "Pass",
        TRUE ~ "Fail"
      ),
      lqas3 = lqas_class
    )

  district <- lots |>
    dplyr::group_by(adm2_guid, adm2_name, adm1_name, adm0_name, year) |>
    dplyr::summarise(
      n_lots = dplyr::n(),
      n_pass = sum(lqas2 == "Pass", na.rm = TRUE),
      n_fail = sum(lqas2 == "Fail", na.rm = TRUE),
      n_invalid = sum(lqas2 == "INVALID", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pass_pct = dplyr::if_else(
        (n_pass + n_fail) > 0,
        100 * n_pass / (n_pass + n_fail),
        NA_real_
      )
    ) |>
    dplyr::arrange(adm0_name, adm1_name, adm2_name, year)

  meta <- list(
    n_lots = nrow(lots),
    default_checked = default_checked,
    multiple_of = multiple_of,
    enforce_since = enforce_since,
    pass_threshold = pass_threshold,
    warn_threshold = warn_threshold
  )

  if (verbose) .lqas_print_summary(lots, district, meta)

  list(lots = lots, district = district, meta = meta)
}

#' Default LQAS classifier (transparent stand-in for REF_LQASThresholds).
#' @keywords internal
#' @noRd
.lqas_classify <- function(coverage, invalid, pass_threshold, warn_threshold) {
  dplyr::case_when(
    invalid ~ "INVALID",
    is.na(coverage) ~ NA_character_,
    coverage >= pass_threshold ~ "Pass",
    !is.null(warn_threshold) & coverage >= warn_threshold ~ "Intermediate",
    TRUE ~ "Fail"
  )
}

#' @keywords internal
#' @noRd
.lqas_print_summary <- function(lots, district, meta) {
  cli::cli_rule()
  cli::cli_h2("LQAS Summary")
  cli::cli_h3("Rules")
  cli::cli_bullets(c(
    "*" = "Default lot size: {meta$default_checked}",
    "*" = "INVALID if sample size not a multiple of {meta$multiple_of} (from {meta$enforce_since})",
    "*" = "Pass if coverage >= {meta$pass_threshold}; Intermediate if >= {meta$warn_threshold} (3-level)",
    "!" = "Classification cut-off is a transparent stand-in, not REF_LQASThresholds."
  ))
  tab <- lots |>
    dplyr::count(lqas2, name = "n") |>
    dplyr::mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1))
  cli::cli_h3("Lots by 2-level class")
  for (i in seq_len(nrow(tab))) {
    cli::cli_text("{tab$lqas2[i]}: {tab$n[i]} ({tab$pct[i]}%)")
  }
  n_dist <- nrow(district)
  cli::cli_alert_success(
    "Rolled up {meta$n_lots} lot(s) to {n_dist} district-year(s)."
  )
  cli::cli_rule()
}

# ---- Independent Monitoring (IM) -------------------------------------------

#' Process raw Independent Monitoring (IM) data into missed-children rates
#'
#' Reproduces the POLIS IM missed-children computation: per district/parent,
#' `missed = 1 - sum(marked)/sum(checked)`, falling back to `mean(result)` when
#' no children were checked; with a Valid/Invalid status flag (Invalid when the
#' result is missing or negative).
#'
#' @param im Raw IM table (data.frame/tibble).
#' @param adm2_guid_var,adm2_name_var District GUID / name columns.
#' @param adm1_name_var,adm0_name_var Optional province / country name columns.
#' @param checked_var Column with children checked.
#' @param marked_var Column with children marked (finger-marked / vaccinated).
#' @param result_var Optional pre-computed per-row missed result (used as the
#'   fallback when no children were checked).
#' @param date_var Optional date column (for a `year` grouping).
#' @param verbose Emit a cli summary (default `TRUE`).
#'
#' @return A list with `district` (per-district missed-children rate + status)
#'   and `meta`.
#' @export
process_im <- function(
  im,
  adm2_guid_var = "Admin2Guid",
  adm2_name_var = "Admin2Name",
  adm1_name_var = "Admin1Name",
  adm0_name_var = "Admin0Name",
  checked_var = "ChildrenChecked",
  marked_var = "ChildrenMarked",
  result_var = "Result",
  date_var = "ActivityStart",
  verbose = TRUE
) {
  if (!is.data.frame(im)) cli::cli_abort("{.arg im} must be a data.frame.")
  if (nrow(im) == 0) cli::cli_abort("{.arg im} is empty.")

  required <- c(adm2_guid_var, checked_var, marked_var)
  missing_cols <- setdiff(required, names(im))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required column(s) in {.arg im}:",
      "x" = "{.var {missing_cols}}",
      "i" = "Set the matching `*_var` argument(s) to your column names."
    ))
  }

  std <- im |>
    dplyr::transmute(
      adm2_guid = as.character(.data[[adm2_guid_var]]),
      adm2_name = if (!is.null(adm2_name_var) && adm2_name_var %in% names(im)) {
        as.character(.data[[adm2_name_var]])
      } else {
        NA_character_
      },
      adm1_name = if (!is.null(adm1_name_var) && adm1_name_var %in% names(im)) {
        as.character(.data[[adm1_name_var]])
      } else {
        NA_character_
      },
      adm0_name = if (!is.null(adm0_name_var) && adm0_name_var %in% names(im)) {
        as.character(.data[[adm0_name_var]])
      } else {
        NA_character_
      },
      year = if (!is.null(date_var) && date_var %in% names(im)) {
        lubridate::year(.polis_to_date(.data[[date_var]]))
      } else {
        NA_integer_
      },
      checked = suppressWarnings(as.numeric(.data[[checked_var]])),
      marked = suppressWarnings(as.numeric(.data[[marked_var]])),
      result = if (!is.null(result_var) && result_var %in% names(im)) {
        suppressWarnings(as.numeric(.data[[result_var]]))
      } else {
        NA_real_
      }
    )

  district <- std |>
    dplyr::group_by(adm2_guid, adm2_name, adm1_name, adm0_name, year) |>
    dplyr::summarise(
      n_checked = sum(checked, na.rm = TRUE),
      n_marked = sum(marked, na.rm = TRUE),
      missed_frac = dplyr::if_else(
        sum(checked, na.rm = TRUE) == 0,
        mean(result, na.rm = TRUE),
        1 - sum(marked, na.rm = TRUE) / sum(checked, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      missed_frac = dplyr::if_else(is.nan(missed_frac), NA_real_, missed_frac),
      im_status = dplyr::if_else(
        is.na(missed_frac) | missed_frac < 0,
        "Invalid",
        "Valid"
      )
    ) |>
    dplyr::arrange(adm0_name, adm1_name, adm2_name, year)

  meta <- list(n_rows = nrow(std), n_district = nrow(district))

  if (verbose) {
    cli::cli_rule()
    cli::cli_h2("Independent Monitoring Summary")
    cli::cli_text(
      "Formula: missed = 1 - sum(marked)/sum(checked) (else mean(result))"
    )
    n_valid <- sum(district$im_status == "Valid", na.rm = TRUE)
    n_invalid <- sum(district$im_status == "Invalid", na.rm = TRUE)
    cli::cli_alert_success(
      "{meta$n_district} district-year(s): {n_valid} Valid, {n_invalid} Invalid."
    )
    if (n_valid > 0) {
      mean_missed <- round(
        100 *
          mean(
            district$missed_frac[district$im_status == "Valid"],
            na.rm = TRUE
          ),
        1
      )
      cli::cli_text("Mean missed-children (valid): {mean_missed}%")
    }
    cli::cli_rule()
  }

  list(district = district, meta = meta)
}

# ---- shared helper ----------------------------------------------------------

#' Parse a heterogeneous date column to Date (tries several formats).
#' @keywords internal
#' @noRd
.polis_to_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  suppressWarnings(lubridate::as_date(lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd", "Y-m-d", "Y-m-d H:M:S", "dmY", "mdY"),
    quiet = TRUE
  )))
}
