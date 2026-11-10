# random state -----------------------------------------------------------------

# runs code under a seed and puts the caller's random state back afterwards
#' @noRd
.sim_with_seed <- function(seed, code) {
  env <- globalenv()
  old_kind <- RNGkind()
  old_seed <- get0(".Random.seed", envir = env, inherits = FALSE)
  on.exit({
    RNGkind(old_kind[[1]], old_kind[[2]], old_kind[[3]])
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = env, inherits = FALSE)) {
        rm(".Random.seed", envir = env)
      }
    } else {
      assign(".Random.seed", old_seed, envir = env)
    }
  })
  set.seed(seed)
  code
}

# argument checks --------------------------------------------------------------

#' @noRd
.sim_check_seed <- function(seed, arg = rlang::caller_arg(seed),
                            call = rlang::caller_env()) {
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) ||
        seed != round(seed)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number.",
      call = call
    )
  }
  invisible(seed)
}

#' @noRd
.sim_check_count <- function(x, arg = rlang::caller_arg(x),
                             call = rlang::caller_env(), min = 0) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x != round(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number.",
      call = call
    )
  }
  if (x < min) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be at least {min}.",
        "x" = "It is {x}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' @noRd
.sim_check_share <- function(x, arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || x > 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single share between 0 and 1.",
      call = call
    )
  }
  invisible(x)
}

#' @noRd
.sim_check_number <- function(x, arg = rlang::caller_arg(x),
                              call = rlang::caller_env(), positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number of 0 or more.",
      call = call
    )
  }
  if (positive && x == 0) {
    cli::cli_abort("{.arg {arg}} must be greater than 0.", call = call)
  }
  invisible(x)
}

#' @noRd
.sim_check_range <- function(x, arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 2 || anyNA(x) || x[[1]] > x[[2]]) {
    cli::cli_abort(
      "{.arg {arg}} must be two numbers, the lower one first.",
      call = call
    )
  }
  invisible(x)
}

#' @noRd
.sim_check_date <- function(x, arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  out <- tryCatch(as.Date(x), error = \(e) as.Date(NA))
  if (length(out) != 1 || is.na(out)) {
    cli::cli_abort("{.arg {arg}} must be a single date.", call = call)
  }
  out
}

#' @noRd
.sim_check_window <- function(start_date, end_date,
                              call = rlang::caller_env()) {
  if (end_date < start_date) {
    cli::cli_abort(
      c(
        "{.arg end_date} must not fall before {.arg start_date}.",
        "x" = "{.arg start_date} is {start_date} and {.arg end_date} is
               {end_date}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# a vector of non-negative weights, named exactly as required when names given
#' @noRd
.sim_check_probs <- function(x, names = NULL, n = NULL,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) == 0 || anyNA(x) || any(x < 0) ||
        sum(x) <= 0) {
    cli::cli_abort(
      "{.arg {arg}} must be non-negative numbers with a positive sum.",
      call = call
    )
  }
  if (!is.null(n) && length(x) != n) {
    cli::cli_abort(
      "{.arg {arg}} must have {n} value{?s}, one for each label.",
      call = call
    )
  }
  if (!is.null(names) && !setequal(rlang::names2(x), names)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be named {.val {names}}.",
        "x" = "It is named {.val {rlang::names2(x)}}."
      ),
      call = call
    )
  }
  invisible(x)
}

# a character vector of labels, named exactly as required when names given
#' @noRd
.sim_check_labels <- function(x, names = NULL, arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0 || anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector with no missing values.",
      call = call
    )
  }
  if (!is.null(names) && !setequal(rlang::names2(x), names)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be named {.val {names}}.",
        "x" = "It is named {.val {rlang::names2(x)}}."
      ),
      call = call
    )
  }
  invisible(x)
}

#' @noRd
.sim_check_admin <- function(admin, arg = rlang::caller_arg(admin),
                             call = rlang::caller_env()) {
  needed <- c(
    "country_iso3code", "adm0", "adm0_guid", "adm1", "adm1_guid",
    "adm1_code", "adm2", "adm2_guid", "adm2_code", "center_lon",
    "center_lat", "u15_pop"
  )
  missing <- setdiff(needed, names(admin))
  if (!is.data.frame(admin) || length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a table from {.fn simulate_admin_frame}.",
        "x" = "Missing column{?s}: {.val {missing}}."
      ),
      call = call
    )
  }
  invisible(admin)
}

# draws ------------------------------------------------------------------------

# whole days with a given mean, overdispersed by a negative binomial size
#' @noRd
.sim_draw_days <- function(n, mean, size) {
  stats::rnbinom(n, size = size, mu = mean)
}

# random GUIDs in the brace-wrapped upper-case form POLIS writes
#' @noRd
.sim_guid <- function(n) {
  hex <- c(as.character(0:9), LETTERS[1:6])
  vapply(
    seq_len(n),
    \(i) {
      s <- paste(sample(hex, 32, replace = TRUE), collapse = "")
      paste0(
        "{", substr(s, 1, 8), "-", substr(s, 9, 12), "-", substr(s, 13, 16),
        "-", substr(s, 17, 20), "-", substr(s, 21, 32), "}"
      )
    },
    character(1)
  )
}

# the calendar years a window touches, with the days of each it covers
#' @noRd
.sim_year_spans <- function(start_date, end_date) {
  years <- seq(
    as.integer(format(start_date, "%Y")), as.integer(format(end_date, "%Y"))
  )
  tibble::tibble(year = years) |>
    dplyr::mutate(
      year_start = as.Date(paste0(.data$year, "-01-01")),
      year_end = as.Date(paste0(.data$year, "-12-31")),
      from = pmax(.data$year_start, start_date),
      to = pmin(.data$year_end, end_date),
      days = as.numeric(.data$to - .data$from) + 1,
      year_days = as.numeric(.data$year_end - .data$year_start) + 1
    ) |>
    dplyr::select("year", "from", "to", "days", "year_days")
}

# a date later than the end of the window has not happened yet
#' @noRd
.sim_censor_after <- function(x, end_date) {
  dplyr::if_else(!is.na(x) & x > end_date, as.Date(NA), x)
}

# a weighted draw of k rows without replacement, as a logical flag
#' @noRd
.sim_pick_weighted <- function(eligible, weight, k) {
  key <- dplyr::if_else(
    eligible, stats::runif(length(eligible))^(1 / weight), -1
  )
  eligible & dplyr::min_rank(dplyr::desc(key)) <= k
}

# the raw detection fields POLIS fills for a virus label
#' @noRd
.sim_virus_flag <- function(label, pattern, value) {
  dplyr::if_else(
    !is.na(label) & grepl(pattern, label), value, NA_character_
  )
}

# the two-digit year an identifier carries
#' @noRd
.sim_yy <- function(date) {
  format(date, "%y")
}
