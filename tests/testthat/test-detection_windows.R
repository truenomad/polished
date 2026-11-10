# Detection windows tests. Synthetic values only, built so every count can be
# checked by hand against the window boundaries.

detw_as_of <- as.Date("2024-07-01")

# Each row sits a set number of days before the reporting date. The default
# windows cover days 0 to 218, 219 to 402, 0 to 400 and 401 to 766.
make_detw_detections <- function() {
  offsets <- c(0, 218, 219, 400, 401, 402, 403, 766, 767, -1)
  tibble::tibble(
    epid = paste0("E-", seq_along(offsets)),
    country_iso3code = c(
      "PAK", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "PAK", "AFG", "AFG"
    ),
    virus_date = detw_as_of - offsets,
    source = c(
      "AFP", "Environmental", "Contact", "Environmental", "AFP",
      "Environmental", "Healthy", "AFP", "AFP", "Environmental"
    ),
    adm1 = c("P1", "P1", "P2", NA, "P2", "P3", "P3", "P4", "P5", "P6"),
    virus_cluster = c("C1", "", "C1", "C2", NA, "C3", "C3", "C4", "C9", "C9")
  ) |>
    dplyr::bind_rows(
      tibble::tibble(
        epid = "E-NA", country_iso3code = "AFG", virus_date = as.Date(NA),
        source = "AFP", adm1 = "P1", virus_cluster = "C1"
      )
    ) |>
    dplyr::mutate(
      adm0_guid = paste0("G-", country_iso3code),
      adm0 = country_iso3code,
      adm1_guid = dplyr::if_else(is.na(adm1), NA, paste0("G-", adm1))
    )
}

detw_value <- function(res, window, metric) {
  res[[metric]][res$window == window]
}

testthat::test_that("default windows match the programme convention", {
  w <- polished::detection_default_windows()
  testthat::expect_s3_class(w, "tbl_df")
  testthat::expect_identical(
    w$window, c("last_6", "preceding_6", "last_12", "preceding_12")
  )
  testthat::expect_identical(w$from, c(218L, 402L, 400L, 766L))
  testthat::expect_identical(w$to, c(0L, 219L, 0L, 401L))

  p <- polished::detection_default_pairs()
  testthat::expect_identical(p$current, c("last_6", "last_12"))
  testthat::expect_identical(p$previous, c("preceding_6", "preceding_12"))
})

testthat::test_that("every default window gives hand-checked counts", {
  res <- polished::calc_detection_windows(make_detw_detections(), detw_as_of)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_identical(
    res$window, c("last_6", "preceding_6", "last_12", "preceding_12")
  )
  testthat::expect_identical(res$detections, c(2L, 4L, 4L, 4L))
  testthat::expect_identical(res$reported_cases, c(1L, 2L, 2L, 3L))
  testthat::expect_identical(res$positive_es, c(1L, 2L, 2L, 1L))
  testthat::expect_identical(res$provinces, c(1L, 2L, 2L, 3L))
  testthat::expect_identical(res$clusters, c(1L, 3L, 2L, 2L))
  testthat::expect_equal(
    res$window_start, detw_as_of - c(218, 402, 400, 766)
  )
  testthat::expect_equal(res$window_end, detw_as_of - c(0, 219, 0, 401))
})

testthat::test_that("both ends of each window are included and no more", {
  one_day <- function(offset) {
    tibble::tibble(
      virus_date = detw_as_of - offset, source = "AFP", adm1 = "P1",
      virus_cluster = "C1"
    )
  }
  in_window <- function(offset) {
    res <- polished::calc_detection_windows(one_day(offset), detw_as_of)
    res$window[res$detections == 1L]
  }
  testthat::expect_identical(in_window(-1), character(0))
  testthat::expect_identical(in_window(0), c("last_6", "last_12"))
  testthat::expect_identical(in_window(218), c("last_6", "last_12"))
  testthat::expect_identical(in_window(219), c("preceding_6", "last_12"))
  testthat::expect_identical(in_window(400), c("preceding_6", "last_12"))
  testthat::expect_identical(in_window(401), c("preceding_6", "preceding_12"))
  testthat::expect_identical(in_window(402), c("preceding_6", "preceding_12"))
  testthat::expect_identical(in_window(403), "preceding_12")
  testthat::expect_identical(in_window(766), "preceding_12")
  testthat::expect_identical(in_window(767), character(0))
})

testthat::test_that("counting by country completes every window with zeros", {
  d <- make_detw_detections()
  res <- polished::calc_detection_windows(d, detw_as_of, level = "adm0")
  testthat::expect_identical(names(res)[1:3], c("level", "guid", "name"))
  testthat::expect_identical(unique(res$level), "adm0")
  pak <- dplyr::filter(res, guid == "G-PAK")
  testthat::expect_identical(unique(pak$name), "PAK")
  testthat::expect_identical(pak$detections, c(1L, 0L, 1L, 1L))
  testthat::expect_identical(pak$provinces, c(1L, 0L, 1L, 1L))
  testthat::expect_identical(nrow(res), 8L)

  total <- polished::calc_detection_windows(d, detw_as_of)
  testthat::expect_false(any(c("level", "guid", "name") %in% names(total)))
  summed <- dplyr::summarise(
    res, detections = sum(detections), .by = "window"
  )
  testthat::expect_identical(summed$detections, total$detections)
})

testthat::test_that("counting by province leaves out rows with no province", {
  res <- polished::calc_detection_windows(
    make_detw_detections(), "2024-07-01", level = "adm1"
  )
  p2 <- dplyr::filter(res, guid == "G-P2")
  testthat::expect_identical(p2$detections, c(0L, 2L, 1L, 1L))
  testthat::expect_true(all(res$provinces <= 1L))
  testthat::expect_false(anyNA(res$guid))
})

testthat::test_that("windows, sources and columns can be changed", {
  d <- make_detw_detections() |>
    dplyr::rename(onset = "virus_date", prov = "adm1")
  res <- polished::calc_detection_windows(
    d, detw_as_of,
    windows = tibble::tibble(window = "week", from = 6, to = 0),
    env_sources = c("Environmental", "Contact"),
    cols = list(detections = list(virus_date = "onset", adm1 = "prov"))
  )
  testthat::expect_identical(res$window, "week")
  testthat::expect_identical(res$detections, 1L)

  wide <- polished::calc_detection_windows(
    d, detw_as_of,
    windows = tibble::tibble(window = "all", from = 800, to = 0),
    env_sources = c("Environmental", "Contact"),
    cols = list(detections = list(virus_date = "onset", adm1 = "prov"))
  )
  testthat::expect_identical(wide$positive_es, 4L)
  testthat::expect_identical(wide$reported_cases, 5L)
})

testthat::test_that("the comparison sets each window against the previous", {
  res <- polished::calc_detection_comparison(make_detw_detections(), detw_as_of)
  testthat::expect_identical(nrow(res), 10L)

  pick <- function(window, metric, col) {
    res[[col]][res$current_window == window & res$metric == metric]
  }
  testthat::expect_identical(pick("last_6", "detections", "current"), 2L)
  testthat::expect_identical(pick("last_6", "detections", "previous"), 4L)
  testthat::expect_identical(pick("last_6", "detections", "difference"), -2L)
  testthat::expect_equal(pick("last_6", "detections", "pct_change"), -50)
  testthat::expect_identical(pick("last_6", "detections", "direction"), "down")
  testthat::expect_identical(pick("last_12", "detections", "direction"), "flat")
  testthat::expect_equal(pick("last_12", "positive_es", "pct_change"), 100)
  testthat::expect_identical(pick("last_12", "positive_es", "direction"), "up")
  testthat::expect_equal(
    pick("last_12", "reported_cases", "pct_change"), -100 / 3
  )
  testthat::expect_identical(
    pick("last_12", "reported_cases", "direction"), "down"
  )
  testthat::expect_identical(pick("last_12", "clusters", "direction"), "flat")
})

testthat::test_that("the comparison handles zero periods and groups", {
  res <- polished::calc_detection_comparison(
    make_detw_detections(), detw_as_of,
    level = "adm0", metrics = "detections"
  )
  testthat::expect_identical(names(res)[1:3], c("level", "guid", "name"))
  pak <- dplyr::filter(res, guid == "G-PAK")
  testthat::expect_identical(pak$current, c(1L, 1L))
  testthat::expect_identical(pak$previous, c(0L, 1L))
  testthat::expect_identical(pak$pct_change, c(Inf, 0))
  testthat::expect_identical(pak$direction, c("up", "flat"))

  none <- polished::calc_detection_comparison(
    make_detw_detections(), as.Date("2010-01-01"), metrics = "detections"
  )
  testthat::expect_identical(none$pct_change, c(0, 0))
  testthat::expect_identical(none$direction, c("flat", "flat"))
})

testthat::test_that("the direction rule matches the programme convention", {
  dir <- polished::detection_direction
  testthat::expect_identical(dir(0, 0), "flat")
  testthat::expect_identical(dir(1, 0), "up")
  testthat::expect_identical(dir(10, 0), "up")
  testthat::expect_identical(dir(0, 1), "down")
  testthat::expect_identical(dir(4, 2), "up")
  testthat::expect_identical(dir(2, 4), "down")
  # 15 percent either way is within the threshold
  testthat::expect_identical(dir(23, 20), "flat")
  testthat::expect_identical(dir(17, 20), "flat")
  testthat::expect_identical(dir(115, 100), "flat")
  testthat::expect_identical(dir(85, 100), "flat")
  # just past it either way
  testthat::expect_identical(dir(24, 20), "up")
  testthat::expect_identical(dir(16, 20), "down")
  testthat::expect_identical(dir(116, 100), "up")
  testthat::expect_identical(dir(84, 100), "down")
  testthat::expect_identical(dir(11, 10, threshold = 5), "up")
  testthat::expect_identical(dir(11, 10, threshold = 10), "flat")
  testthat::expect_identical(dir(c(NA, 1), 1), c(NA, "flat"))
  testthat::expect_identical(dir(c(0, 5, 30), 20), c("down", "down", "up"))
})

testthat::test_that("the slice keeps the detections that count", {
  virus <- tibble::tibble(
    epid = c("A-1", "A-1", "A-1", "A-2", "A-3", "A-4", "A-5", "A-6"),
    vtype = c("WPV 1", "WPV 1", "cVDPV 2", "WILD1", "Sabin 1", "WPV 1",
              "aVDPV 2", "WPV 1"),
    country_iso3code = c("AFG", "AFG", "AFG", "PAK", "AFG", "NGA", "AFG",
                         NA),
    source = c("AFP", "AFP", "AFP", "Environmental", "AFP", "AFP",
               "Contact", "AFP"),
    virus_date = as.Date("2024-01-01") + c(5, 5, 5, 1, 2, 3, 4, 0),
    updated = as.Date("2024-03-01") + c(0, 9, 0, 0, 0, 0, 0, 0),
    adm1 = c("old", "new", "x", "y", "z", "w", "v", "u")
  )

  res <- polished::detection_slice(virus)
  testthat::expect_identical(nrow(res), 4L)
  testthat::expect_identical(res$epid, c("A-2", "A-5", "A-1", "A-1"))
  testthat::expect_identical(res$vtype, c("WPV1", "aVDPV2", "WPV1", "cVDPV2"))
  testthat::expect_identical(res$adm1[res$vtype == "WPV1" & res$epid == "A-1"],
                             "old")

  latest <- polished::detection_slice(virus, latest_by = "updated")
  testthat::expect_identical(
    latest$adm1[latest$vtype == "WPV1" & latest$epid == "A-1"], "new"
  )

  wpv1 <- polished::detection_slice(
    virus, countries = "AFG", vtype_pattern = "^WPV1$", key = "epid"
  )
  testthat::expect_identical(wpv1$epid, "A-1")

  human <- polished::detection_slice(virus, sources = c("AFP", "Contact"))
  testthat::expect_false("A-2" %in% human$epid)

  anywhere <- polished::detection_slice(virus, countries = NULL)
  testthat::expect_true("A-4" %in% anywhere$epid)
  testthat::expect_false("A-6" %in% anywhere$epid)
})

testthat::test_that("the slice reads raw POLIS names through cols", {
  raw <- tibble::tibble(
    EPID = c("A-1", "A-2"),
    VirusTypeName = c("WILD1", "cVDPV2"),
    CountryISO3Code = c("AFG", "IRN"),
    VirusDate = as.Date(c("2024-01-01", "2024-01-02"))
  )
  res <- polished::detection_slice(
    raw,
    key = c("EPID", "VirusTypeName"),
    cols = list(detections = list(
      epid = "EPID", vtype = "VirusTypeName", country = "CountryISO3Code",
      virus_date = "VirusDate"
    ))
  )
  testthat::expect_identical(res$EPID, "A-1")
  testthat::expect_identical(res$VirusTypeName, "WPV1")
})

testthat::test_that("arguments are validated", {
  d <- make_detw_detections()
  calc <- polished::calc_detection_windows
  testthat::expect_error(calc(list(), detw_as_of), "data.frame")
  testthat::expect_error(calc(d, "not a date"), "single date")
  testthat::expect_error(calc(d, detw_as_of + 0:1), "single date")
  testthat::expect_error(
    calc(d, detw_as_of, windows = tibble::tibble(window = "a", from = 1)),
    "missing from"
  )
  testthat::expect_error(
    calc(d, detw_as_of,
         windows = tibble::tibble(window = "a", from = 1, to = 5)),
    "at least"
  )
  testthat::expect_error(
    calc(d, detw_as_of,
         windows = tibble::tibble(window = "a", from = -1, to = -5)),
    "whole numbers"
  )
  testthat::expect_error(
    calc(d, detw_as_of,
         windows = tibble::tibble(window = c("a", "a"), from = 2, to = 1)),
    "once"
  )
  testthat::expect_error(
    calc(d, detw_as_of, cols = list(onset = "virus_date")), "Unknown source"
  )
  testthat::expect_error(
    calc(d, detw_as_of, cols = list(detections = list(onset = "virus_date"))),
    "unknown role"
  )
  testthat::expect_error(
    calc(d, detw_as_of, cols = list(detections = list(adm1 = "adm9"))),
    "missing from"
  )
  testthat::expect_error(calc(d, detw_as_of, level = "adm3"), "level")
  testthat::expect_error(calc(d, detw_as_of, level = "adm2"), "missing from")
  testthat::expect_error(calc(d, detw_as_of, env_sources = 1), "character")

  comp <- polished::calc_detection_comparison
  testthat::expect_error(comp(d, detw_as_of, threshold = -1), "threshold")
  testthat::expect_error(comp(d, detw_as_of, threshold = NA), "threshold")
  testthat::expect_error(comp(d, detw_as_of, metrics = "cases"), "metrics")
  testthat::expect_error(
    comp(d, detw_as_of,
         pairs = tibble::tibble(current = "last_6", previous = "last_9")),
    "last_9"
  )

  dir <- polished::detection_direction
  testthat::expect_error(dir("1", 1), "numeric")
  testthat::expect_error(dir(1:3, 1:2), "same length")
  testthat::expect_error(dir(1, 1, threshold = c(1, 2)), "threshold")

  slice <- polished::detection_slice
  testthat::expect_error(slice("x"), "data.frame")
  testthat::expect_error(slice(d), "missing from")
  testthat::expect_error(
    slice(dplyr::mutate(d, vtype = "WPV1"), vtype_pattern = c("a", "b")),
    "regular expression"
  )
  testthat::expect_error(
    slice(dplyr::mutate(d, vtype = "WPV1"), countries = 1), "countries"
  )
  testthat::expect_error(
    slice(dplyr::mutate(d, vtype = "WPV1"), key = character(0)), "key"
  )
})
