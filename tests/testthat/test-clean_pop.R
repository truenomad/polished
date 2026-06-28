# Fixtures -------------------------------------------------------------------

# A 3-district attribute "shape": braced upper-case guids (as the processed
# spatial layer carries them), parents, and wide-open validity windows.
pop_shape <- function() {
  tibble::tibble(
    who_region = "EMRO",
    iso_3_code = "AFG",
    adm0 = "AFGHANISTAN",
    adm0_guid = "{A0}",
    adm1 = "PROVINCE 1",
    adm1_guid = "{A1}",
    adm2 = c("DISTRICT A", "DISTRICT B", "DISTRICT C"),
    adm2_guid = c("{G1}", "{G2}", "{G3}"),
    startdate = as.Date("2015-01-01"),
    enddate = as.Date("2030-01-01")
  )
}

# Raw POLIS population: bare lower-case PlaceId, the three AgeGroupNames, with
# a conflicting dup (g1 u15 2020), a zero (g2 u15 2020), an implausible value
# (g2 u15 2021), and an orphan guid g9 whose name resolves to DISTRICT C (g3).
pop_raw <- function() {
  rows <- function(id, nm, u15, u5, all, yr) {
    tibble::tibble(
      PlaceId = id,
      PlaceDisplayName = nm,
      Year = yr,
      AgeGroupName = c("0 to 15 years", "0 to 5 years", "All ages"),
      Value = c(u15, u5, all)
    )
  }
  dplyr::bind_rows(
    rows("g1", "DISTRICT A", 1000, 350, 2200, 2020),
    rows("g1", "DISTRICT A", 1200, 350, 2200, 2020), # conflicting u15 dup
    rows("g1", "DISTRICT A", 1100, 360, 2250, 2021),
    rows("g2", "DISTRICT B", 0, 320, 2100, 2020), # zero u15
    rows("g2", "DISTRICT B", 5, 330, 2150, 2021), # implausible u15
    rows("g9", "DISTRICT C", 980, 340, 2050, 2020), # orphan -> g3
    rows("g9", "DISTRICT C", 1010, 345, 2080, 2021)
  )
}

# Pre-extracted WorldPop: one tidy adm2 x year table per age band.
pop_worldpop <- function() {
  grid <- tidyr::crossing(
    adm2_guid = c("{G1}", "{G2}", "{G3}"),
    year = 2020:2021
  )
  list(
    u15 = dplyr::mutate(grid, u15_pop = 900L),
    u5 = dplyr::mutate(grid, u5_pop = 300L),
    all = dplyr::mutate(grid, all_pop = 2000L)
  )
}

cell <- function(df, guid, yr) {
  df[df$adm2_guid == guid & df$year == yr, , drop = FALSE]
}

# WorldPop path -------------------------------------------------------------

test_that("clean_pop reconciles POLIS against WorldPop", {
  res <- clean_pop(
    pop_raw(),
    shape = pop_shape(),
    worldpop = pop_worldpop(),
    years = 2020:2021
  )

  expect_named(res, c("adm0", "adm1", "adm2", "meta"))
  # ISO column standardised to the package convention
  expect_true("country_iso3code" %in% names(res$adm2))
  expect_false("iso_3_code" %in% names(res$adm2))

  a2 <- res$adm2

  # conflicting dup collapsed to the median (1000, 1200 -> 1100); trusted POLIS
  g1 <- cell(a2, "{G1}", 2020)
  expect_equal(g1$u15_pop, 1100L)
  expect_equal(g1$u15_pop_source, "polis")

  # zero -> missing -> WorldPop fallback
  g2_20 <- cell(a2, "{G2}", 2020)
  expect_equal(g2_20$u15_pop, 900L)
  expect_equal(g2_20$u15_pop_source, "worldpop")
  expect_true(g2_20$u15_pop_imputed)

  # implausible value (5 vs 900) -> suspect -> WorldPop
  g2_21 <- cell(a2, "{G2}", 2021)
  expect_equal(g2_21$u15_pop, 900L)
  expect_equal(g2_21$u15_pop_source, "worldpop")

  # orphan guid g9 remapped onto DISTRICT C (g3): its value is used
  g3 <- cell(a2, "{G3}", 2020)
  expect_equal(g3$u15_pop, 980L)
  expect_equal(g3$u15_pop_source, "polis")
})

test_that("clean_pop rolls up to adm1/adm0 by summing valid districts", {
  res <- clean_pop(
    pop_raw(),
    shape = pop_shape(),
    worldpop = pop_worldpop(),
    years = 2020:2021
  )
  d0 <- res$adm0[res$adm0$year == 2020, ]
  # 1100 (g1) + 900 (g2, worldpop) + 980 (g3) = 2980
  expect_equal(d0$u15_pop, 2980L)
  expect_equal(nrow(res$adm1), 2L) # one province x two years
})

test_that("clean_pop surfaces dup conflicts and orphan resolution in meta", {
  res <- clean_pop(
    pop_raw(),
    shape = pop_shape(),
    worldpop = pop_worldpop(),
    years = 2020:2021
  )
  expect_true(any(res$meta$dup_conflicts$adm2_guid == "{G1}"))
  orph <- res$meta$orphan_xwalk
  g9 <- orph[orph$polis_guid == "{G9}", ]
  expect_equal(g9$xwalk_status, "resolved")
  expect_equal(g9$current_guid, "{G3}")
})

# POLIS-only path (no WorldPop) ---------------------------------------------

test_that("without WorldPop, gaps fall back to the district trend and are flagged", {
  res <- clean_pop(pop_raw(), shape = pop_shape(), years = 2020:2021)
  g2_20 <- cell(res$adm2, "{G2}", 2020)
  # g2 u15: 2020 is zero (missing); its only positive history is 2021 = 5
  expect_equal(g2_20$u15_pop_source, "district_trend")
  expect_true(g2_20$u15_pop_imputed)
  expect_equal(g2_20$u15_pop, 5L)
})

test_that("checks_pop returns a workbook-ready summary", {
  res <- clean_pop(
    pop_raw(),
    shape = pop_shape(),
    worldpop = pop_worldpop(),
    years = 2020:2021
  )
  ck <- checks_pop(res)
  expect_true(is.data.frame(ck$summary))
  expect_true(all(
    c("check", "severity", "n_flagged", "description") %in% names(ck$summary)
  ))
  # the conflicting-dup check counts the g1 row
  cd <- ck$summary[ck$summary$check == "conflicting_dups", ]
  expect_gte(cd$n_flagged, 1L)
})

# Pipeline integration -------------------------------------------------------

test_that("run_pipeline produces a global pop stream from a population input", {
  cfg <- polis_config(
    regions = "EMRO",
    shape = pop_shape(),
    worldpop = pop_worldpop(),
    pop_years = 2020:2021
  )
  out <- run_pipeline(inputs = list(population = pop_raw()), cfg = cfg)
  expect_true("pop" %in% names(out))
  expect_s3_class(out$pop$adm2, "data.frame")
  # foundational: every shape district is present, not region-filtered away
  expect_setequal(unique(out$pop$adm2$adm2_guid), c("{G1}", "{G2}", "{G3}"))
})

# Degraded: no shape ---------------------------------------------------------

test_that("clean_pop works POLIS-only with no shape (guid-keyed, no rollups)", {
  res <- clean_pop(pop_raw(), years = 2020:2021)
  expect_true("adm2_guid" %in% names(res$adm2))
  expect_null(res$adm1)
  expect_null(res$adm0)
})
