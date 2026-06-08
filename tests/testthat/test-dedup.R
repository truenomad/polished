# polis_upsert + flag_ambiguous

testthat::test_that("polis_upsert keeps the latest row per id", {
  df <- tibble::tibble(
    id = c(1, 1, 2),
    last_update_date = c("2024-01-01", "2024-03-01", "2024-02-01"),
    value = c("old", "new", "x")
  )
  out <- polished::polis_upsert(df, id = "id", date = "last_update_date")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$value[out$id == 1], "new")
})

testthat::test_that("polis_upsert collapses exact duplicates at a grain", {
  df <- tibble::tibble(
    id = c(1, 2),
    last_update_date = c("2024-03-01", "2024-02-01"),
    v = c("a", "b")
  )
  out <- polished::polis_upsert(
    rbind(df, df),
    id = "id",
    date = "last_update_date",
    grain = c("id", "v")
  )
  testthat::expect_equal(nrow(out), 2L)
})

testthat::test_that("polis_upsert falls back to exact dedup without an id", {
  df <- tibble::tibble(a = c(1, 1, 2), b = c("x", "x", "y"))
  testthat::expect_warning(
    out <- polished::polis_upsert(df, id = "id"),
    "No"
  )
  testthat::expect_equal(nrow(out), 2L)
})

testthat::test_that("flag_ambiguous flags but never drops rows", {
  df <- tibble::tibble(
    id = c(1, 2, 3),
    epid = c("A", "A", "B"),
    adm0 = c("X", "X", "Y")
  )
  out <- suppressMessages(polished::flag_ambiguous(df, key = c("epid", "adm0")))
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_equal(nrow(attr(out, "polis_ambiguous")), 2L)
})
