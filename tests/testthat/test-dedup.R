# polis_upsert + flag_ambiguous

test_that("polis_upsert keeps the latest row per id", {
  df <- tibble::tibble(
    id = c(1, 1, 2),
    last_update_date = c("2024-01-01", "2024-03-01", "2024-02-01"),
    value = c("old", "new", "x")
  )
  out <- polis_upsert(df, id = "id", date = "last_update_date")
  expect_equal(nrow(out), 2L)
  expect_equal(out$value[out$id == 1], "new")
})

test_that("polis_upsert collapses exact duplicates at a grain", {
  df <- tibble::tibble(
    id = c(1, 2),
    last_update_date = c("2024-03-01", "2024-02-01"),
    v = c("a", "b")
  )
  out <- polis_upsert(
    rbind(df, df),
    id = "id",
    date = "last_update_date",
    grain = c("id", "v")
  )
  expect_equal(nrow(out), 2L)
})

test_that("polis_upsert falls back to exact dedup without an id column", {
  df <- tibble::tibble(a = c(1, 1, 2), b = c("x", "x", "y"))
  expect_warning(out <- polis_upsert(df, id = "id"), "No")
  expect_equal(nrow(out), 2L)
})

test_that("flag_ambiguous flags but never drops rows", {
  df <- tibble::tibble(
    id = c(1, 2, 3),
    epid = c("A", "A", "B"),
    adm0 = c("X", "X", "Y")
  )
  out <- suppressMessages(flag_ambiguous(df, key = c("epid", "adm0")))
  expect_equal(nrow(out), 3L)
  expect_equal(nrow(attr(out, "polis_ambiguous")), 2L)
})
