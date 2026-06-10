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

testthat::test_that("upsert/flag_ambiguous/remap_synonyms/reconcile cover their branches", {
  # polis_upsert: empty frame returned as-is; no date column -> keep-latest by key
  testthat::expect_identical(nrow(polished::polis_upsert(data.frame())), 0L)
  nodate <- data.frame(
    id = c(1, 1, 2),
    v = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(nrow(polished::polis_upsert(nodate)), 2L)

  # flag_ambiguous writes a sink when one business key spans >1 id
  amb <- data.frame(
    id = c(1, 2),
    epid = c("E", "E"),
    adm0 = c("X", "X"),
    stringsAsFactors = FALSE
  )
  sink <- file.path(withr::local_tempdir(), "amb.csv")
  out <- polished::flag_ambiguous(amb, key = c("epid", "adm0"), sink = sink)
  testthat::expect_true(file.exists(sink))
  testthat::expect_s3_class(attr(out, "polis_ambiguous"), "data.frame")

  # remap_synonyms: bad table warns + no-op; a valid table rewrites the epid
  testthat::expect_warning(
    polished::remap_synonyms(
      data.frame(epid = "A-1"),
      synonyms = data.frame(x = 1)
    ),
    "needs columns"
  )
  remapped <- polished::remap_synonyms(
    data.frame(epid = c("OLD", "B"), stringsAsFactors = FALSE),
    synonyms = data.frame(
      epid = "OLD",
      canonical_epid = "NEW",
      stringsAsFactors = FALSE
    )
  )
  testthat::expect_identical(remapped$epid, c("NEW", "B"))

  # reconcile: no shared id column warns; otherwise stale ids are pruned
  testthat::expect_warning(
    polished::reconcile(data.frame(x = 1), data.frame(id = 1)),
    "skipping reconcile"
  )
  testthat::expect_equal(
    nrow(polished::reconcile(
      data.frame(id = c(1, 2, 3)),
      data.frame(id = c(1, 2))
    )),
    2L
  )
})
