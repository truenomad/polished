# Parallel-dispatch machinery. The cluster-lifecycle helpers are tested against
# real PSOCK clusters (deterministic, offline) but gated off CRAN/CI because they
# spawn worker processes. The full dispatch + refetch run real HTTP in fresh
# worker sessions, so they are covered by a gated live integration test.

testthat::test_that("cluster helpers capture worker pids, stop cleanly, and force-kill survivors", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if(
    .Platform$OS.type == "windows",
    "the pskill liveness/force-kill path is POSIX-only"
  )

  cl <- parallel::makePSOCKcluster(1L)
  pids <- polished:::.polis_cluster_pids(cl)
  testthat::expect_true(length(pids) >= 1L && all(pids > 0))

  # clean teardown: the worker exits, the happy path is silent
  testthat::expect_silent(polished:::.polis_stop_cluster(cl, pids))
  # stopping an already-stopped cluster is tolerated (invalid-connection swallowed)
  testthat::expect_no_error(polished:::.polis_stop_cluster(cl, integer(0)))

  # force-kill branch: pass a still-alive worker pid alongside a different cluster
  victim <- parallel::makePSOCKcluster(1L)
  victim_pid <- polished:::.polis_cluster_pids(victim)
  spare <- parallel::makePSOCKcluster(1L)
  suppressMessages(polished:::.polis_stop_cluster(spare, victim_pid))
  Sys.sleep(0.3)
  testthat::expect_false(isTRUE(tryCatch(
    tools::pskill(victim_pid, 0L),
    error = function(e) FALSE
  )))
  try(parallel::stopCluster(victim), silent = TRUE)
})

testthat::test_that("get_polis_data parallel pull works end-to-end (live, workers = 2)", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  # PSOCK workers run in separate, un-instrumented processes; under covr the run
  # adds no coverage and can hang the instrumented session, so skip it there.
  testthat::skip_if(
    as.logical(Sys.getenv("R_COVR", "false")),
    "PSOCK workers are not instrumented under covr"
  )
  testthat::skip_if_offline()
  key <- Sys.getenv("POLIS_API_KEY")
  testthat::skip_if(!nzchar(key), "POLIS_API_KEY not set")
  testthat::skip_if(
    length(find.package("polished", quiet = TRUE)) == 0L,
    "polished must be installed for PSOCK workers"
  )

  root <- withr::local_tempdir()
  tryCatch(
    polished::get_polis_data(
      tables = "im",
      min_date = "2022-01-01",
      polis_folder = root,
      polis_api_key = key,
      workers = 2L,
      auto_refetch = FALSE,
      quiet = TRUE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (
        grepl("timed out|resolve|HTTP|curl|installed", msg, ignore.case = TRUE)
      ) {
        testthat::skip(paste("POLIS/worker unavailable:", msg))
      }
      stop(e)
    }
  )
  df <- readRDS(file.path(root, "im.rds"))
  testthat::expect_true("Id" %in% names(df))
  testthat::expect_false(any(duplicated(df$Id)))
})
