
oprint <- function(tagline, results) {
  cat(tagline, "\n", sep = "")
  cat(paste(rep("-", getOption("width")), collapse = ""), "\n", sep = "")
  out <- capture.output(print(results))
  cat(paste0("  ", out), sep = "\n")
  cat("\n")
}

overhead <- function() {

  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("The 'microbenchmark' package is needed for overhead tests")
  }


  oprint(
    "Non-interactive, constructor separate, short (10), fast",
    overhead_noninteractive_create_short_fast())
  oprint(
    "Non-interactive, short (10), fast",
    overhead_noninteractive_short_fast())
  oprint(
    "Non-interactive, long (10000), fast",
    overhead_noninteractive_long_fast())
  oprint(
    "Non-interactive, short (10), slow",
    overhead_noninteractive_short_slow())

  oprint(
    "Constructore separate, short (10), fast",
    overhead_create_short_fast())
  oprint(
    "Short (10), fast",
    overhead_short_fast())
  oprint(
    "Long (10000), fast",
    overhead_long_fast())
  oprint(
    "Short (10), slow",
    overhead_short_slow())
}

overhead_noninteractive_create_short_fast <- function() {
  testthat::with_mock(
    `progress::is_supported` = function(...) FALSE,
    oh <- microbenchmark::microbenchmark(
      without = { for (i in 1:10) { } },
      create = {
        pb <<- progress_bar$new(total = 10, stream = stdout(), width = 80)
      },
      run = {
        for (i in 1:10) pb$tick()
      }
    )
  )

  oh
}

overhead_noninteractive_short_fast <- function() {

  testthat::with_mock(
    `progress::is_supported` = function(...) FALSE,
    oh <- microbenchmark::microbenchmark(
      without = { for (i in 1:10) { } },
      with = {
        pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
        for (i in 1:10) pb$tick()
      }
    )
  )

  oh
}

overhead_noninteractive_long_fast <- function() {

  testthat::with_mock(
    `progress::is_supported` = function(...) FALSE,
    oh <- microbenchmark::microbenchmark(
      times = 10,
      without = { for (i in 1:10000) { } },
      with = {
        pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
        for (i in 1:10000) pb$tick()
      }
    )
  )

  oh
}

overhead_noninteractive_short_slow <- function() {

  testthat::with_mock(
    `progress::is_supported` = function(...) FALSE,
    oh <- microbenchmark::microbenchmark(
      times = 10,
      without = { for (i in 1:10) { Sys.sleep(0.01) } },
      with = {
        pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
        for (i in 1:10) { Sys.sleep(0.01); pb$tick() }
      }
    )
  )

  oh
}

overhead_create_short_fast <- function() {

  sink(tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  on.exit(sink(NULL), add = TRUE)

  oh <- microbenchmark::microbenchmark(
    times = 10,
    without = { for (i in 1:10) { } },
    create = {
      pb <<- progress_bar$new(total = 10, stream = stdout(), width = 80)
    },
    run = {
      for (i in 1:10) pb$tick()
    }
  )

  oh
}

overhead_short_fast <- function() {

  sink(tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  on.exit(sink(NULL), add = TRUE)

  oh <- microbenchmark::microbenchmark(
    times = 10,
    without = { for (i in 1:10) { } },
    with = {
      pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
      for (i in 1:10) pb$tick()
    }
  )

  oh
}

overhead_long_fast <- function() {

  sink(tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  on.exit(sink(NULL), add = TRUE)

  oh <- microbenchmark::microbenchmark(
    times = 10,
    without = { for (i in 1:10000) { } },
    with = {
      pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
      for (i in 1:10000) pb$tick()
    }
  )

  oh
}

overhead_short_slow <- function() {

  sink(tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  on.exit(sink(NULL), add = TRUE)

  oh <- microbenchmark::microbenchmark(
    times = 10,
    without = { for (i in 1:10) { Sys.sleep(0.01) } },
    with = {
      pb <- progress_bar$new(total = 10, stream = stdout(), width = 80)
      for (i in 1:10) { Sys.sleep(0.01); pb$tick() }
    }
  )

  oh
}
