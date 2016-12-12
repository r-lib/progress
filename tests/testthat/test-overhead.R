
context("Overhead")

library(microbenchmark)

test_that("overhead for short and fast loop", {

  skip_on_cran()

  sink(tempfile())

  oh <- microbenchmark(
    without = { for (i in 1:100) { x <- sqrt(5) } },
    with = {
      pb <- progress_bar$new(total = 100, stream = stdout(),
                             force = TRUE, width = 20)
      for (i in 1:100) {
        x <- sqrt(5)
        pb$tick()
      }
    }
  )

  sink(NULL)

  cat("\rOverhead, short and fast loop\n"); print(oh); cat("\n")
})

test_that("overhead for long and fast loop", {

  skip_on_cran()

  sink(tempfile())

  oh <- microbenchmark(
    without = { for (i in 1:10000) { x <- sqrt(5) } },
    with = {
      pb <- progress_bar$new(total = 100, stream = stdout(),
                             force = TRUE, width = 20)
      for (i in 1:10000) {
        x <- sqrt(5)
        if (! i %% 100) pb$tick()
      }
    }
  )

  sink(NULL)

  cat("\rOverhead, long and fast loop\n"); print(oh); cat("\n")
})

test_that("overhead for short and slow(er) loop", {

  skip_on_cran()

  sink(tempfile())

  oh <- microbenchmark(
    times = 10,
    without = { for (i in 1:100) { Sys.sleep(0.001) } },
    with = {
      pb <- progress_bar$new(total = 100, stream = stdout(),
                             force = TRUE, width = 20)
      for (i in 1:100) {
        Sys.sleep(0.001)
        pb$tick()
      }
    }
  )

  sink(NULL)

  cat("\rOverhead, short and slow loop\n"); print(oh); cat("\n")
})
