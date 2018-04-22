
context("C++ API")

test_that("C++ API works", {

  skip_on_cran()

  Sys.setenv("R_TESTS" = "")

  dir.create(lib <- tempfile())
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  install.packages("progresstest_1.0.0.tar.gz", lib = lib, quiet = TRUE)

  on.exit(unloadNamespace("progresstest"), add = TRUE)
  withr::with_libpaths(lib, action = "prefix", {
    withr::with_message_sink(
      file.path(lib, basename(tempfile())),
      expect_error(progresstest::my_test_progress(), NA)
    )
  })

  withr::with_libpaths(lib, action = "prefix", {
    withr::with_options(list(progress_enabled = FALSE),
      expect_false(progresstest::my_is_option_enabled())
    )
  })
})
