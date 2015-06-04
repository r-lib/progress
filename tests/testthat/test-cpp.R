
context("C++ API with Rcpp")

test_that("C++ API works", {

  ## Need to "link to" the current package
  inc_dir <- system.file("include", package = "progress")
  test_pkg_dir <- system.file("progresstest", package = "progress")

  ## OK, we could install it
  R <- file.path(R.home("bin"), "R")
  install.packages(test_pkg_dir, repos = NULL,
                   type = "source", quiet = TRUE)
  expect_true(TRUE)

  ## OK, we could load it
  on.exit(try(silent = TRUE, unloadNamespace("progresstest")), add = TRUE)
  library(progresstest)
  expect_true(TRUE)

})
