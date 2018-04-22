
context("C++ API")

test_that("C++ API works", {

  skip_on_cran()

  Sys.setenv("R_TESTS" = "")

  ## Need to "link to" the current package
  test_pkg_dir <- system.file("progresstest", package = "progress")

  f <- file(tempfile(), open = "w")
  sink(f)
  sink(f, type = "message")
  on.exit({ sink(NULL, type = "message"); sink(NULL)})
  suppressWarnings(
    install.packages("Rcpp", repos = c(CRAN = "https://cran.rstudio.com"),
                     quiet = TRUE)
  )

  sink(NULL, type = "message"); sink(NULL)
  on.exit(NULL)
  close(f)
  unlink(f)

  ## OK, we could install it
  try(silent = TRUE, unloadNamespace("progresstest"))
  install.packages(test_pkg_dir, repos = NULL, type = "source",
                   quiet = TRUE)

  ## OK, we could load it
  expect_error(
    do.call("library", list("progresstest", character.only = TRUE)), NA)
  on.exit(unloadNamespace("progresstest"))

  f <- file(tempfile(), open = "w")
  sink(f)
  sink(f, type = "message")
  expect_error(my_test_progress(), NA)
  sink(NULL, type = "message")
  sink(NULL)
  close(f)
  unlink(f)

  try(silent = TRUE, unloadNamespace("progresstest"))

})
