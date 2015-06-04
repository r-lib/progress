
context("C++ API")

test_that("C++ API works", {

  skip_on_cran()

  Sys.setenv("R_TESTS" = "")

  ## Need to "link to" the current package
  test_pkg_dir <- system.file("progresstest", package = "progress")

  f <- file(tempfile(), open = "w")
  sink(f)
  sink(f, type = "message")
  install.packages("Rcpp", repos = c(CRAN = "http://cran.rstudio.com"),
                   quiet = TRUE)
  sink(NULL, type = "message")
  sink(NULL)
  close(f)
  unlink(f)

  ## OK, we could install it
  try(silent = TRUE, unloadNamespace("progresstest"))
  install.packages(test_pkg_dir, repos = NULL, type = "source",
                   quiet = TRUE)

  ## OK, we could load it
  do.call("library", list("progresstest", character.only = TRUE))

  f <- file(tempfile(), open = "w")
  sink(f)
  sink(f, type = "message")
  my_test_progress()
  sink(NULL, type = "message")
  sink(NULL)
  close(f)
  unlink(f)

  try(silent = TRUE, unloadNamespace("progresstest"))

})
