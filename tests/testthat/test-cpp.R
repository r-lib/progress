
context("C++ API with Rcpp")

test_that("C++ API works", {

  ## Need to "link to" the current package
  inc_dir <- system.file("include", package = "progress")
  test_pkg_dir <- system.file("progresstest", package = "progress")

  tmp_lib <- tempfile()
  old_libs <- .libPaths()
  
  on.exit(unlink(tmp_lib, recursive = TRUE), add = TRUE)
  on.exit(.libPaths(old_libs), add = TRUE)

  dir.create(tmp_lib)
  .libPaths(tmp_lib)

  ## OK, we could install it
  R <- R.home("bin/R")
  system(paste(R, "CMD INSTALL", "-l", tmp_lib, test_pkg_dir),
         intern = TRUE, ignore.stderr = TRUE)
  expect_true(TRUE)

  ## OK, we could load it
  on.exit(unloadNamespace("progresstest"), add = TRUE)
  library(progresstest)
  expect_true(TRUE)

  ## OK, we can even use it
  on.exit(sink(NULL, type = "message"), add = TRUE)
  on.exit(if (con) { close(con) }, add = TRUE)
  con <- FALSE
  out_file <- tempfile()
  con <- file(out_file, open = "w")
  sink(con, type = "message")
  progresstest::my_test_progress()
  out <- readChar(out_file, nchars = file.info(out_file)$size)
  expect_true(grepl("----", out))
  expect_true(grepl("====", out))

})
