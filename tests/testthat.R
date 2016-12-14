
if (Sys.getenv("NOT_CRAN", "") != "") {
  library(testthat)
  library(progress)
  test_check("progress")
}
