
f <- function() {

  ## Need to "link to" the current package
  test_pkg_dir <- system.file("progresstest", package = "progress")

  options(repos = structure(c(CRAN = "http://cran.rstudio.com")))
  install.packages("Rcpp")

  ## OK, we could install it
  try(silent = TRUE, unloadNamespace("progresstest"))
  install.packages(test_pkg_dir, repos = NULL, type = "source")

  ## OK, we could load it
  do.call("library", list("progresstest", character.only = TRUE))
  try(silent = TRUE, unloadNamespace("progresstest"))

}

f()
