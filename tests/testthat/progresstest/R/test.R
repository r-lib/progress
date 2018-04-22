
#' @importFrom progress progress_bar
#' @importFrom Rcpp loadRcppModules
#' @useDynLib progresstest
#' @export

my_test_progress <- function(format = "[:bar] :percent ") {
  test_progress(format)
}

#' @export

my_is_option_enabled <- function() {
  test_is_option_enabled()
}
