
#' @useDynLib progresstest2, .registration = TRUE, .fixes = "c_"
NULL

#' @export

test0 <- function() {
  .Call(c_test0)
}

#' @export

test1 <- function() {
  .Call(c_test1)
}
