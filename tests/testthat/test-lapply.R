
context("Lapply")

test_that("simple lapply", {

  res <- progress %~~% lapply(1:100, function(x) { Sys.sleep(.1) ; 2 * x})

})

test_that("lapply with extra arguments", {

  flag <- TRUE
  f <- function(x, flag) { Sys.sleep(.1); x + flag }
  res <- progress %~~% lapply(1:100, f, flag = flag)
  
})

