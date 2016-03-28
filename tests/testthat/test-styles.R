
context("Predefined styles")

test_that("spinnerbar", {

  pb <- progress_bar$new(format = ":spinnerbar")
  for (i in 1:100) { Sys.sleep(interactive() / 10); pb$tick() }

})
