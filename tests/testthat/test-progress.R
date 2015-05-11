
context("Progress bar")

test_that("Vanilla progress bar works", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    for (i in 1:5) {
      pb$tick(20)
    }
  })

  sout <- paste0(
    "\r[===----------]  20%",
    "\r[=====--------]  40%",
    "\r[========-----]  60%",
    "\r[==========---]  80%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})

test_that("Calling tick(0)", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    pb$tick(0)
    for (i in 1:5) {
      pb$tick(20)
    }
  })

  sout <- paste0(
    "\r[-------------]   0%",
    "\r[===----------]  20%",
    "\r[=====--------]  40%",
    "\r[========-----]  60%",
    "\r[==========---]  80%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})

test_that("Digress", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    f <- function() {
      pb$tick(50)
      pb$tick(-20)
      pb$tick(50)
      pb$tick(-30)
      pb$tick(100)
    }
    f()
  })

  sout <- paste0(
    "\r[======-------]  50%",
    "\r[====---------]  30%",
    "\r[==========---]  80%",
    "\r[======-------]  50%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)
})

test_that("No :bar item", {

  ## TODO

})

test_that(":current and :total tokens", {

  ## TODO

})

test_that(":elapsed token", {

  ## TODO

})

test_that(":eta token", {

  ## TODO

})

test_that(":rate and :bytes tokens", {

  ## TODO

})

test_that("complete and incomplete chars", {

  ## TODO

})

test_that("callback function", {

  ## TODO

})

test_that("clearing and not clearing", {

  ## TODO

})

test_that("show_after argument", {

  ## TODO

})

test_that("custom tokens", {

  ## TODO

})
