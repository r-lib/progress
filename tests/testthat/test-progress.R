
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
    "\r[==>----------]  20%",
    "\r[====>--------]  40%",
    "\r[=======>-----]  60%",
    "\r[=========>---]  80%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})

test_that("Update method works", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    updates = c(20, 40, 20, 80, 100) / 100
    for (i in 1:5) {
      pb$update(updates[i])
    }
  })

  sout <- paste0(
    "\r[==>----------]  20%",
    "\r[====>--------]  40%",
    "\r[==>----------]  20%",
    "\r[=========>---]  80%",
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
    "\r[==>----------]  20%",
    "\r[====>--------]  40%",
    "\r[=======>-----]  60%",
    "\r[=========>---]  80%",
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
    "\r[=====>-------]  50%",
    "\r[===>---------]  30%",
    "\r[=========>---]  80%",
    "\r[=====>-------]  50%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)
})

test_that("No :bar item, :current and :total tokens", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, total = 10,
                           clear = FALSE, format = ":current/:total")
    pb$tick(2)
    pb$tick(5)
    pb$tick(3)
  })

  sout <- paste0(
    "\r2/10",
    "\r7/10",
    "\r10/10\n"
  )

  expect_equal(out, sout)
})

## This is somewhat timing dependent, but
## should work in general

test_that(":eta and :elapsed tokens", {

  skip_on_cran()

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, total = 4,
                           format = "[:eta :elapsed]")
    pb$tick(0)
    Sys.sleep(1)    # 1 sec per tick
    pb$tick(1)      # So 3 more ticks is 3 secs
    Sys.sleep(1)
    pb$tick(1)      # 2 more is 2 secs
    pb$tick(2)
  })

  sout <- paste0(
    "\r[ ?s  0s]",
    "\r[ 3s  1s]",
    "\r[ 2s  2s]",
    "\r[ 0s  2s]",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)
})

test_that("complete and incomplete chars", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, total = 5,
                           complete = "#", current = "@", incomplete = " ",
                           clear = FALSE)
    for (i in 1:5) pb$tick(1)
  })

  sout <- paste0(
    "\r[##@          ]  20%",
    "\r[####@        ]  40%",
    "\r[#######@     ]  60%",
    "\r[#########@   ]  80%",
    "\r[#############] 100%",
    "\n"
  )

  expect_equal(out, sout)
})

test_that("callback function", {

  x <- ""
  cb <- function(self) {
    x <<- "done"
  }

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, callback = cb)
    pb$tick(0)
    pb$tick(50)
    pb$tick(50)
  })

  expect_equal(x, "done")

  x <- ""

  str <- file(tmp <- tempfile(), open = "w")
  on.exit(unlink(tmp), add = TRUE)
  pb <- progress_bar$new(stream = str,
                         show_after = 0, width = 20, callback = cb)
  pb$tick(0)
  pb$tick(50)
  pb$tick(50)
  close(str)

  expect_equal(x, "done")

})

test_that("clearing and not clearing", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, clear = TRUE)
    pb$tick(0)
    pb$tick(50)
    pb$tick(50)
  })

  sout <- paste0(
    "\r[-------------]   0%",
    "\r[=====>-------]  50%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, clear = FALSE)
    pb$tick(0)
    pb$tick(50)
    pb$tick(50)
  })

  sout <- paste0(
    "\r[-------------]   0%",
    "\r[=====>-------]  50%",
    "\r[=============] 100%",
    "\n"
  )

  expect_equal(out, sout)
})

test_that("show_after argument", {

  skip_on_cran()

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = .1, width = 20, clear = TRUE)
    pb$tick(0)
    pb$tick(25)
    pb$tick(25)
    Sys.sleep(.1)
    pb$tick(25)
    pb$tick(25)
  })

  sout <- paste0(
    "\r[=========>---]  75%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})

test_that("custom tokens", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20,
                           format = ":what [:bar] :percent",
                           clear = FALSE, total = 200)
    pb$tick(50, tokens = list(what = "foo   "))
    pb$tick(50, tokens = list(what = "foo   "))
    pb$tick(50, tokens = list(what = "foobar"))
    pb$tick(50, tokens = list(what = "foobar"))
  })

  sout <- paste0(
    "\rfoo    [=>----]  25%",
    "\rfoo    [==>---]  50%",
    "\rfoobar [===>--]  75%",
    "\rfoobar [======] 100%",
    "\n"
  )

  expect_equal(out, sout)
})

test_that("bar adepts to width of custom tokens", {
  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20,
                           format = ":what [:bar] :percent",
                           clear = FALSE, total = 200)
    pb$tick(50, tokens = list(what = "text"))
    pb$tick(50, tokens = list(what = "long text"))
    pb$tick(100, tokens = list(what = "text"))
  })

  sout <- paste0(
    "\rtext [=>------]  25%",
    "\rlong text [=>-]  50%",
    "\rtext [========] 100%",
    "\n"
  )

  expect_equal(out, sout)
})

test_that(":rate and :bytes tokens", {

  skip_on_cran()

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20, total = 4 * 1024,
                           format = "[:rate :bytes]")
    pb$tick(0)
    Sys.sleep(1)       # 1 sec per 1000 bytes
    pb$tick(1024)      # So 3000 more bytes is 3 secs
    Sys.sleep(1)
    pb$tick(1024)      # 2000 more is 2 secs
    pb$tick(2048)
  })

  ## Next output line might be shorter, so 'progress_bar$render'
  ## might to erase it first
  soutm <- paste0(
    "^\\r\\[0 B/s 0 B\\]",
    "\\r?[ ]*",
    "\\r\\[[0-9\\.]+ k?B/s [0-9\\.]+ k?B\\]",
    "\\r?[ ]*",
    "\\r\\[[0-9\\.]+ k?B/s [0-9\\.]+ kB\\]",
    "\\r?[ ]*",
    "\\r\\[[0-9\\.]+ k?B/s [0-9\\.]+ kB\\]",
    "\\r[ ]*",
    "\\r$"
  )

  expect_match(out, soutm)
})

test_that("very quick loops, only the nothing is shown", {

  out <- get_output({
    pb <- progress_bar$new(total = 100, stream = stdout(), force = TRUE,
                           width = 20)
    for (i in 1:100) pb$tick()
  })

  sout <- paste0("")
  expect_equal(out, sout)

})


test_that("message works", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    for (i in 1:4) {
      if (i %% 2 == 0) {
        pb$message(as.character(i))
      }
      pb$tick(25)
    }
  })

  sout <- paste0(
    "\r[==>----------]  25%",
    "\r                    ",
    "\r2\n",
    "[==>----------]  25%",
    "\r[=====>-------]  50%",
    "\r[=========>---]  75%",
    "\r                    ",
    "\r4\n",
    "[=========>---]  75%",
    "\r[=============] 100%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})

test_that("terminate works", {

  out <- get_output({
    pb <- progress_bar$new(stream = stdout(), force = TRUE,
                           show_after = 0, width = 20)
    for (i in 1:4) {
      if (i == 3) {
        pb$terminate()
        break
      }
      pb$tick(25)
    }
  })

  sout <- paste0(
    "\r[==>----------]  25%",
    "\r[=====>-------]  50%",
    "\r                    ",
    "\r"
  )

  expect_equal(out, sout)

})
