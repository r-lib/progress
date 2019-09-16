
context("progress_handler_cli")

test_that("basics", {
  h <- progress_handler_cli()
  out <- get_output(with_only_handler(h, {
      id <- job_add("foobar", id = "xy")
      job_add_progress(id = id)
      job_set_progress(id = id, 50)
      job_add_progress(id = id, 10)
      job_set_status(id = id, "foobar2")
      job_set_estimate(id = id, seconds = 42)
      job_add_output(id = id, "out put")
      job_complete(id = id)
    }
  ))

  # We drop the first two elements, because that's the clearing of the
  # current status bar, which is context dependent
  ncout <- split_lines(gsub("\r", "\n", crayon::strip_style(out)))[-(1:2)]
  expect_equal(
    ncout,
    c("",
      "",
      "foobar 0/100 | ",
      "               ",
      "foobar 1/100 | ",
      "               ",
      "foobar 50/100 | ",
      "                ",
      "foobar 60/100 | ",
      "                ",
      "foobar 60/100 | foobar2",
      "                       ",
      "foobar 60/100 | foobar2",
      "                       ",
      "foobar 60/100 | foobar2",
      "                       ",
      "foobar 60/100 | foobar2",
      "                       "
    )
  )
})
