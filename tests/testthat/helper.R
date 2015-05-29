
get_output <- function(..., stream = stdout()) {

  if (identical(stream, stdout())) {
    type <- "output"
  } else if (identical(stream, stderr())) {
    type <- "message"
  }

  cleanup <- function() {
    unlink(tmp)
    sink(NULL, type = type)
  }

  tmp <- tempfile()
  on.exit(cleanup())

  sink(tmp, type = type)
  force(...)

  ## Windows has some strange readBin and sink interplay,
  ## so we need to remove the sink before reading the file
  sink(NULL, type = type)
  x <- readBin(tmp, raw(0), n = file.info(tmp)$size)
  unlink(tmp)

  ## No cleanup is needed any more
  on.exit(force(1))

  rawToChar(x)
}

win_newline <- function(..., collapse = NULL) {
  x <- paste0(...)
  if (.Platform$OS.type == "windows") {
    x <- gsub("\n", "\r\n", x, fixed = TRUE)
  }
  x
}
