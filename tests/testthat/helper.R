
get_output <- function(..., stream = stdout()) {

  if (identical(stream, stdout())) {
    type <- "output"
  } else if (identical(stream, stderr())) {
    type <- "message"
  }

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  on.exit(sink(NULL, type = type), add = TRUE)

  sink(tmp, type = type)
  force(...)
  sink(NULL, type = type)

  rawToChar(readBin(tmp, raw(0), n = file.info(tmp)$size))
}
