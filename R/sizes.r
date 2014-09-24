
#' Bytes in a human readable string
#'
#' @param bytes Numeric vector.
#' @return Character vector, the formatted sizes.
#'
#' @export

pretty_bytes <- function(bytes) {

  stopifnot(is.numeric(bytes))

  units <- c('B', 'kB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB')

  neg <- bytes < 0
  bytes <- abs(bytes)

  exponent <- pmin(floor(log(bytes, 1000)), length(units) - 1)
  res <- round(bytes / 1000 ^ exponent, 2)
  unit <- units[exponent + 1]

  ## Zero bytes
  res[bytes == 0] <- 0
  unit[bytes == 0] <- units[1]

  paste0(ifelse(neg, '-', ''), res, ' ', unit)
}
