
spark_ticks <- c("\u2581", "\u2582", "\u2583", "\u2584",
                 "\u2585", "\u2586", "\u2587", "\u2588")

scale_to <- function(data, width) {

  stopifnot(is.numeric(data), is.numeric(width),
            length(width) == 1, width == as.integer(width), width >= 2)

  sun <- seq_along(data) / length(data) * width
  sun_unit <- sun[2] - sun[1]
  res <- numeric(width)
  for (i in seq_along(res)) {
    points <- which(sun >= i-1 & sun <= i)
    w <- rep(1, length(points))

    if (points[1] != 1) {
      w[1] <- (sun[points[1]] - (i-1)) / sun_unit
    }

    if (tail(points, 1) != length(data)) {
      w1 <- (i - sun[tail(points, 1)]) / sun_unit
      w <- c(w, w1)
      points <- c(points, tail(points, 1) + 1)
    }
    res[i] <- sum(w * data[points]) / sum(w)
  }
  res
}

#' Spark line of a numeric vector on the terminal
#'
#' @param data The data to visualize. It can be a numeric
#'   vector, or anything that can be cut into intervals
#'   with \code{cut}. Infinite values in numeric data are ignored,
#'   and a black character is plotted instead of them.
#' @param width The width (number of characters) of the output.
#'   \sQuote{data} means that it will match the length of the data.
#'   \sQuote{screen} means that it will be scaled to match the
#'   width of the screen. \sQuote{auto} means \sQuote{data}
#'   if the length of the data is not longer than the screen width,
#'   and \sQuote{screen} otherwise.
#' @return Character scalar containing the spark line.
#'
#' @export
#' @examples
#' ## Annoal number of Lynx trappings
#' cat(spark_line(lynx[1:getOption("width")]), "\n")
#'
#' ## Luteinizing Hormone in Blood Samples,
#' ## in blue, if the terminal supports it
#' cat(crayon::blue(spark_line(lh)), "\n")

spark_line <- function(data, width = c("data", "auto", "screen")) {

  width <- match.arg(width)

  win_size <- getOption("width")
  if (width == "auto") {
    width <- if (width <= win_size) "data" else "screen"
  }

  if (width == "screen") data <- scale_to(data, win_size)

  if (is.numeric(data)) data[!is.finite(data)] <- NA

  code <- cut(data, breaks = length(spark_ticks)) %>%
    as.integer()

  ifelse(is.na(code), ' ', spark_ticks[code]) %>%
    paste(collapse = "")
}
