
spark_ticks <- c("\u2581", "\u2582", "\u2583", "\u2584",
                 "\u2585", "\u2586", "\u2587", "\u2588")

#' Spark line of a numeric vector on the terminal
#'
#' @param data The data to visualize. It can be a numeric
#'   vector, or anything that can be cut into intervals
#'   with \code{cut}. Infinite values in numeric data are ignored,
#'   and a black character is plotted instead of them.
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

spark_line <- function(data) {

  if (is.numeric(data)) data[!is.finite(data)] <- NA

  code <- cut(data, breaks = length(spark_ticks)) %>%
    as.integer()

  ifelse(is.na(code), ' ', spark_ticks[code]) %>%
    paste(collapse = "")
}
