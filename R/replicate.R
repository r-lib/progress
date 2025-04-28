#' Same as [base::replicate()] but with a progress bar.
#'
#' @param n Integer: the number of replications.
#' @param expr The expression (a language object, usually a call) to
#'   evaluate repeatedly.
#' @param simplify Passed to [base::sapply()], see the docs there.
#' @param format Progress bar format.
#' @param ... Extra arguments are passed to `progress_bar$new()`.
#'
#' @export
#' @examples
#' replicate(50000, rnorm(1) > 1.65) |>
#'     (\(x) list(mean = mean(x), sd = sd(x)))()

replicate <- function(n, expr, simplify = "array",
                      format = "Replicating : [:bar] :percent in :elapsed",
                      ...) {
  pb <- progress_bar$new(format = format, total = n, clear = T, ...)
  sapply(
    integer(n),
    eval.parent(substitute(function(...) {
      pb$tick()
      expr
    })),
    simplify = simplify
  )
}
