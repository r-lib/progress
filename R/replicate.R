#' Same as \code{\link{base::replicate}} but with a progress bar.
#'
#' @seealso \code{\link{s2a}} 
#' @export
#' @examples 
#' library(dplyr)
#' replicate_pb(50000, {rnorm(1)>1.65}) %>% {paste(mean(.), " - ", sd(.))}
replicate_pb <- function(n, expr, simplify = "array",
                         format = "Replicating : [:bar] :percent in :elapsed",
                         ...){
  pb <- progress_bar$new(format = format, total = n, clear = T, ...)
  sapply(integer(n), eval.parent(substitute(function(...){ 
    pb$tick()
    expr
  })),
  simplify = simplify)
}
