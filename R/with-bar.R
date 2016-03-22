
#' @export

progress <- structure(list(total = 100), class = "pb")

#' @export

print.pb <- function(x, ...) {
  cat("Progress bar generator. Examples:\n")
  cat("  progress %))% for (i in seq) { body(i) }\n")
  cat("  progress %))% lapply(seq, function(i) { body(i) }\n")
}

#' @export

`%~~%` <- function(e1, e2) {
  if (! interactive() || !is_supported(default_stream(NULL))) {
    e2
  } else {
    loop <- substitute(e2)
    if (is.call(loop) && identical(loop[[1]], quote(`lapply`))) {
      pb_lapply(loop, e1)

    } else {
      stop("progress only works with lapply calls currently")
    }
  }
}

pb_lapply <- function(loop, pars) {
  lcall <- match.call(lapply, loop, expand.dots = FALSE)

  mypb <- do.call(progress_bar$new, unclass(pars))

  X <- eval.parent(lcall$X, 2)
  len <- length(X)
  counter <- 1

  tick <- function() {
    mypb$update(counter / len)
    counter <<- counter + 1
  }

  new_fun <- seq2(eval.parent(lcall$FUN, 2), tick)
  do.call(
    lapply,
    c(list(X, new_fun), lcall$`...`),

  )
}

seq2 <- function (f1, f2) {
  function(...) { res <- f1(...); f2(); res }
}
