
is_stdout <- function(stream) {
  identical(stream, stdout()) && sink.number() == 0
}

is_stderr <- function(stream) {
  identical(stream, stderr())
}

is_r_studio <- function() {
  Sys.getenv("RSTUDIO") == 1
}

r_studio_stdx <- function(stream) {
  r_studio_stdout(stream) || r_studio_stderr(stream)
}

r_studio_stdout <- function(stream) {
  interactive() &&
    is_r_studio() &&
    identical(stream, stdout()) &&
    is_stdout(stream)
}

r_studio_stderr <- function(stream) {
  interactive() &&
    is_r_studio() &&
    identical(stream, stderr()) &&
    is_stderr(stream)
}

is_r_app <- function() {
  Sys.getenv("R_GUI_APP_VERSION") != ""
}

r_app_stdx <- function(stream) {
  interactive() &&
    is_r_app() &&
    (is_stdout(stream) || is_stderr(stream))
}

is_rkward <- function() {
    "rkward" %in% (.packages())
}

rkward_stdx <- function(stream) {
  interactive() &&
    is_rkward() &&
    (is_stdout(stream) || is_stderr(stream))
}

is_supported <- function(stream) {
  is_option_enabled() &&
    (isatty(stream) || r_studio_stdx(stream) || r_app_stdx(stream) || rkward_stdx(stream))
}

is_option_enabled <- function() {
  isTRUE(getOption("progress_enabled", TRUE))
}

default_stream <- function(stream) {
  if (! is.null(stream)) {
    stream
  } else {
    if (is_r_studio()) stdout() else stderr()
  }
}

assert_character <- function(x) {
  stopifnot(is.character(x),
            length(x) > 0)
}
assert_character_scalar <- function(x) {
  stopifnot(is.character(x),
            length(x) == 1,
            !is.na(x))
}

assert_scalar <- function(x, finite = TRUE, na = FALSE) {
  stopifnot(is.numeric(x),
            length(x) == 1,
            na || !is.na(x),
            na || !finite || is.finite(x))
}

assert_positive_scalar <- function(x, finite = TRUE) {
  assert_scalar(x, finite = finite)
  stopifnot(x > 0)
}

assert_nonnegative_scalar <- function(x, finite = TRUE, na = FALSE) {
  assert_scalar(x, finite = finite, na = na)
  stopifnot(na || x >= 0)
}


assert_ratio <- function(x) {
  assert_nonnegative_scalar(x)
  stopifnot(x <= 1)
}

assert_nonzero_count <- function(x, finite = TRUE) {
  assert_positive_scalar(x, finite = TRUE)
  stopifnot(as.integer(x) == x)
}

assert_connection <- function(x) {
  stopifnot(inherits(x, "connection"))
}

assert_single_char <- function(x) {
  assert_character_scalar(x)
  stopifnot(nchar(x) == 1)
}

assert_function <- function(x) {
  stopifnot(is.function(x))
}

assert_flag <- function(x) {
  stopifnot(is.logical(x), length(x) == 1, !is.na(x))
}

assert_named_or_empty_list <- function(x) {
  stopifnot(length(x) == 0 || !is.null(names(x)))
}



#' Same as \code{\link{base::replicate}} but with a progress bar.
#'
#' @seealso \code{\link{s2a}} 
#' @export
#' @examples 
#' library(dplyr)
#' replicate_pb(50000, {rnorm(1)>1.65}) %>% {paste(mean(.), " - ", sd(.))}
replicate_pb = function(n, expr, simplify = "array", format = "Replicating : [:bar] :percent in :elapsed", ...){
  pb <- progress_bar$new(format = format, total = n, clear = T, ...)
  sapply(integer(n), eval.parent(substitute(function(...){ 
    pb$tick()
    expr
  })),
  simplify = simplify)
}
