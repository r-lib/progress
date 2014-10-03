
clear_line <- function(tty, width) {
  if (!isatty(tty)) { return(invisible()) }
  str <- paste0(c("\r", rep(" ", width)), collapse = "")
  cat(str, file = tty)
}

cursor_to_start <- function(tty) {
  if (!isatty(tty)) { return(invisible()) }
  cat("\r", file = tty)
}

assert_character_scalar <- function(x) {
  stopifnot(is.character(x),
            length(x) == 1,
            !is.na(x))
}

assert_scalar <- function(x, finite = TRUE) {
  stopifnot(is.numeric(x),
            length(x) == 1,
            !is.na(x),
            !finite || is.finite(x))
}

assert_positive_scalar <- function(x, finite = TRUE) {
  assert_scalar(x, finite = finite)
  stopifnot(x > 0)
}

assert_nonnegative_scalar <- function(x, finite = TRUE) {
  assert_scalar(x, finite = finite)
  stopifnot(x >= 0)
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