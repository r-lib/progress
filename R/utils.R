
clear_line <- function(tty, width) {
  str <- paste0(c("\r", rep(" ", width)), collapse = "")
  cat(str, file = tty)
}

cursor_to_start <- function(tty) {
  cat("\r", file = tty)
}

is_stdout <- function(stream) {
  identical(stream, stdout()) && sink.number() == 0
}

is_stderr <- function(stream) {
  identical(stream, stderr())
}

is_r_studio <- function() {
  Sys.getenv("RSTUDIO") == 1
}

r_studio_stdout <- function(stream) {
  interactive() &&
    is_r_studio() &&
    identical(stream, stdout()) &&
    is_stdout(stream)
}

is_r_app <- function() {
  Sys.getenv("R_GUI_APP_VERSION") != ""
}

r_app_stdx <- function(stream) {
  interactive() &&
    is_r_app() &&
    (is_stdout(stream) || is_stderr(stream))
}

is_supported <- function(stream) {
  isatty(stream) || r_studio_stdout(stream) || r_app_stdx(stream)
}

default_stream <- function(stream) {
  if (! is.null(stream)) {
    stream
  } else {
    if (is_r_studio()) stdout() else stderr()
  }
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

assert_named_or_empty_list <- function(x) {
  stopifnot(length(x) == 0 || !is.null(names(x)))
}
