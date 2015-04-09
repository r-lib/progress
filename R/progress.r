
#' @importFrom magrittr %>%
NULL

. <- "STFU"

#' Progress bar in the terminal
#'
#' Progress bars are configurable, may include percentage, elapsed time,
#' and/or the estimated completion time. They work in the command line,
#' in Emacs and in R Studio. The progress package was heavily influenced by
#' https://github.com/tj/node-progress
#'
#' @section Creating the progress bar:
#' A progress bar is an R6 object, that can be created with
#' \code{progress_bar$new()}. It has the following arguments:
#' \describe{
#'   \item{format}{The format of the progress bar. A number of
#'     tokens can be used here, see them below. It defaults to
#'     \code{"[:bar] :percent"}, which means that the progress
#'     bar is within brackets on the left, and the percentage
#'     is printed on the right.}
#'   \item{total}{Total number of ticks to complete. Defaults to 100.}
#'   \item{width}{Width of the progress bar. Default is the current
#'     terminal width (see \code{options()} and \code{width}) minus two.}
#'   \item{stream}{The output stream to put the progress bar on.
#'     It defaults to \code{stderr()}, except in R Studio that has
#'     a bug when printing on the standard error, so there we use
#'     \code{stdout}. If the output stream is not a terminal and
#'     we are not in R Studio, then no progress bar is printed.}
#'   \item{complete}{Completion character, defaults to \code{=}.}
#'   \item{incomplete}{Incomplete character, defaults to \code{-}.}
#'   \item{callback}{Callback function to call when the progress
#'     bar finishes. The progress bar object itself is passed to it
#'     as the single parameter.}
#'   \item{clear}{Whether to clear the progress bar on completion.
#'     Defaults to \code{TRUE}.}
#' }
#'
#' @section Using the progress bar:
#' Two functions can update a progress bar. \code{progress_bar$tick()}
#' increases the number of ticks by one (or another specified value).
#' \code{progress_bar$update()} sets a given ratio.
#'
#' @section Tokens:
#' They can be used in the \code{format} argument when creating the
#' progress bar.
#' \describe{
#'   \item{:bar}{The progress bar itself.}
#'   \item{:current}{Current tick number.}
#'   \item{:total}{Total ticks.}
#'   \item{:elapsed}{Elapsed time in seconds.}
#'   \item{:eta}{Estimated completion time in seconds.}
#'   \item{:percent}{Completion percentage.}
#'   \item{:rate}{Download rate, bytes per second. See example below.}
#'   \item{:bytes}{Shows :current, formatted as bytes. Useful
#'      for downloads or file reads if you don't know the size of the
#'      file in advance. See example below.}
#' }
#'
#' Custom tokens are also supported, and you need to pass their
#' values to \code{progress_bar$tick()} or \code{progress_bar$update()},
#' in a named list. See example below.
#'
#' @importFrom R6 R6Class
#'
#' @export
#' @examples
#' \donttest{
#' ## Basic
#' pb <- progress_bar$new(total = 100)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(1 / 100)
#' }
#'
#' ## ETA
#' pb <- progress_bar$new(
#'   format = "  downloading [:bar] :percent eta: :eta",
#'   total = 100, clear = FALSE, width= 60)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(1 / 100)
#' }
#'
#' ## Elapsed time
#' pb <- progress_bar$new(
#'   format = "  downloading [:bar] :percent in :elapsed",
#'   total = 100, clear = FALSE, width= 60)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(1 / 100)
#' }
#'
#' ## Custom tokens
#' pb <- progress_bar$new(
#'   format = "  downloading :what [:bar] :percent eta: :eta",
#'   clear = FALSE, total = 200, width = 60)
#' f <- function() {
#'   for (i in 1:100) {
#'     pb$tick(tokens = list(what = "foo   "))
#'     Sys.sleep(2 / 100)
#'   }
#'   for (i in 1:100) {
#'     pb$tick(tokens = list(what = "foobar"))
#'     Sys.sleep(2 / 100)
#'   }
#' }
#' f()
#'
#' ## Download (or other) rates
#' pb <- progress_bar$new(
#'   format = "  downloading foobar at :rate, got :bytes in :elapsed",
#'   clear = FALSE, total = 1e7, width = 60)
#' f <- function() {
#'   for (i in 1:100) {
#'     pb$tick(sample(1:100 * 1000, 1))
#'     Sys.sleep(2/100)
#'   }
#'   pb$tick(1e7)
#'   invisible()
#' }
#' f()
#' }
#'
#' @name progress_bar
NULL

progress_bar <- R6Class("progress_bar",

  public = list(

    initialize = function(format = "[:bar] :percent", total = 100,
      width = getOption("width") - 2, stream = NULL, complete = "=",
      incomplete = "-", callback = function(self) {}, clear = TRUE) {
        pb_init(self, private, format, total, width, stream, complete,
          incomplete, callback, clear)
    },
    tick = function(len = 1, tokens = list()) {
      pb_tick(self, private, len, tokens) },
    update = function(ratio, tokens) { pb_update(self, private, ratio, tokens) }
  ),

  private = list(

    render = function(tokens) { pb_render(self, private, tokens) },
    terminate = function() { pb_terminate(self, private) },

    format = NULL,
    total = NULL,
    current = 0,
    width = NULL,
    stream = NULL,
    chars = list(
      complete = "=",
      incomplete = "-"
    ),
    callback = NULL,
    clear = NULL,
    last_draw = "",

    start = NULL,
    complete = FALSE
  )
)


pb_init <- function(self, private, format, total, width, stream,
                    complete, incomplete, callback, clear) {

  stream <- default_stream(stream)

  assert_character_scalar(format)
  assert_positive_scalar(total)
  assert_nonzero_count(width)
  assert_connection(stream)
  assert_single_char(complete)
  assert_single_char(incomplete)
  assert_function(callback)
  assert_flag(clear)

  private$format <- format
  private$total <- total
  private$width <- width
  private$stream <- stream
  private$chars$complete <- complete
  private$chars$incomplete <- incomplete
  private$callback <- callback
  private$clear <- clear

  self
}

pb_tick <- function(self, private, len, tokens) {

  assert_positive_scalar(len)
  assert_named_or_empty_list(tokens)

  if (private$current == 0) private$start = Sys.time()

  private$current <- private$current + len

  private$render(tokens)

  if (private$current > private$total ||
      isTRUE(all.equal(private$current, private$total))) {
    private$complete <- TRUE
    private$terminate()
    private$callback(self)
  }

  self
}

#' @importFrom magrittr subtract divide_by
#' @importFrom prettyunits vague_dt pretty_bytes
#' @importFrom utils flush.console

pb_render <- function(self, private, tokens) {

  if (! is_supported(private$stream)) return(invisible())

  ratio <- (private$current / private$total) %>%
    max(0) %>%
    min(1)
  percent <- ratio * 100
  elapsed_secs <- Sys.time() %>%
    subtract(private$start)
  elapsed <- vague_dt(elapsed_secs, format = "terse")
  eta_secs <- if (isTRUE(all.equal(percent, 100))) {
    0
  } else {
    elapsed_secs * (private$total / private$current - 1.0)
  }
  eta <- eta_secs %>%
    as.difftime(units = "secs") %>%
    vague_dt(format = "terse")
  rate <- private$current %>%
    divide_by(as.double(elapsed_secs, units = "secs")) %>%
    round() %>%
    pretty_bytes() %>%
    paste0("/s")
  bytes <- private$current %>%
    round() %>%
    pretty_bytes()

  str <- private$format %>%
    sub(pattern = ":current", replacement = round(private$current)) %>%
    sub(pattern = ":total", replacement = round(private$total)) %>%
    sub(pattern = ":elapsed", replacement = elapsed) %>%
    sub(pattern = ":eta", replacement = eta) %>%
    sub(pattern = ":percent", replacement =
          paste0(format(round(percent), width = 3), "%")) %>%
    sub(pattern = ":rate", replacement = rate) %>%
    sub(pattern = ":bytes", replacement = bytes)

  bar_width <- str %>%
    sub(pattern = ":bar", replacement = "") %>%
    nchar() %>%
    subtract(private$width, .) %>%
    max(0)

  complete_len <- round(bar_width * ratio)
  complete <- rep("", complete_len + 1) %>%
    paste(collapse = private$chars$complete)
  incomplete <- rep("", bar_width - complete_len + 1) %>%
    paste(collapse = private$chars$incomplete)

  str <- sub(":bar", paste0(complete, incomplete), str)

  for (t in names(tokens)) {
    str <- gsub(paste0(":", t), tokens[[t]], str, fixed = TRUE)
  }

  if (private$last_draw != str) {
    if (nchar(private$last_draw) > nchar(str)) {
      clear_line(private$stream, private$width)
    }
    cursor_to_start(private$stream)
    cat(str, file = private$stream)
    private$last_draw <- str
  }

  flush.console()

  self
}

pb_update <- function(self, private, ratio, tokens) {
  assert_ratio(ratio)
  goal <- floor(ratio * private$total)
  private$tick(goal - private$current, tokens)
}

pb_terminate <- function(self, private) {
  if (private$clear) {
    clear_line(private$stream, private$width)
    cursor_to_start(private$stream)
  } else {
    cat("\n")
  }
}
