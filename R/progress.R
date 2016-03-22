
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
#'   \item{show_after}{Amount of time in seconds, after which the progress
#'     bar is shown on the screen. For very short processes,
#'     it is probably not worth showing it at all. Defaults to two
#'     tenth of a second.}
#'   \item{force}{Whether to force showing the progress bar,
#'     even if the given (or default) stream does not seem support it.}
#' }
#'
#' @section Using the progress bar:
#' Two functions can update a progress bar. \code{progress_bar$tick()}
#' increases the number of ticks by one (or another specified value).
#' \code{progress_bar$update()} sets a given ratio.
#'
#' The progress bar is displayed after the first `tick` command.
#' This might not be desirable for long computations, because
#' nothing is shown before the first tick. It is good practice to
#' call `tick(0)` at the beginning of the computation or download,
#' which shows the progress bar immediately.
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
#'   \item{:spin}{Shows a spinner that updates even when progress is
#'      advanced by zero.}
#' }
#'
#' Custom tokens are also supported, and you need to pass their
#' values to \code{progress_bar$tick()} or \code{progress_bar$update()},
#' in a named list. See example below.
#'
#' @importFrom R6 R6Class
#' @useDynLib progress
#'
#' @export
#' @examples
#' ## Basic
#' pb <- progress_bar$new(total = 100)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(.1 / 100)
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
#'   Sys.sleep(.1 / 100)
#' }
#'
#' ## Spinner
#' pb <- progress_bar$new(
#'   format = "(:spin) [:bar] :percent",
#'   total = 30, clear = FALSE, width = 60)
#' for (i in 1:30) {
#'   pb$tick()
#'   Sys.sleep(3 / 100)
#' }
#'
#' ## Custom tokens
#' pb <- progress_bar$new(
#'   format = "  downloading :what [:bar] :percent eta: :eta",
#'   clear = FALSE, total = 200, width = 60)
#' f <- function() {
#'   for (i in 1:100) {
#'     pb$tick(tokens = list(what = "foo   "))
#'     Sys.sleep(0.5 / 100)
#'   }
#'   for (i in 1:100) {
#'     pb$tick(tokens = list(what = "foobar"))
#'     Sys.sleep(0.5 / 100)
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
#'     Sys.sleep(.1/100)
#'   }
#'   pb$tick(1e7)
#'   invisible()
#' }
#' f()
#'
#' @name progress_bar
NULL

progress_bar <- R6Class("progress_bar",

  public = list(

    initialize = function(format = "[:bar] :percent", total = 100,
      width = getOption("width") - 2, stream = NULL, complete = "=",
      incomplete = "-", callback = NULL, clear = TRUE,
      show_after = 0.2, force = FALSE) {
        pb_init(self, private, format, total, width, stream, complete,
          incomplete, callback, clear, show_after, force)
    },
    tick = function(len = 1, tokens = list()) {
      .Call("progress_tick", self, private, len, tokens,
        PACKAGE = "progress") },
    update = function(ratio, tokens = list()) { 
      .Call("progress_update", self, private, ratio, tokens,
        PACKAGE = "progress") }
  ),

  private = list(

    render = function(tokens) {
      .Call("progress_render", self, private, tokens,
        PACKAGE = "progress") },
    terminate = function() {
      .Call("progress_terminate", self, private,
        PACKAGE = "progress") },

    first = TRUE,
    supported = NA,
    format = NULL,
    total = NULL,
    current = 0L,
    width = NULL,
    stream = NULL,
    chars = list(
      complete = "=",
      incomplete = "-"
    ),
    callback = NULL,
    clear = NULL,
    show_after = NULL,
    last_draw = "",

    start = NULL,
    toupdate = FALSE,
    complete = FALSE,
    spin = 1L,

    spin_symbols = c("-", "\\", "|", "/"),

    ## Order is important here, the C code uses this exact order!
    has_token = c(current = FALSE, total = FALSE, elapsed = FALSE,
      eta = FALSE, percent = FALSE, rate = FALSE, bytes = FALSE,
      bar = FALSE, spin = FALSE)
  )
)

pb_init <- function(self, private, format, total, width, stream,
                    complete, incomplete, callback, clear, show_after,
                    force) {

  stream <- default_stream(stream)

  assert_character_scalar(format)
  assert_positive_scalar(total)
  assert_nonzero_count(width)
  assert_connection(stream)
  assert_single_char(complete)
  assert_single_char(incomplete)
  assert_function_or_null(callback)
  assert_flag(clear)
  assert_nonnegative_scalar(show_after)

  private$first <- TRUE
  private$supported <- force || is_supported(stream)
  private$format <- format
  private$total <- total
  private$width <- width
  private$stream <- stream
  private$chars$complete <- complete
  private$chars$incomplete <- incomplete
  private$callback <- callback
  private$clear <- clear
  private$show_after <- as.difftime(show_after, units = "secs")
  private$spin <- 1L

  self
}
