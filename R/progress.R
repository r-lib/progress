
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
#'   \item{total}{Total number of ticks to complete. If it is unknown,
#'      use \code{NA} here. Defaults to 100.}
#'   \item{width}{Width of the progress bar. Default is the current
#'     terminal width (see \code{options()} and \code{width}) minus two.}
#'   \item{stream}{This argument is deprecated, and \code{message()} is
#'     used to print the progress bar.}
#'   \item{complete}{Completion character, defaults to \code{=}.}
#'   \item{incomplete}{Incomplete character, defaults to \code{-}.}
#'   \item{current}{Current character, defaults to \code{>}.}
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
#'     even if the given (or default) stream does not seem to support it.}
#'   \item{message_class}{Extra classes to add to the message conditions
#'     signalled by the progress bar.}
#' }
#'
#' @section Using the progress bar:
#' Three functions can update a progress bar. \code{progress_bar$tick()}
#' increases the number of ticks by one (or another specified value).
#' \code{progress_bar$update()} sets a given ratio and
#' \code{progress_bar$terminate()} removes the progress bar.
#' \code{progress_bar$finished} can be used to see if the progress bar has
#' finished.
#'
#' Note that the progress bar is not shown immediately, but only after
#' \code{show_after} seconds. (Set this to zero, and call `tick(0)` to
#' force showing the progress bar.)
#'
#' \code{progress_bar$message()} prints a message above the progress bar.
#' It fails if the progress bar has already finished.
#'
#' @section Tokens:
#' They can be used in the \code{format} argument when creating the
#' progress bar.
#' \describe{
#'   \item{:bar}{The progress bar itself.}
#'   \item{:current}{Current tick number.}
#'   \item{:total}{Total ticks.}
#'   \item{:elapsed}{Elapsed time in seconds.}
#'   \item{:elapsedfull}{Elapsed time in hh:mm:ss format.}
#'   \item{:eta}{Estimated completion time in seconds.}
#'   \item{:percent}{Completion percentage.}
#'   \item{:rate}{Download rate, bytes per second. See example below.}
#'   \item{:tick_rate}{Similar to \code{:rate}, but we don't assume that
#'      the units are bytes, we just print the raw number of ticks per
#'      second.}
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
#' @section Options:
#' The `progress_enabled` option can be set to `FALSE` to turn off the
#' progress bar. This works for the C++ progress bar as well.
#'
#' @importFrom R6 R6Class
#'
#' @export
#' @examples
#'
#' ## We don't run the examples on CRAN, because they takes >10s
#' ## altogether. Unfortunately it is hard to create a set of
#' ## meaningful progress bar examples that also run quickly.
#' \dontrun{
#'
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
#'   clear = FALSE, total = NA, width = 60)
#' f <- function() {
#'   for (i in 1:100) {
#'     pb$tick(sample(1:100 * 1000, 1))
#'     Sys.sleep(2/100)
#'   }
#'   pb$tick(1e7)
#'   invisible()
#' }
#' f()
#'
#' pb <- progress_bar$new(
#'   format = "  got :current rows at :tick_rate/sec",
#'   clear = FALSE, total = NA, width = 60)
#' f <- function() {
#'   for (i in 1:100) {
#'     pb$tick(sample(1:100, 1))
#'     Sys.sleep(2/100)
#'   }
#'   pb$terminate()
#'   invisible()
#' }
#' f()
#'
#' }
#'
#' @name progress_bar
NULL

progress_bar <- R6Class("progress_bar",

  public = list(

    initialize = function(format = "[:bar] :percent", total = 100,
      width = getOption("width") - 2, stream = NULL, complete = "=",
      incomplete = "-", current = ">", callback = function(self) {},
      clear = TRUE, show_after = 0.2, force = FALSE, message_class = NULL) {
        pb_init(self, private, format, total, width, stream, complete,
                incomplete, current, callback, clear, show_after, force,
                message_class)
    },
    tick = function(len = 1, tokens = list()) {
      pb_tick(self, private, len, tokens) },
    update = function(ratio, tokens = list()) {
      pb_update(self, private, ratio, tokens) },
    message = function(msg, set_width = TRUE) {
      pb_message(self, private, msg, set_width) },
    terminate = function() { pb_terminate(self, private) },
    finished = FALSE
  ),

  private = list(

    render = function(tokens) { pb_render(self, private, tokens) },
    ratio = function() { pb_ratio(self, private) },
    progress_message = function(..., domain = NULL, appendLF = TRUE) {
      pb_progress_message(self, private, ..., domain = domain,
                          appendLF = appendLF) },
    clear_line = function(width) {
      pb_clear_line(self, private, width) },
    cursor_to_start = function() {
      pb_cursor_to_start(self, private) },

    first = TRUE,
    supported = NA,
    format = NULL,
    total = NULL,
    current = 0,
    width = NULL,
    chars = list(
      complete = "=",
      incomplete = "-",
      current = ">"
    ),
    callback = NULL,
    clear = NULL,
    show_after = NULL,
    last_draw = "",
    message_class = NULL,

    start = NULL,
    toupdate = FALSE,
    complete = FALSE,

    spin = NULL,

    has_token = c(current = FALSE, total = FALSE, elapsedfull = FALSE,
      elapsed = FALSE, eta = FALSE, percent = FALSE, rate = FALSE,
      bytes = FALSE, bar = FALSE, spin = FALSE, tick_rate = FALSE)
  )
)

pb_init <- function(self, private, format, total, width, stream,
                    complete, incomplete, current, callback, clear,
                    show_after, force, message_class) {

  assert_character_scalar(format)
  assert_nonnegative_scalar(total <- as.numeric(total), na = TRUE)
  assert_nonzero_count(width)
  assert_single_char(complete, strip_color = TRUE)
  assert_single_char(incomplete, strip_color = TRUE)
  assert_single_char(current, strip_color = TRUE)
  assert_function(callback)
  assert_flag(clear)
  assert_nonnegative_scalar(show_after)

  private$first <- TRUE
  private$supported <- force || is_supported(stderr())
  private$format <- format
  private$total <- total
  private$width <- width
  private$chars$complete <- complete
  private$chars$incomplete <- incomplete
  private$chars$current <- current
  private$callback <- callback
  private$clear <- clear
  private$show_after <- as.difftime(show_after, units = "secs")
  private$spin <- spin_symbols()
  private$message_class <- message_class

  private$has_token <- pb_update_has_token(private$has_token, format)

  self
}

pb_update_has_token <- function(tokens, format) {
  for (n in names(tokens)) {
    tokens[n] <- grepl(paste0(":", n), format, fixed = TRUE)
  }

  tokens
}

pb_tick <- function(self, private, len, tokens) {

  assert_scalar(len)
  assert_named_or_empty_list(tokens)
  stopifnot(!self$finished)

  if (private$first) {
    private$first <- FALSE
    private$start <- Sys.time()
  }

  private$current <- private$current + len

  if (!private$toupdate) {
    if (Sys.time() - private$start >= private$show_after) {
      private$toupdate <- TRUE
    }
  }

  if (!is.na(private$total) && private$current >= private$total) {
    private$complete <- TRUE
  }

  if (private$toupdate) private$render(tokens)

  if (private$complete) {
    self$terminate()
    private$callback(self)
  }

  self
}

#' @importFrom prettyunits vague_dt pretty_bytes
#' @importFrom utils flush.console

pb_ratio <- function(self, private) {
  ratio <- (private$current / private$total)
  ratio <- max(ratio, 0)
  ratio <- min(ratio, 1)
  ratio
}

#' @importFrom hms as.hms
#' @importFrom crayon col_nchar col_substr

pb_render <- function(self, private, tokens) {

  if (! private$supported) return(invisible())

  str <- private$format

  if (private$has_token["percent"]) {
    percent <- private$ratio() * 100
    str <- sub(str, pattern = ":percent", replacement =
                 paste0(format(round(percent), width = 3), "%"))
  }

  if (private$has_token["elapsedfull"]) {
    elapsed <- Sys.time() - private$start
    units(elapsed) <- "secs"
    elapsedfull <- format(as.hms(as.integer(elapsed)))
    str <- sub(str, pattern = ":elapsedfull", replacement = elapsedfull)
  }

  if (private$has_token["elapsed"]) {
    elapsed_secs <- Sys.time() - private$start
    elapsed <- vague_dt(elapsed_secs, format = "terse")
    str <- sub(str, pattern = ":elapsed", replacement = elapsed)
  }

  if (private$has_token["eta"]) {
    if (is.na(private$total)) {
      eta <- "?"
    } else {
      percent <- private$ratio() * 100
      elapsed_secs <- Sys.time() - private$start
      eta_secs <- if (percent == 100) {
        0
      } else {
        elapsed_secs * (private$total / private$current - 1.0)
      }
      eta <- as.difftime(eta_secs, units = "secs")
      if (is.nan(eta) || eta == Inf) {
        eta <- " ?s"
      } else {
        eta <- vague_dt(eta, format = "terse")
      }
    }
    str <- sub(str, pattern = ":eta", replacement = eta)
  }

  if (private$has_token["rate"]) {
    elapsed_secs <- Sys.time() - private$start
    rate <- private$current / as.double(elapsed_secs, units = "secs")
    if (is.nan(rate)) rate <- 0
    rate <- paste0(pretty_bytes(round(rate)), "/s")
    str <- sub(str, pattern = ":rate", replacement = rate)
  }

  if (private$has_token["tick_rate"]) {
    elapsed_secs <- Sys.time() - private$start
    tick_rate <- private$current / as.double(elapsed_secs, units = "secs")
    if (is.nan(tick_rate)) tick_rate <- 0
    tick_rate <- format(tick_rate, digits = 2)
    str <- sub(str, pattern = ":tick_rate", replacement = tick_rate)
  }

  if (private$has_token["current"]) {
    str <- sub(str, pattern = ":current",
               replacement = round(private$current))
  }

  if (private$has_token["total"]) {
    str <- sub(str, pattern = ":total", replacement = round(private$total))
  }

  if (private$has_token["bytes"]) {
    bytes <- pretty_bytes(round(private$current))
    str <- sub(str, pattern = ":bytes", replacement = bytes)
  }

  if (private$has_token["spin"]) {
    ## NOTE: fixed = TRUE is needed here or "\\" causes trouble with
    ## the replacement (I think it's interpreted as an invalid
    ## backreference).
    str <- sub(str, pattern = ":spin", replacement = private$spin(), fixed = TRUE)
  }

  for (t in names(tokens)) {
    txt <- tryCatch(as.character(tokens[[t]])[[1]], error = function(e) "???")
    str <- gsub(paste0(":", t), txt, str, fixed = TRUE)
  }

  if (private$has_token["bar"]) {
    bar_width <- col_nchar(sub(str, pattern = ":bar", replacement = ""))
    bar_width <- private$width - bar_width
    bar_width <- max(0, bar_width)

    ratio <- private$ratio()
    complete_len <- round(bar_width * ratio)
    complete <- paste(rep("", complete_len),
                      collapse = private$chars$complete)
    current <- if (private$complete) {
      private$chars$complete
    } else if (complete_len >= 1) {
      private$chars$current
    }
    incomplete <- paste(rep("", bar_width - complete_len + 1),
                        collapse = private$chars$incomplete)

    str <- sub(
      ":bar", paste0(complete, current, incomplete), str)
  }

  if (col_nchar(str) > private$width) {
    str <- paste0(col_substr(str, 1, private$width - 3), "...")
  }

  if (private$last_draw != str) {
    if (col_nchar(private$last_draw) > col_nchar(str)) {
      private$clear_line(private$width)
    }
    private$cursor_to_start()
    private$progress_message(str, appendLF = FALSE)
    private$last_draw <- str
  }

  flush.console()

  self
}

pb_update <- function(self, private, ratio, tokens) {
  assert_ratio(ratio)
  stopifnot(!self$finished)

  goal <- floor(ratio * private$total)
  self$tick(goal - private$current, tokens)
}

pb_message <- function(self, private, msg, set_width) {
  assert_character(msg)
  stopifnot(!self$finished)

  if (set_width) {
    too_long <- col_nchar(msg) > private$width
    if (any(too_long)) {
      msg[too_long] <-
        paste0(col_substr(msg[too_long], 1, private$width - 3), "...")
    }
  }

  if (!private$supported) {
    private$progress_message(paste0(msg, "\n"), appendLF = FALSE)
  } else {
    private$clear_line(private$width)
    private$cursor_to_start()
    private$progress_message(paste0(msg, "\n"), appendLF = FALSE)
    if (!self$finished) {
      private$progress_message(private$last_draw, appendLF = FALSE)
    }
  }
}

pb_terminate <- function(self, private) {
  self$finished <- TRUE
  if (!private$supported || !private$toupdate) return(invisible())
  if (private$clear) {
    private$clear_line(private$width)
    private$cursor_to_start()
  } else {
    private$progress_message("\n", appendLF = FALSE)
  }
}

spin_symbols <- function() {
  sym <- c("-", "\\", "|", "/")
  i <- 0L
  n <- length(sym)
  function() {
    sym[[i <<- if (i >= n) 1L else i + 1L]]
  }
}

pb_progress_message <- function(self, private, ..., domain, appendLF) {

  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)

  cond <- structure(
    list(message = msg, call = NULL),
    class = c(private$message_class, "message", "condition"))

  defaultHandler <- function(c) {
    cat(conditionMessage(c), file = stderr(), sep = "")
  }

  withRestarts({
    signalCondition(cond)
    defaultHandler(cond)
  }, muffleMessage = function() NULL)

  invisible()
}

pb_clear_line <- function(self, private, width) {
  str <- paste0(c("\r", rep(" ", width)), collapse = "")
  private$progress_message(str, appendLF = FALSE)
}

pb_cursor_to_start <- function(self, private) {
  private$progress_message("\r", appendLF = FALSE)
}
