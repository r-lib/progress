
#' Progress bar on the terminal
#'
#'
#' @importFrom R6 R6Class
#'
#' @export
#' @examples
#'
#' pb <- progress_bar$new(total = 100)
#' for (i in 1:100) {
#'   pb$tick()
#'   Sys.sleep(i / 100 * 5)
#' }
#'

progress_bar <- R6Class("progress_bar",

  public = list(

    initialize = function(format = "[:bar] :percent", total = 100,
      width = getOption("width"), stream = stderr(), complete = "=",
      incomplete = "-", callback = function(self) {}, clear = TRUE) {
        pb_init(self, private, format, total, width, stream, complete,
          incomplete, callback, clear)
    },
    tick = function(len = 1) { pb_tick(self, private, len) },
    update = function(ratio) { pb_update(self, rivate, ratio) }
  ),

  private = list(

    render = function() { pb_render(self, private) },
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

pb_tick <- function(self, private, len) {

  assert_positive_scalar(len)

  if (private$current == 0) private$start = Sys.time()

  private$current <- private$current + len

  private$render()

  if (private$current > private$total ||
      isTRUE(all.equal(private$current, private$total))) {
    private$complete <- TRUE
    private$terminate()
    private$callback(self)
  }

  self
}

pb_render <- function(self, private) {
  if (!isatty(private$stream)) return(invisible())

  ratio <- (private$current / private$total) %>%  max(0) %>% min(1)
  percent <- ratio * 100
  elapsed <- Sys.time() %>% subtract(private$start) %>% as.vector()
  eta <- if (isTRUE(all.equal(percent, 100))) {
    0
  } else {
    elapsed * (private$total / private$current - 1.0)
  }

  str <- private$format %>%
    sub(pattern = ":current", replacement = round(private$current)) %>%
    sub(pattern = ":total", replacement = round(private$total)) %>%
    sub(pattern = ":elapsed", replacement = round(elapsed, 1)) %>%
    sub(pattern = ":eta", replacement = round(eta, 1)) %>%
    sub(pattern = ":percent", replacement = paste0(round(percent), "%"))

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

  if (private$last_draw != str) {
    clear_line(private$stream, private$width)
    cursor_to_start(private$stream)
    cat(str, file = private$stream)
    private$last_draw <- str
  }

  self
}

pb_update <- function(self, rivate, ratio) {
  assert_ratio(ratio)
  goal <- floor(ratio * private$total)
  private$tick(goal - private$current)
}

pb_terminate <- function(self, private) {
  if (private$clear) {
    clear_line(private$stream, private$width)
    cursor_to_start(private$stream)
  }
}
