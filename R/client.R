
#' @export

job_add <- function(name = NULL, id = NULL, status = NULL, total = 100,
                    type = c("iterator", "tasks", "download", "custom"),
                    format = NULL, estimate = NULL,
                    auto_estimate = TRUE, tokens = list(), group = NULL) {
  id <- id %||% generate_id()
  throw_cond("add_job", name = name, id = id, status = status,
             total = total, type = match.arg(type), format = format,
             estimate = estimate, auto_estimate = auto_estimate,
             tokens = tokens, group = group)
  invisible(id)
}

#' @export

job_add_group <- function(name = NULL, id = NULL,
                          type = c("gtasks", "gdownloads", "custom"),
                          format = NULL, status = NULL, tokens = list()) {
  id <- id %||% generate_id()
  throw_cond("add_job_group", name = name, id = id, type = match.arg(type),
             format = format, status = status, tokens = tokens)
  invisible(id)
}

#' @export

job_set_progress <- function(progress, id = NULL, tokens = list()) {
  throw_cond("set_job_progress",  progress = progress, id = id,
             tokens = tokens)
  invisible(id)
}

#' @export

job_add_progress <- function(increment = 1L, id = NULL, tokens = list()) {
  throw_cond("add_job_progress", increment = increment, id = id,
             tokens = tokens)
  invisible(id)
}

#' @export

job_set_status <- function(status, id = NULL, tokens = list()) {
  throw_cond("set_job_status", status = status, id = id, tokens = tokens)
  invisible(id)
}

#' @export

job_set_estimate <- function(seconds, id = NULL, tokens = list()) {
  throw_cond("set_job_estimate", seconds = seconds, id = id, tokens = tokens)
  invisible(id)
}

#' @export

job_add_output <- function(output, id = NULL,
                           output_type = c("message", "warning"),
                           tokens = list()) {
  throw_cond("add_job_output", output = output, id = id,
             output_type = match.arg(output_type), tokens = tokens)
  invisible(id)
}

#' @export

job_complete <- function(succeeded = TRUE, output = NULL, error = NULL,
                         id = NULL, tokens = list()) {
  throw_cond("complete_job", succeeded = succeeded, output = output,
             error = error, id = id, tokens = tokens)
  invisible(id)
}

#' @export

job_complete_process <- function(pid, succeeded = FALSE, error = NULL,
                                 tokens = list()) {
  throw_cond("complete_process", pid = pid, succeeded = succeeded,
             error = error, tokens = tokens)
  invisible()
}

client_init <- function() {
  progress_env$client_version <- "1.0.0"
  progress_env$pid <- Sys.getpid()
  progress_env$lastid <- 0L
}

throw_cond <- function(msgtype, ...) {
  cond <- list(...)
  cond$msgtype <- msgtype
  cond$version <- progress_env$client_version
  cond$pid <- progress_env$pid
  class(cond) <-  c("progress_message", "callr_message", "condition")

  handle_here <- function(msg) {
    handle_progress_message(msg)
  }

  withRestarts({
    signalCondition(cond)
    handle_here(cond)
  }, muffleProgress = function(...) NULL)

  invisible()
}
