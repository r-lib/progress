
#' @export

add_job <- function(name = NULL, id = NULL, status = NULL, total = 100,
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

add_job_group <- function(name = NULL, id = NULL,
                          type = c("gtasks", "gdownloads", "custom"),
                          format = NULL, status = NULL, tokens = list()) {
  id <- id %||% generate_id()
  throw_cond("add_job_group", name = name, id = id, type = match.arg(type),
             format = format, status = status, tokens = tokens)
  invisible(id)
}

#' @export

set_job_progress <- function(progress, id = NULL, tokens = list()) {
  throw_cond("set_job_progress",  progress = progress, id = id,
             tokens = tokens)
  invisible(id)
}

#' @export

add_job_progress <- function(increment = 1L, id = NULL, tokens = list()) {
  throw_cond("add_job_progress", increment = increment, id = id,
             tokens = tokens)
  invisible(id)
}

#' @export

set_job_status <- function(status, id = NULL, tokens = list()) {
  throw_cond("set_job_status", status = status, id = id, tokens = tokens)
  invisible(id)
}

#' @export

set_job_estimate <- function(seconds, id = NULL, tokens = list()) {
  throw_cond("set_job_estimate", seconds = seconds, id = id, tokens = tokens)
  invisible(id)
}

#' @export

add_job_output <- function(output, id = NULL,
                           output_type = c("message", "warning"),
                           tokens = list()) {
  throw_cond("add_job_output", output = output, id = id,
             output_type = match.arg(output_type), tokens = tokens)
  invisible(id)
}

#' @export

complete_job <- function(succeeded = TRUE, output = NULL, error = NULL,
                         id = NULL, tokens = list()) {
  throw_cond("complete_job", succeeded = succeeded, output = output,
             error = error, id = id, tokens = tokens)
  invisible(id)
}

#' @export

complete_process <- function(pid, succeeded = FALSE, error = NULL,
                             tokens = list()) {
  throw_cond("complete_process", pid = pid, succeeded = succeeded,
             error = error, tokens = tokens)
  invisible()
}

client_init <- function() {
  progress_env$pid <- Sys.getpid()
  progress_env$lastid <- 0L
}

generate_id <- function() {
  id <- progress_env$lastid <- progress_env$lastid + 1L
  as.character(id)
}

throw_cond <- function(msgtype, ...) {
  cond <- list(...)
  cond$msgtype <- msgtype
  cond$version <- progress_env$version
  cond$pid <- progress_env$pid
  class(cond) <-  c("progress_message", "callr_message", "condition")

  default_handler <- function(msg) {
    handle_progress_message(msg)
  }

  withRestarts({
    signalCondition(cond)
    progress_default_handler(cond)
  },
}
