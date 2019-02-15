
server_init <- function() {
  progress_env$pb_active <- FALSE
  progress_env$stack <- list()
}

#' @export

with_progress <- function(expr) {
  if (progress_env$pb_active) return()
  progress_env$pb_active <- TRUE
  on.exit(progress_env$pb_active <- FALSE, add = TRUE)
  withCallingHandlers(
    expr,
    progress_message = function(msg) handle_progress_message(msg)
  )
}

handle_progress_message <- function(msg) {
  if (!check_msg(msg)) return()
  switch(
    msg$command,
    "add_job" = msg_add_job(msg),
    "add_job_group" = msg_add_job_group(msg),
    "set_job_progress" = msg_set_job_progress(msg),
    "add_job_progress" = msg_add_job_progress(msg),
    "set_job_status" = msg_set_job_status(msg),
    "set_job_estimate" = msg_set_job_estimate(msg),
    "add_job_output" = msg_add_job_output(msg),
    "complete_job" = msg_complete_job(msg),
    "complete_process" = msg_complete_process(msg),
    warning("Unknown progress message: ", msg$type)
  )
}

check_msg <- function(msg) {
  if (is.null(msg$command)) {
    warning("Invalid progress message, no `command`")
    FALSE
  } else if (is.null(msg$version)) {
    warning("Invalid progress message, no `version`")
    FALSE
  } else if (package_version(progress_env$version) < msg$version) {
    once("invalid_version",
         warning("Need progress server version ", msg$version))
    FALSE
  } else if (!is_string(msg$id)) {
    warning("Invalid progress bar id, must be a string")
    FALSE
  } else {
    TRUE
  }
}

msg_add_job <- function(msg) {
  ## TODO: duplicate id?
  progress_env$stack[[msg$id]] <- msg
  maybe_update(msg$id)
}

msg_add_job_group <- function(msg) {
  TODO
}

msg_set_job_progress <- function(msg) {
  if (is.null(progress_env$stack[[msg$id]])) return()
  progress_env$stack[[msg$id]]$progress <- msg$progress
  maybe_update(msg$id)
}

msg_add_job_progress <- function(msg) {
  if (is.null(progress_env$stack[[msg$id]])) return()
  progress_env$stack[[msg$id]]$progress <-
    progress_env$stack[[msg$id]]$progress + msg$increment
  maybe_update(msg$id)
}

msg_set_job_status <- function(msg) {
  if (is.null(progress_env$stack[[msg$id]])) return()
  progress_env$stack[[msg$id]]$status <- msg$status
  maybe_update(msg$id)
}

msg_set_job_estimate <- function(msg) {
  if (is.null(progress_env$stack[[msg$id]])) return()
  progress_env$stack[[msg$id]]$estimate <- msg$seconds
  maybe_update(msg$id)
}

msg_add_job_output <- function(msg) {
  TODO
}

msg_complete_job <- function(msg) {
  if (is.null(progress_env$stack[[msg$id]])) return()
  progress_env$stack[[msg$id]]$complete <- msg
  maybe_update(msg$id)
}

msg_complete_process <- function(msg) {
  pids <- vapply(progress_env$stack, "[[", integer(1), "pid")  
  ww <- which(pids == msg$pid)
  for (w in ww) progress_env$stack[[w]]$complete <- msg
  maybe_update(id = vapply(progress_env$stack[ww], "[[", character(1), "id"))
}

maybe_update <- function(ids) {
  ## TODO
}
