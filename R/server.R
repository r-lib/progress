
server_init <- function() {
  progress_env$handlers <- new.env(parent = emptyenv())
  progress_env$server_version <- "1.0.0"
}

check_msg <- function(msg) {
  if (is.null(msg$msgtype)) {
    warning("Invalid progress message, no `msgtype`")
    FALSE
  } else if (is.null(msg$version)) {
    warning("Invalid progress message, no `version`")
    FALSE
  } else if (package_version(progress_env$server_version) < msg$version) {
    # TODO: silently ignore?
    FALSE
  } else if (!is_string(msg$id)) {
    warning("Invalid progress bar id, must be a string")
    FALSE
  } else {
    TRUE
  }
}

handle_progress_message <- function(msg) {
  if (!check_msg(msg)) return()

  # Run default, unless opted out
  if (!isTRUE(getOption("progress_omit_default_handler"))) {
    run_handler(choose_default_handler(), msg)
  }

  # Run others as well
  hs <- getOption("progress_handlers_override", progress_env$handlers)

  # TODO: ignore warnings and errors?
#  tryCatch(
    withCallingHandlers(
      eapply(hs, run_handler, msg),
      warning = function(w) NULL
    )#,
#    error = function(e) NULL
#  )

  invisible()
}

choose_default_handler <- function() {
  # TODO: add others
  progress_handler_cli()
}

run_handler <- function(handler_record, msg) {
  handler <- handler_record$handler
  if (msg$msgtype %in% names(handler)) {
    handler[[msg$msgtype]](msg)
  } else if ("*" %in% names(handler)) {
    handler[["*"]](msg)
  }
}
