
new_handler_record <- function(id, handler, data = NULL, ...) {
  config <- list(id = id, ...)
  list(handler = handler, data = data, config = config)
}

#' @export

handler_add <- function(handler, id = NULL, data = NULL) {
  id <- id %||% generate_id()
  progress_env$handlers[[id]] <- new_handler_record(
    id,
    handler,
    data = data
  )
}

#' @export

handler_list <- function() {
  # TODO: pretty print this
  as.list(progress_env$handlers)
}
