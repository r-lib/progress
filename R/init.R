
progress_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  progress_env$version <- "1.0.0"
  client_init()
  server_init()
}
