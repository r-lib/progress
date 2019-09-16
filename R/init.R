
progress_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  server_init()
  client_init()
}
