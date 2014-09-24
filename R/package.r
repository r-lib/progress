
.onLoad <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows") {
    signs <<- signs_win
  }
  invisible()
}
