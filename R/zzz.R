# zzz.R
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "www/img",
      package = "gglearn2"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}
