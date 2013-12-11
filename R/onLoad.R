.onLoad <- function(libname, pkgname){
  suppressWarnings(.push_tweaks())
  options("tern.discard.external"=TRUE)
}