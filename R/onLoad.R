.onLoad <- function(libname, pkgname){
  #suppressWarnings(.push_tweaks())
  options("tern.discard.external"=TRUE)
  options("tern.pip.tollerance"  =0.01)
}