.onLoad <- function(libname, pkgname){
  options("tern.discard.external"=TRUE)
  options("tern.pip.tollerance"  =0.01)
  set_last_coord(NULL)
}