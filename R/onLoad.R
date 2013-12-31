.onLoad <- function(libname, pkgname){
  options("tern.discard.external"      = TRUE)
  options("tern.pip.tollerance"        = 0.01)
  options("tern.breaks.default"        = seq(0.1,1.0,by=0.1))
  options("tern.breaks.default.minor"  = seq(0.1,1.0,by=0.05))
  set_last_coord(NULL)
}