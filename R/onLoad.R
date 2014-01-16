.onLoad <- function(libname, pkgname){
  #ggtern options.
  options("tern.discard.external"      = TRUE)
  options("tern.clockwise"             = TRUE)
  options("tern.showarrows"            = TRUE)
  options("tern.ticks.outside"         = TRUE)
  options("tern.ticks.showprimary"     = TRUE)
  options("tern.ticks.showsecondary"   = FALSE)
  options("tern.pip.tollerance"        = 0.01)
  options("tern.breaks.default"        = seq(0.1, 1.0,by=0.1))
  options("tern.breaks.default.minor"  = seq(0.05,1.0,by=0.05))
  
  #Set the theme and the last coordinates.
  theme_set(theme_gray())
  set_last_coord(NULL)
}