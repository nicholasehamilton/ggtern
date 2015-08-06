.onLoad <- function(libname, pkgname){
  #ggtern options.
  options("tern.discard.external"      = TRUE)
  options("tern.clockwise"             = TRUE)
  options("tern.showarrows"            = TRUE)
  options("tern.showtitles"            = TRUE)
  options("tern.showlabels"            = TRUE)
  options("tern.showgrid.major"        = TRUE)
  options("tern.showgrid.minor"        = TRUE)
  options("tern.ticks.outside"         = TRUE)
  options("tern.ticks.showprimary"     = TRUE)
  options("tern.ticks.showsecondary"   = FALSE)
  options("tern.expand"                = 0.005)
  options("tern.expand.contour.inner"  =-0.0005)
  options("tern.breaks.default"        = seq(0.2, 1.0,by=0.2))
  options("tern.breaks.default.minor"  = seq(0.1, 0.9,by=0.2))
  options("tern.dont_transform"        = FALSE)
  options("tern.default.T"             = "y")
  options("tern.default.L"             = "x")
  options("tern.default.R"             = "z")
  options("tern.arrowstart"            = 0.3)
  options("tern.arrowfinish"           = 0.7)
  options("tern.arrowbaseline"         = 2)
  options("tern.mesh.buffer"           = 1.50)
  options("tern.mesh.size"             = 200)
  
  #Set the theme and the last coordinates.
  theme_set(theme_gray())
  set_last_coord(NULL)
}

