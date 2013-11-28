.onLoad <- function(libname, pkgname){
  library(scales)
  
  # do whatever needs to be done when the package is loaded
  # some people use it to bombard users with 
  # messages using 
  packageStartupMessage("")
  packageStartupMessage("+--------------------------------------------------------+")
  packageStartupMessage("|             Ternary Plots in R via ggplot2             |")
  packageStartupMessage("+--------------------------------------------------------+")
  packageStartupMessage("| Features Include:                                      |")
  packageStartupMessage("|  + Extension to ggplot2 for ternary diagrams           |")
  packageStartupMessage("|  + New Geometries:                                     |")
  packageStartupMessage("|    + geom_point_tern                                   |")
  packageStartupMessage("|    + geom_path_tern                                    |")
  packageStartupMessage("|    + geom_polygon_tern                                 |")
  packageStartupMessage("|                                                        |")
  packageStartupMessage("+--------------------------------------------------------+")
  
  #Hack Hackidie hack hack hack.
  .theme_hack()
  #.build_hack()
  .aes_hack()
  #.panel_hack()
}