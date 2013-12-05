.onLoad <- function(libname, pkgname){
  library(scales)
  
  # do whatever needs to be done when the package is loaded
  # some people use it to bombard users with 
  # messages using 
  packageStartupMessage("")
  packageStartupMessage("+--------------------------------------------------------+")
  packageStartupMessage("|             Ternary Plots in R using ggplot2           |")
  packageStartupMessage("+--------------------------------------------------------+")
  packageStartupMessage("| Features Include:                                      |")
  packageStartupMessage("|  + Extension to ggplot2 for ternary diagrams           |")
  packageStartupMessage("|                                                        |")
  packageStartupMessage("+--------------------------------------------------------+")
  
  #Hack Hackidie hack hack hack.
  .theme_hack()
  .aes_hack()
  .utilities_hack()
  
  options("tern.discard.external"=TRUE)
}