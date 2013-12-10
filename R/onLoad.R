.onLoad <- function(libname, pkgname){
  
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
  
  #Update ggplot2 Methods.
  suppressWarnings(.push_tweaks())
  
  #Global Setting.
  options("tern.discard.external"=TRUE)
}