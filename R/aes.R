

#' Modified Aesthetic Mappings
#' 
#' An extension to the base aes functin from ggplot2, this is modified to handle a default z mapping for application in ternary phase diagrams.
#' Does not alter the standard behaviour. 
#' @param x x value
#' @param y y value
#' @param z z value
#' @seealso Parent \code{\link[ggplot2]{aes}} function.
aes <- function(x,y,z=NULL,...) {
  aes <- structure(as.list(match.call()[-1]), class="uneval")
  ggplot2:::rename_aes(aes)
}

.aes_hack <- function(){
  # Look up the scale that should be used for a given aesthetic
  #aes_to_scale <- function(var) {
  #  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  #  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  #  var[var %in% c("T", "Tmin", "Tmax", "Tend", "Tintercept")] <- "T"
  #  var[var %in% c("L", "Lmin", "Lmax", "Lend", "Lintercept")] <- "L"
  #  var[var %in% c("R", "Rmin", "Rmax", "Rend", "Rintercept")] <- "R"
  #  var
  #}
  #unlockBinding("aes_to_scale", asNamespace("ggplot2"))
  #  assign("aes_to_scale", aes_to_scale, asNamespace("ggplot2"))
  #lockBinding("aes_to_scale", asNamespace("ggplot2"))
  
  # Figure out if an aesthetic is a position aesthetic or not
  #is_position_aes <- function(vars) {
  #  aes_to_scale(vars) %in% c("x", "y","T","L","R")
  #}
  #unlockBinding("is_position_aes", asNamespace("ggplot2"))
  #  assign("is_position_aes", is_position_aes, asNamespace("ggplot2"))
  #lockBinding("is_position_aes", asNamespace("ggplot2"))
  
  
  .all_aesthetics <- ggplot2:::.all_aesthetics
  .all_aesthetics <- c(.all_aesthetics,"T","L","R","zend")
  #PUSH BACK.
  unlockBinding(".all_aesthetics", asNamespace("ggplot2"))
    assign(".all_aesthetics", .all_aesthetics, asNamespace("ggplot2"))
  lockBinding(".all_aesthetics", asNamespace("ggplot2"))
}