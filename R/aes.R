

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

.all_aesthetics <- ggplot2:::.all_aesthetics
.all_aesthetics <- c(.all_aesthetics,"T","L","R","zend")