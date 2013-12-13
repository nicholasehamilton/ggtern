

#' Modified Aesthetic Mappings
#' 
#' An extension to the base aes functin from ggplot2, this is modified to handle a default z mapping for application in ternary phase diagrams.
#' Does not alter the standard behaviour. 
#' @param x x value
#' @param y y value
#' @param z z value
#' @param ... other arguments as per \code{\link[ggplot2]{aes}}
#' @seealso Parent \code{\link[ggplot2]{aes}} function.
aes <- function(x,y,z,...) {
  X <- structure(as.list(match.call()[-1]), class="uneval")
  do.call(find_global("rename_aes"),args=list(x=X))
}

.all_aesthetics <- c(ggint$.all_aesthetics,"T","L","R","zend")