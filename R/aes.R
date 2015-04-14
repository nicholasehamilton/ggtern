

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
  do.call(find_global_tern(".rename_aes"),args=list(x=X))
}
# Rename American or old-style aesthetics name
.rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]
  rename(x, find_global_tern(".base_to_ggplot"), warn_missing = FALSE)
}
.all_aesthetics <- c(ggint$.all_aesthetics,"T","L","R","zend")