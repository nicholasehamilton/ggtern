
#'Create ggtern object
#'
#'\code{ggtern} is a function, analogous to \code{ggplot}, which creates an object of class \code{ggtern} (which inherits \code{ggplot}), 
#'and creation of a ternary plot is predicated on using this constructor.
#'@param ... same arguments as passed through to ggplot
#'@seealso \code{\link{ggplot}}
#'@export
ggtern <- function(...){
  plot <- ggplot(...)
  
  theme_set(theme_tern_gray())
  plot <- plot + coord_tern() + theme_update()
  class(plot) <- c("ggtern",class(plot))
  plot
}

ggplot <- function(...){
  theme_set(theme_gray())
  ggplot2:::ggplot(...)
}







