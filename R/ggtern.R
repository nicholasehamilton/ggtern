
#'Create ggtern object
#'
#'\code{ggtern} is a function, analogous to \code{ggplot}, which creates an object of class \code{ggtern} (which inherits \code{ggplot}).
#'
#'This method is a convenience wrapper for \code{ggplot(...) + coord_tern()}
#'@param ... same arguments as passed through to ggplot
#'@inheritParams ggplot2::ggplot
#'@seealso \code{\link{ggplot}}
#'@rdname ggtern_constructor
#'@name   ggtern_constructor
#'@alias  ggtern_constructor
#'@examples
#'#Dummy Data.
#'DATA <- data.frame(x=1,y=1,z=1)
#'
#'#Method 1
#'plot <- ggtern(data=DATA,aes(x,y,z)) + geom_point()
#'plot
#'
#'#Method 2
#'plot <- ggplot(data=DATA,aes(x,y,z)) + geom_point() + coord_tern()
#'plot
#'
#'@export
ggtern <- function(...){ggplot(...) + coord_tern()}







