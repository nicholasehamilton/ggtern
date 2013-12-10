
#'Create ggtern object
#'
#'\code{ggtern} is a function, analogous to \code{ggplot}, which creates an object of class \code{ggtern} (which inherits \code{ggplot}), 
#'and creation of a ternary plot is predicated on using this constructor.
#'@param ... same arguments as passed through to ggplot
#'@seealso \code{\link{ggplot}}
#'@rdname ggtern-constructor
#'@export
ggtern <- function(...){ggplot(...) + coord_tern()}







