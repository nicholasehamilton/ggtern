#' Change Axis labels and legend titles
#' 
#' New label modification functions, equivalent to the original functions in ggplot2. 
#' 
#' NB: \code{Tlab} and \code{\link{xlab}} are equivalent, 
#' as is \code{Llab} and \code{\link{ylab}}, and \code{Rlab} and \code{zlab}.
#' \code{Wlab} changes the ternary arrow suffix. (ie atomic percent, weight percent etc.)
#' @param label the label value to change
#' @rdname ggtern-labels
#' @name labels
#' @seealso \code{\link[ggplot2]{labs}}
#' @examples
#' \donttest{
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'         geom_point() + 
#'         xlab("ABC")  + 
#'         ylab("DEF")  + 
#'         zlab("GHI")
#' plot
#' }
#' @export
NULL

#' @rdname ggtern-labels
#' @export
zlab <- function(label){labs(z=label)}

#' @rdname ggtern-labels
#' @export
Tlab <- function(label){labs(T=label)}

#' @rdname ggtern-labels
#' @export
Llab <- function(label){labs(L=label)}

#' @rdname ggtern-labels
#' @export
Rlab <- function(label){labs(R=label)}

#' @rdname ggtern-labels
#' @export
Wlab <- function(label){labs(W=label)}