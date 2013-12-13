#' Change Axis labels and legend titles
#' 
#' New label modification functions, equivalent to the original functions in ggplot2. 
#' 
#' NB: \code{Tlab} and \code{\link{xlab}} are equivalent, 
#' as is \code{Llab} and \code{\link{ylab}}, and \code{Rlab} and \code{zlab}.
#' \code{Wlab} changes the ternary arrow suffix. (ie atomic percent, weight percent etc.)
#' @param label the label value to change
#' @rdname labsnew
#' @name labsnew
#' @seealso \code{\link[ggplot2]{labs}}
#' @examples
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'         geom_point() + 
#'         xlab("ABC")  + 
#'         ylab("DEF")  + 
#'         zlab("GHI")
#' plot
#' @export
NULL

#' @rdname labsnew
#' @export
zlab <- function(label){labs(z=label)}

#' @rdname labsnew
#' @export
Tlab <- function(label){labs(T=label)}

#' @rdname labsnew
#' @export
Llab <- function(label){labs(L=label)}

#' @rdname labsnew
#' @export
Rlab <- function(label){labs(R=label)}

#' @rdname labsnew
#' @export
Wlab <- function(label){labs(W=label)}