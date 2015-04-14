#' Change Axis labels and legend titles
#' 
#' New label modification functions, equivalent to the original functions in ggplot2 (\code{\link{xlab}} and \code{\link{ylab}}) 
#' however for the new axes used in the \code{ggtern} package
#' 
#' \code{Tlab} and \code{\link{xlab}} are equivalent (when \code{T='x'} in the \code{\link{coord_tern}} definition), 
#' as is \code{Llab} and \code{\link{ylab}} (when \code{L='y'}) , and \code{Rlab} and \code{zlab} (when \code{R='z'}), for other
#' assignments when \code{coord_tern} is defined, the equivalence is not the case, however, if \code{T='XXX'}, 
#' then \code{Tlab} will be the same as \code{XXXlab} (where \code{XXX} can be substituted for \code{'x', 'y' or 'z'}, and likewise for
#' \code{Llab} and \code{Rlab}). 
#' 
#' \code{zlab} is new to \code{ggtern}, but is intended to be an analogous to \code{xlab} and \code{ylab} 
#' as per the definitions in \code{ggplot2}.
#' 
#' @section Arrow Label Suffix:
#' \code{Wlab} changes the ternary arrow suffix (ie atomic percent, weight percent etc) when the ternary arrows are enabled 
#' (see \code{\link{theme_showarrows}} and \code{\link{weight_percent}})
#' 
#' @section Precedence:
#' \code{AAAlab} takes precedence over \code{BBBlab} (where \code{AAA} represents \code{T, L or R} and \code{BBB} 
#' represents \code{x, y or z})
#' 
#' @section Use of Expressions:
#' Expressions can be used in the labels, in the event that the user wishes to render formula, subscripts or superscripts, see the last example below.
#' 
#' @section Creation of Aliasses:
#' Aliasses exist for \code{Tlab}, \code{Llab}, \code{Rlab} and \code{Wlab}, which are \code{tlab}, \code{llab}, \code{rlab} and \code{wlab}. 
#' These aliasses produce an identical result, and are there for convenience (as opposed to having an error thrown) 
#' in the event that the user forgets to use an upper-case letter.
#' 
#' @param label the desired label, usually as a \code{\link{character}} or \code{\link{expression}}, 
#' although other values can be inputed (such as, for example, scalar \code{\link{numeric}} or \code{\link{logical}}).
#' @aliases llab tlab rlab wlab
#' @rdname ggtern-labels
#' @name TLRWlab
#' @seealso ggplot2 \code{\link[ggplot2]{labs}}
#' @examples
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) +  geom_point() + 
#'         xlab("ABC") + ylab("DEF") + zlab("GHI")
#' 
#' #Alternatives, and Arrow Label
#' plot + Tlab("TOP") + Llab("LHS") + Rlab("RHS") + 
#'   theme_showarrows() + Wlab("WEIGHT")
#' @export
NULL

#' \code{Tlab} modifies the label of the TOP apex species
#' @rdname ggtern-labels
#' @export
Tlab <- function(label){labs(T=label)}
tlab <- Tlab

#' \code{Llab} modifies the label of the LHS apex species
#' @rdname ggtern-labels
#' @export
Llab <- function(label){labs(L=label)}
llab <- Llab

#' \code{Rlab} modifies the label of the RHS apex species
#' @rdname ggtern-labels
#' @export
Rlab <- function(label){labs(R=label)}
rlab <- Rlab

#' \code{Wlab} modifies the label of the arrow suffix
#' @rdname ggtern-labels
#' @export
Wlab <- function(label){labs(W=label)}
wlab <- Wlab

#' @rdname ggtern-labels
#' @export
zlab <- function(label){labs(z=label)}