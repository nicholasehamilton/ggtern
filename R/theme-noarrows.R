.theme_arrows <- function(show){theme(axis.tern.showarrows = show)}

#' Show or Hide the Ternary Arrows
#' 
#' \code{theme_noarrows} is a function that appends to the current theme a flag to switch OFF the ternary arrows
#' @rdname themeshowhidearrows
#' @export
theme_noarrows   <- function(){.theme_arrows(FALSE)}

#' \code{theme_hidearrows} is an alias for \code{theme_noarrows}
#' @rdname themeshowhidearrows
#' @export
theme_hidearrows <- theme_noarrows

#' \code{theme_showarrows} is a function that appends to the current theme a flag to switch ON the ternary arrows
#' @rdname themeshowhidearrows
#' @export
theme_showarrows <- function(){.theme_arrows(TRUE)}