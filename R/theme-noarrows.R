.theme_arrows <- function(show){
  if(!is.logical(show)){show=TRUE}
  show=show[1]
  current <- theme_update()
  e <- current$ternary.options
  if(inherits(e,"element_ternary")){
    e$showarrows <- show
  }else{
    e <- element_ternary(showarrows=show)
  }
  #current %+replace% 
  theme(ternary.options=e)
}

#' Show or Hide the Ternary Arrows
#' 
#' \code{theme_noarrows} is a function that apepnds to the current theme a flag to switch OFF the ternary arrows
#' @rdname themeshowhidearrows
#' @export
theme_noarrows   <- function(){.theme_arrows(FALSE)}

#' \code{theme_hidearrows} is an alias for \code{theme_noarrows}
#' @rdname themeshowhidearrows
#' @export
theme_hidearrows <- theme_noarrows

#' \code{theme_showarrows} is a function that apepnds to the current theme a flag to switch ON the ternary arrows
#' @rdname themeshowhidearrows
#' @export
theme_showarrows <- function(){.theme_arrows(TRUE)}