
#' Modify Ternary Theme Elements
#' 
#' Convenience functions to assist in the rapid modification of key theme elements.
#' @aliases modifynewthemes modifyterntheme convenience
#' @name modifyterntheme
#' @rdname modifyterntheme
#' @examples 
#' data(Feldspar)
#' \donttest{
#' plot <- ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
#' plot + theme_showarrows()
#' plot + theme_noarrows()
#' plot + theme_showarrows()
#' plot + theme_tern_rgbg()
#' plot + theme_tern_rgbw()
#' plot + theme_tern_bw()
#' plot + theme_tern_gray()
#' plot + theme_tern_nogrid_minor()
#' plot + theme_tern_nogrid_major()
#' plot + theme_tern_nogrid()
#' }
#' @export
NULL

.theme_arrows <- function(show){
  if(!is.logical(show)){show=TRUE}
  show=show[1]
  current <- theme_update()
  e <- current$ternary.options
  if(inherits(e,"element_ternary")){
    e$showarrows <- show
    return(theme(ternary.options=e))
  }else{
    ggtern::theme(ternary.options=element_ternary(showarrows=show))
  }
}

#' \code{theme_noarrows} is a function that apepnds to the current theme a flag to switch OFF the ternary arrows
#' @rdname modifyterntheme
#' @export
theme_noarrows   <- function(){.theme_arrows(FALSE)}

#' \code{theme_showarrows} is a function that apepnds to the current theme a flag to switch ON the ternary arrows
#' @rdname modifyterntheme
#' @export
theme_showarrows <- function(){.theme_arrows(TRUE)}

#' \code{theme_tern_rgbg} ternary theme, red green blue with gray background
#' @rdname modifyterntheme
#' @export
theme_tern_rgbg  <- function(){.theme_tern(col.BG="gray90")}

#' \code{theme_tern_rgbw} ternary theme, red green blue with white background
#' @rdname modifyterntheme
#' @export
theme_tern_rgbw  <- function(){.theme_tern(col.BG="white")}

#' \code{theme_tern_bw} ternary theme black and white
#' @rdname modifyterntheme
#' @export
theme_tern_bw    <- function(){.theme_tern("white","black","black","black")}

#' \code{theme_tern_gray} ternary theme, gray theme
#' @rdname modifyterntheme
#' @export
theme_tern_gray  <- function(){.theme_tern(col.BG="grey90",col.T="black",col.L="black",col.R="black")}

#' \code{theme_tern_nogrid} ternary theme, no minor grids.
#' @rdname modifyterntheme
#' @export
theme_tern_nogrid_minor <- function(){
  theme(panel.grid.tern.minor=element_blank(),
        panel.grid.tern.minor.T=element_blank(),
        panel.grid.tern.minor.L=element_blank(),
        panel.grid.tern.minor.R=element_blank()
  ) 
}

#' \code{theme_tern_nogrid} ternary theme, no major grids.
#' @rdname modifyterntheme
#' @export
theme_tern_nogrid_major <- function(){
  theme(panel.grid.tern.major=element_blank(),
        panel.grid.tern.major.T=element_blank(),
        panel.grid.tern.major.L=element_blank(),
        panel.grid.tern.major.R=element_blank()
  ) 
}

#' \code{theme_tern_nogrid} ternary theme, no major or minor grids.
#' @rdname modifyterntheme
#' @export
theme_tern_nogrid <- function(){
  list(
    theme_tern_nogrid_minor(),
    theme_tern_nogrid_major()
  )
}

#' \code{theme_clockwise} is an alias for \code{\link{tern_clockwise}}
#' @rdname modifyterntheme
#' @export
theme_clockwise <- function(){tern_clockwise()}
#' \code{theme_anticlockwise} is an alias for \code{\link{tern_anticlockwise}}
#' @rdname modifyterntheme
#' @export
theme_anticlockwise <- function(){tern_anticlockwise()}
#' \code{theme_counterclockwise} is an alias for \code{\link{tern_counterclockwise}}
#' @rdname modifyterntheme
#' @export
theme_counterclockwise <- function(){tern_counterclockwise()}

