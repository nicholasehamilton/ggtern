#' Theme Convenience Functions
#' 
#' Convenience functions to assist in the rapid modification of key theme elements.
#' @aliases modifynewthemes modifyterntheme convenience
#' @name modifyterntheme
#' @seealso \code{\link[=theme_noarrows]{Show/Hide Arrows}}, \code{\link[=theme_nogrid]{Show/Hide Grids}},  
#' \code{\link[=theme_clockwise]{Clockwise/Anticlockwise}},  
#' \code{\link[=themeelements]{Ternary Theme Elements}}, \code{\link[=theme_tern_bw]{Black and White Theme}},
#' \code{\link[=theme_tern_gray]{Gray and White Theme}}, \code{\link[=theme_tern_rgbw]{RGB and White Theme}},
#' \code{\link[=theme_tern_rgbg]{RGB and Gray Theme}}
#' @rdname modifyterntheme
#' @examples 
#' data(Feldspar)
#' \donttest{
#' plot <- ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
#' plot + theme_showarrows()
#' plot + theme_noarrows()
#' plot + theme_showarrows()
#' plot + theme_rgbg()
#' plot + theme_rgbw()
#' plot + theme_bw()
#' plot + theme_gray()
#' plot + theme_nogrid_minor()
#' plot + theme_nogrid_major()
#' plot + theme_nogrid()
#' plot + theme_clockwise()
#' plot + theme_anticlockwise()
#' }
#' @export
NULL



