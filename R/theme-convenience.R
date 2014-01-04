#' Theme Convenience Functions
#' 
#' @description
#' \code{ggtern} has made available a number of convenience functions for rapid tweaking of the various theme elements, for a full list of the available 
#' theme elements which can be manually modified, see \link[=themeelements]{HERE}.
#' 
#' @section Default Themes:
#' Default themes which ship with \code{ggtern}:
#' \itemize{
#'   \item \code{\link[=theme_tern_bw]{Black and White Theme}}
#'   \item \code{\link[=theme_tern_gray]{Gray and White Theme}} 
#'   \item \code{\link[=theme_tern_rgbw]{RGB and White Theme}}
#'   \item \code{\link[=theme_tern_rgbg]{RGB and Gray Theme}}
#' }
#' 
#' @section Convenience Functions:
#' Convenience functions that ship with \code{ggtern}, to assist in the rapid modification of key theme elements:
#' \itemize{
#'   \item \code{\link[=theme_noarrows]{Show/Hide Arrows}} 
#'   \item \code{\link[=theme_nogrid]{Show/Hide Grids}}
#'   \item \code{\link[=theme_clockwise]{Clockwise/Anticlockwise Axis Precession}}  
#'   \item \code{\link[=theme_ticksoutside]{Ticks Inside or Outside of the Main Plot Area}}
#'   \item \code{\link[=theme_noticks]{Show or Hide Primary or Secondary Ticks.}}
#'   \item \code{\link[=atomic_percent]{Atomic or Weight Percent Arrow Label Suffix.}}
#' }
#' 
#' @section Manual Modification:
#' For manual modification on a per-element basis:
#' \itemize{
#'   \item \code{\link[=themeelements]{Ternary Theme Elements}} 
#' }
#' 
#' @aliases modifynewthemes modifyterntheme convenience-functions
#' @name convenience
#' @rdname convenience
#' @examples
#' \donttest{
#' #Load data and create the base plot.
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
#' 
#' #Default Themes
#' plot + theme_rgbg()
#' plot + theme_rgbw()
#' plot + theme_bw()
#' plot + theme_gray()
#' 
#' #Show or Hide Arrows
#' plot + theme_showarrows()
#' plot + theme_noarrows()
#' 
#' #Major/Minor Grids?
#' plot + theme_nogrid_minor()
#' plot + theme_nogrid_major()
#' plot + theme_nogrid()
#' 
#' #Clockwise/Anticlockwise Precession
#' plot + theme_clockwise()
#' plot + theme_anticlockwise()
#' 
#' #Ticks Inside or Outside
#' plot + theme_ticksoutside()
#' plot + theme_ticksinside()
#' 
#' #Show/Hide BOTH Primary and Secondary Ticks
#' plot + theme_showticks()
#' plot + theme_hideticks()
#' 
#' #Show/Hide EITHER Primary OR Secondary Ticks.
#' plot + theme_showprimary() + theme_hidesecondary()
#' plot + theme_hideprimary() + theme_showsecondary()
#' 
#' #Atomic / Weight Percent
#' plot + theme_showarrows() + atomic_percent()
#' plot + theme_showarrows() + weight_percent()
#' }
#' @export
NULL



