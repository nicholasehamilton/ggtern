#' Complete Themes
#' 
#' \code{ggtern} ships with a number of complete themes:
#' \itemize{
#'   \item Black and White Theme: \code{\link[=theme_tern_bw]{theme_bw(...)}}
#'   \item Minimal Theme: \code{\link[=theme_tern_minimal]{theme_minimal(...)}}
#'   \item Classic Theme: \code{\link[=theme_tern_classic]{theme_classic(...)}}
#'   \item Gray and White Theme: \code{\link[=theme_tern_gray]{theme_gray(...)}} 
#'   \item Red, Green, Blue and White Theme: \code{\link[=theme_tern_rgbw]{theme_rgbw(...)}}
#'   \item Red, Green, Blue and Gray Theme: \code{\link[=theme_tern_rgbg]{theme_rgbg(...)}}
#' }
#' @rdname theme_complete
#' @name theme_complete
NULL

#' Theme Convenience Functions
#' 
#' @description
#' \code{ggtern} has made available a number of convenience functions for rapid tweaking of the various theme elements, 
#' for a full list of the available theme elements which can be manually modified, see \link[=themeelements]{HERE}.
#' 
#' @section Convenience Functions:
#' Convenience functions that ship with \code{ggtern}, to assist in the rapid modification of key theme elements:
#' \itemize{
#'   \item \code{\link[=theme_showtitles]{Show/Hide Axis Titles}}
#'   \item \code{\link[=theme_showarrows]{Show/Hide Arrows}} 
#'   \item \code{\link[=theme_showgrid]{Show/Hide Grids}}
#'   \item \code{\link[=theme_showprimary]{Show/Hide Primary/Secondary Ticks}}
#'   \item \code{\link[=theme_showlabels]{Show/Hide Axis Ticklabels}}
#'   \item \code{\link[=theme_clockwise]{Clockwise/Anticlockwise Axis Precession}}  
#'   \item \code{\link[=theme_ticksoutside]{Ticks Inside or Outside of the Main Plot Area}}
#'   \item \code{\link[=atomic_percent]{Atomic or Weight Percent Arrow Label Suffix.}}
#' }
#' 
#' @section Manual Modification:
#' For manual modification on a per-element basis:
#' \itemize{
#'   \item \code{\link[=theme_elements]{Ternary Theme Elements}} 
#' }
#' 
#' @section Default Themes:
#' Default (complete) themes which ship with \code{ggtern}:
#' \itemize{
#'   \item \code{\link[=theme_complete]{Complete Themes}} 
#' }
#' 
#' @aliases theme_convenience convenience convenience_functions
#' @name convenience_functions
#' @rdname convenience_functions
#' @examples
#' #Load data and create the base plot.
#' data(Feldspar)
#' plot <- ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
#' 
#' #Default Themes
#' plot + theme_rgbw()
#' #plot + theme_rgbg()
#' #plot + theme_bw()
#' #plot + theme_gray()
#' #plot + theme_custom(col.T="red",col.L="blue",col.R="black")
#' 
#' #Show or Hide Arrows
#' plot + theme_showarrows()
#' #plot + theme_hidearrows()
#' #plot + theme_noarrows()
#' 
#' #Major/Minor Grids?
#' plot + theme_nogrid_minor()
#' #plot + theme_nogrid_major()
#' #plot + theme_nogrid()
#' 
#' #Clockwise/Anticlockwise Precession
#' plot + theme_clockwise()
#' #plot + theme_anticlockwise()
#' 
#' #Ticks Inside or Outside
#' plot + theme_ticksoutside()
#' #plot + theme_ticksinside()
#' 
#' #Show/Hide BOTH Primary and Secondary Ticks
#' plot + theme_showticks()
#' #plot + theme_hideticks()
#' 
#' #Show/Hide EITHER Primary OR Secondary Ticks.
#' plot + theme_showprimary() + theme_hidesecondary()
#' #plot + theme_hideprimary() + theme_showsecondary()
#' 
#' #Atomic / Weight Percent
#' plot + theme_showarrows() + atomic_percent()
#' #plot + theme_showarrows() + weight_percent()
#' @export
NULL



