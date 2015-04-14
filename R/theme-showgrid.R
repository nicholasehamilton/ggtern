.theme.showgrid.major <- function(show){theme(axis.tern.showgrid.major = show)}
.theme.showgrid.minor <- function(show){theme(axis.tern.showgrid.minor = show)}

#' Show or Hide Grid
#' 
#' A set of convenience functions to enable or disable the use of major or minor (or both) gridlines. 
#' 
#' These flags operate at the 'rendering' level, and, supercede the presence of theme elements, therefore, 
#' \code{theme_hidegrid(...)} or its aliases will PREVENT rendering of grid elements, 
#' irrespective of whether those grid elements are valid (renderable). From the counter perspective, 
#' \code{theme_showgrid(...)} or its aliases will ALLOW rendering of grid elements, subject to those grid 
#' elements being valid (renderable, ie say \code{\link{element_line}} as opposed to \code{\link{element_blank}}).  
#' @rdname showhidegrid
#' @name theme_showgrid
#' @examples
#'   #Load data
#'   data(Feldspar)
#'   plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + 
#'           geom_point()   + #Layer 
#'           theme_bw()     + #For clarity
#'           theme_hidegrid() #Turn off both major and minor
#'   plot
#'   
#'   #Demonstrate switching on major, minor and both gridlines
#'   plot + theme_showgrid_minor() #show minor only
#'   plot + theme_showgrid_major() #show major only
#'   plot + theme_showgrid()       #show both major and minor
#'   
#'   #Demonstrate switching OFF major, minor and both gridlines, Uncomment to run
#'   #plot <- plot + theme_showgrid() #as before
#'   #plot + theme_hidegrid_minor() #show major only (hide minor)
#'   #plot + theme_hidegrid_major() #show minor only (hide major)
#'   #plot + theme_hidegrid()       #show none (hide both major and minor)
NULL

#' \code{theme_showgrid} is a function which \strong{enables} both MAJOR and MINOR gridlines.
#' @rdname showhidegrid
#' @export
theme_showgrid       <- function(){theme_showgrid_major() + theme_showgrid_minor()}

#' \code{theme_hidegrid} or \code{theme_nogrid} (alias) is a function which \strong{disables} both MAJOR and MINOR gridlines.
#' @aliases theme_nogrid theme_tern_nogrid
#' @rdname showhidegrid
#' @export
theme_hidegrid       <- function(){theme_hidegrid_major() + theme_hidegrid_minor()}
theme_nogrid <- theme_hidegrid
theme_tern_nogrid <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid() has been superceded by theme_nogrid()")
  theme_nogrid()
}

#' \code{theme_showgrid_major} is a function which \strong{enables} MAJOR gridlines.
#' @rdname showhidegrid
#' @export
theme_showgrid_major <- function(){.theme.showgrid.major(TRUE)}

#' \code{theme_hidegrid_major} or \code{theme_nogrid_major} (alias) is a function which \strong{disables} MAJOR gridlines.
#' @aliases theme_nogrid_major theme_tern_nogrid_major
#' @rdname showhidegrid
#' @export
theme_hidegrid_major <- function(){.theme.showgrid.major(FALSE)}
theme_nogrid_major <- theme_hidegrid_major
theme_tern_nogrid_major <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_major() has been superceded by theme_nogrid_major()")
  theme_nogrid_major()
}

#' \code{theme_showgrid_major} is a function which \strong{enables} MINOR gridlines.
#' @rdname showhidegrid
#' @export
theme_showgrid_minor <- function().theme.showgrid.minor(TRUE)

#' \code{theme_hidegrid_minor} or \code{theme_nogrid_minor} (alias) is a function which \strong{disables} MINOR gridlines.
#' @aliases theme_nogrid_minor theme_tern_nogrid_minor
#' @rdname showhidegrid
#' @export
theme_hidegrid_minor <- function(){.theme.showgrid.minor(FALSE)}
theme_nogrid_minor <- theme_hidegrid_minor
theme_tern_nogrid_minor <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_minor() has been superceded by theme_nogrid_minor()")
  theme_nogrid_minor()
}

