#Internal function
.theme_arrowsize <- function(s=getOption("tern.arrowstart"),f=getOption("tern.arrowfinish")){
  
  #Check numeric
  s = is.numericor(s,getOption("tern.arrowstart") )#[1]
  f= is.numericor(f,getOption("tern.arrowfinish"))#[1]
  
  #Execute
  theme(axis.tern.showarrows= (s!=f), axis.tern.arrowstart=s,axis.tern.arrowfinish=f)
}

#' @title
#' Change the Length of the Ternary Arrows
#' 
#' @description
#' A set of convenience functions to rapidly change the length of the ternary arrows, the convenience functions include presets 
#' (short, normal, long), or makes provision for the user to specify custom fractional starting and ending values relative to the
#' size of the ternary axis. In the event that the user elects to specify the values via the \code{theme_arrowcustomlength} (or its aliasses), 
#' then the user can specify a single scalar value which apply to all three (3) arrows, or, alternatively, can provide a numeric vector
#' of length three (3), one for each arrow respectively.
#' 
#' @details
#' If the ternary arrows are switched OFF 
#' (via the \code{\link{theme_hidearrows}} command, or the \code{theme(axis.tern.showarrows=FALSE)} theme element), then under such circumstance,
#' these convenience functions will turn ON the ternary arrows, essentially running \code{\link{theme_showarrows}} or \code{theme(axis.tern.showarrows=TRUE)}
#' 
#' @details
#' If for some reason, the \code{start} and \code{finish} arguments are identical, then the ternary arrows will be switched OFF, tantamount to
#' running the \code{\link{theme_hidearrows}} convenience function.
#' 
#' @seealso \code{\link{theme_arrowbaseline}} and \code{theme(\link{axis.tern.arrowsep}=X)} for methods to adjust the separation distance of the ternary arrows 
#' from the ternary axes.
#' @examples
#'  #Create base plot
#'  #plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + geom_point()
#' 
#'  #Pre-Specified Values
#'  plot + theme_arrowsmall()
#'  ## Alternatives, Uncomment lines below
#'  #plot + theme_arrownormal()
#'  #plot + theme_arrowlarge()
#'  #plot + theme_arrowcustomlength(.1,.8)
#'  #plot + theme_arrowlength(start=c(.1,.2,.3),finish=c(.9,.8,.7))
#' @name themearrowlength
#' @rdname themearrowlength
NULL

#' @section Custom Length:
#' \code{theme_arrowcustomlength} or \code{theme_arrowlength} (alias) sets the ternary arrow lengths to values as specified by the user, 
#' occupying a length between the values as specified by the  \code{start} and \code{finish} arguments (fractions) relative to the 
#' length of the ternary axis.
#' 
#' @param start a numeric scalar, or numeric vector of length three (3), representing the fractional [0,1] position along 
#' the axis where the arrow/s should START.
#' @param finish a numeric scalar, or numeric vector of length three (3), representing the fractional [0,1] position along 
#' the axis where the arrow/s should FINISH.
#' @rdname themearrowlength
#' @export
theme_arrowcustomlength <- function(start=getOption("tern.arrowstart"),finish=getOption("tern.arrowfinish")){
  #Execute
  .theme_arrowsize(s=start,f=finish)
}

#' @rdname themearrowlength
#' @export
theme_arrowlength <- theme_arrowcustomlength

#' @section Short Arrow Length:
#' \code{theme_arrowsmall} or \code{theme_arrowshort}(alias) reduces the ternary arrows to short arrows, occupying a length between 
#' \strong{0.4} and \strong{0.6} of the length of the ternary axis
#' @rdname themearrowlength
#' @export
theme_arrowsmall <- function(){.theme_arrowsize(s=0.4,f=0.6)}

#' @rdname themearrowlength
#' @export
theme_arrowshort <- theme_arrowsmall

#' @section Normal/Default Arrow Length:
#' \code{theme_arrownormal} or \code{theme_arrowdefault}(alias) reduces the ternary arrows to normally sized arrows, occupying a length between 
#' \code{getOption("tern.arrowstart")} and \code{getOption("tern.arrowfinish")} global option values, whatever they may be.
#' @rdname themearrowlength
#' @export
theme_arrownormal<- function(){.theme_arrowsize(s=getOption("tern.arrowstart"),f=getOption("tern.arrowfinish"))}

#' @rdname themearrowlength
#' @export
theme_arrowdefault <- theme_arrownormal

#' @section Long Arrow Length:
#' \code{theme_arrowlarge} or \code{theme_arrowlong}(alias) increases the ternary arrows to long arrows occupying a length between 
#' \strong{0.2} and \strong{0.8} of the length of the ternary axis
#' @rdname themearrowlength
theme_arrowlarge <- function(){.theme_arrowsize(s=0.2,f=0.8)}

#' @rdname themearrowlength
#' @export
theme_arrowlong  <- theme_arrowlarge
