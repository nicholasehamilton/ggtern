#' Ternary Continuous Scales
#' 
#' Create continuous scales for the top (T), left (L) and right(R) species in the ternary diagram, and, control the appearance of the labels, the limits, major and
#' minor breaks. 
#' 
#' 
#' At the moment, only continuous scales are availa ble for \code{ggtern} T, L and R axes - log scales are NOT available. If one is interested in merely controling the limits (ranges) of the T, L and R species, it is much easier to use the \code{tern_limits(...)} function 
#' or its aliasses, click \code{\link[=tern_limits]{HERE}} for further information.
#' 
#' @aliases scale_L_continuous scale_T_continuous scale_R_continuous scale_T scale_L scale_R
#' @param breaks the major breaks
#' @param minor_breaks the minor breaks
#' @param labels the major labels
#' @param limits the range of the scale
#' @param name the name of the scale
#' @rdname ternaryscales
#' @name ternaryscales
#' @examples
#' \donttest{
#'   data(Feldspar)
#'   plot <- ggtern(data=Feldspar,aes(An,Ab,Or)) + 
#'           geom_point() + 
#'           scale_T_continuous(breaks=seq(0,1,0.2),minor_breaks=seq(0,1,0.1)) +
#'           scale_L_continuous(breaks=seq(0,1,0.2),minor_breaks=seq(0,1,0.1))
#'   plot
#' }
NULL 

#' @section Top Apex Species:
#' \code{scale_T_continuous} creates a continuous scale for the top (T) species in the ternary diagram
#' @rdname ternaryscales
#' @export
scale_T_continuous <- function(name=NULL,breaks=getOption("tern.breaks.default"),minor_breaks=getOption("tern.breaks.default.minor"),labels=100*breaks,limits=waiver()){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  continuous_scale(c("T"),name=name,scale_name="tern_T", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}

#' @section Left Apex Species:
#' \code{scale_L_continuous} creates a continuous scale for the left (L) species in the ternary diagram
#' @rdname ternaryscales
#' @export
scale_L_continuous <- function(name=NULL,breaks=getOption("tern.breaks.default"),minor_breaks=getOption("tern.breaks.default.minor"),labels=100*breaks,limits=waiver()){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  continuous_scale(c("L"),name=name,scale_name="tern_L", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}

#' @section Right Apex Species:
#' \code{scale_R_continuous} creates a continuous scale for the right (R) species in the ternary diagram
#' @rdname ternaryscales
#' @export
scale_R_continuous <- function(name=NULL,breaks=getOption("tern.breaks.default"),minor_breaks=getOption("tern.breaks.default.minor"),labels=100*breaks,limits=waiver()){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  continuous_scale(c("R"),name=name,scale_name="tern_R", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}


