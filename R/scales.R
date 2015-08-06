#' Ternary Continuous Scales
#' 
#' Create continuous scales for the top (T), left (L) and right(R) species in the ternary diagram, and, control the appearance of the labels, the limits, major and
#' minor breaks. 
#' 
#' 
#' At the moment, only continuous scales are availa ble for \code{ggtern} T, L and R axes - log scales are NOT available. If one is interested in merely controling the limits (ranges) of the T, L and R species, it is much easier to use the \code{tern_limits(...)} function 
#' or its aliasses, click \code{\link[=limit_tern]{HERE}} for further information.
#' 
#' @aliases scale_L_continuous scale_T_continuous scale_R_continuous scale_T scale_L scale_R scale_clone.continuous_ternary scale_transform.continuous_ternary 
#' scale_break_info.continuous_ternary scale_breaks.continuous_ternary scale_labels.continuous_ternary scale_breaks_minor.continuous_ternary
#' @param breaks the major breaks
#' @param minor_breaks the minor breaks
#' @param labels the major labels
#' @param limits the range of the scale
#' @param name the name of the scale
#' @rdname scale_TLR_continuous
#' @name scale_TLR_continuous
#' @examples
#'   data(Feldspar)
#'   plot <- ggtern(data=Feldspar,aes(An,Ab,Or)) + geom_point() + 
#'           scale_T_continuous(breaks=seq(0,1,0.2),minor_breaks=seq(0,1,0.1)) +
#'           scale_L_continuous(breaks=seq(0,1,0.2),minor_breaks=seq(0,1,0.1))
#'   plot
NULL 

#' @section Top Apex Species:
#' \code{scale_T_continuous} creates a continuous scale for the top (T) species in the ternary diagram
#' @rdname scale_TLR_continuous
#' @export
scale_T_continuous <- function(name=NULL,
                               limits       = waiver(),
                               breaks       = getBreaks(limits,TRUE),
                               minor_breaks = getBreaks(limits,FALSE),
                               labels       = 100*breaks){
  if(!is.numeric(breaks)){minor_breaks=breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  ret <- continuous_scale(c("T"),scale_name="tern_T",identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels,
                          expand = waiver(),guide = "none",limits=limits,name=name)
  class(ret) <- c("tern_T","continuous_ternary","scale")
  ret
}

#' @section Left Apex Species:
#' \code{scale_L_continuous} creates a continuous scale for the left (L) species in the ternary diagram
#' @rdname scale_TLR_continuous
#' @export
scale_L_continuous <- function(name=NULL,
                               limits       = waiver(),
                               breaks       = getBreaks(limits,TRUE),
                               minor_breaks = getBreaks(limits,FALSE),
                               labels       = 100*breaks){
  if(!is.numeric(breaks)){minor_breaks=breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  ret <- continuous_scale(c("L"),scale_name="tern_L", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels,
                          expand = waiver(), guide = "none",limits=limits,name=name)
  class(ret) <- c("tern_L","continuous_ternary","scale")
  ret
}

#' @section Right Apex Species:
#' \code{scale_R_continuous} creates a continuous scale for the right (R) species in the ternary diagram
#' @rdname scale_TLR_continuous
#' @export
scale_R_continuous <- function(name=NULL,
                               limits       = waiver(),
                               breaks       = getBreaks(limits,TRUE),
                               minor_breaks = getBreaks(limits,FALSE),
                               labels       = 100*breaks){
  if(!is.numeric(breaks)){minor_breaks=breaks="none"}
  if(length(labels) == 1 & length(breaks) > 1){labels = rep(labels,length(breaks))}
  ret <- continuous_scale(c("R"),scale_name="tern_R",identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels,
                          expand = waiver(), guide = "none",limits=limits,name=name)
  class(ret) <- c("tern_R","continuous_ternary","scale")
  ret
}

scale_clone.continuous_ternary       <- find_global_tern("scale_clone.continuous")
scale_transform.continuous_ternary   <- find_global_tern("scale_transform.continuous")
scale_break_info.continuous_ternary  <- find_global_tern("scale_break_info.continuous")
scale_breaks.continuous_ternary      <- find_global_tern("scale_breaks.continuous")
scale_labels.continuous_ternary      <- find_global_tern("scale_labels.continuous")
scale_breaks_minor.continuous_ternary<- find_global_tern("scale_breaks_minor.continuous")

