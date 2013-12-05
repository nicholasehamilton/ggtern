#' Ternary Continuous Scales
#' 
#' \code{scale_L_continuous} creates a continuous scale for the left species in the ternary diagram
#' @param breaks the major breaks
#' @param minor_breaks the minor breaks
#' @param labels the major labels
#' @param limits the range of the scale
#' @rdname ternaryscales
#' @export
scale_L_continuous <- function(name=NA,breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  ret <- continuous_scale(c("L"), "tern_L", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
  if(is.character(name)){
    list(ret,labs(y=name))
  }else{
    ret
  }
}

#' Ternary Continuous Scales
#' 
#' \code{scale_T_continuous} creates a continuous scale for the top species in the ternary diagram
#' @rdname ternaryscales
#' @export
scale_T_continuous <- function(name=NA,breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  ret <- continuous_scale(c("T"), "tern_T", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
  if(is.character(name)){
    list(ret,labs(x=name))
  }else{
    ret
  }
}

#' Ternary Continuous Scales
#' 
#' \code{scale_R_continuous} creates a continuous scale for the right species in the ternary diagram
#' @rdname ternaryscales
#' @export
scale_R_continuous <- function(name=NA,breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  ret <- continuous_scale(c("R"), "tern_R", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
  if(is.character(name)){
    list(ret,labs(z=name))
  }else{
    ret
  }
}

