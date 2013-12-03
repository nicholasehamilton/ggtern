scale_L_continuous <- function(breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  continuous_scale(c("L"), "tern_L", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}
scale_T_continuous <- function(breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  continuous_scale(c("T"), "tern_T", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}
scale_R_continuous <- function(breaks=pretty(c(0,1),n=10),minor_breaks=pretty(c(0,1),n=20),labels=waiver(),limits=c(0,1)){
  if(!is.numeric(breaks)){minor_breaks=major_breaks="none"}
  continuous_scale(c("R"), "tern_R", identity,breaks=breaks,minor_breaks=minor_breaks,labels=labels, expand = waiver(), guide = "none",limits=limits)
}