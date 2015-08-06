
tlr2xy <- function(data,coord,...,inverse=FALSE,scale=TRUE){
  #Run Some Checks
  if(!inherits(coord,"ternary")) 
    stop("argument 'coord' must be a ternary coordinate structure")
  if(class(data) != "data.frame")
    stop("arguemnt 'data' must be of type 'data.frame'")
  if(!is.logical(inverse) | !is.logical(scale))
    stop("argument 'inverse' and 'scale' (both) must be logical")
  
  #Global Variables
  ix.trl  = c("T","L","R"); ix.xy   = c("x","y")
  
  #Local function to adjust the range, depending on if inverse or not
  adjustRange <- function(input,lim,inv=inverse){
    if(is.null(lim)) lim=c(0,1)
    if( !diff(lim) ) lim=c(0,1)
    adl   = abs(diff(lim))
    ml    = min(lim)
    input = if(inv[1]){ input*adl + ml }else{ (input-ml)/adl }
    input
  }
  
  #Local function to scale ternary coordinates
  scaleCoordinates <- function(input){
    s  <- rowSums(input);
    ix <- which(!as.logical(s))
    if(length(ix) > 0){ input[ix,] <- rep(1/3,3); s[ix]  <- 1.0 } #Prevent Div By 0 error
    input[,ix.trl] = input[,ix.trl]/s
    input
  }
  
  #Inverse transformation is cartesian to ternary
  if(inverse[1]){
    if(!all(ix.xy %in% names(data))) stop('data must contain columns x and y')
    #Calculate
    out.R = data$x - data$y*tan(pi/6)
    out.T = data$y/(tan(pi/3)*0.5)
    out.L = 1.0 - out.R - out.T
    data  = data.frame(T=out.T,L=out.L,R=out.R)
    #Adjust for the Limits.
    for(x in ix.trl) data[,x] = adjustRange(data[,x],coord[[paste0(x,"lim")]])
  
  #Forward transformation is ternary to cartesian
  }else{
    if(!all(ix.trl %in% names(data))) stop("data must contain columns T, L and R")
    #If scale to composition sum of 1
    if(scale[1]) data = scaleCoordinates(data)
    #Adjust for the Limits.
    for(x in ix.trl) data[,x] = adjustRange(data[,x],coord[[paste0(x,"lim")]])
    #Calculate
    out.Y = data$T*tan(pi/3)*0.5
    out.X = data$R + out.Y*tan(pi/6)
    data  = data.frame(x=out.X,y=out.Y)
  }
  return(data)
}



xy2tlr <- function(data,coord,...,inverse=TRUE,scale=TRUE){
  tlr2xy(data,coord,...,inverse=!inverse,scale=scale)
}

#' df = data.frame(T=c(0,1),L=c(0,1),R=c(0,1))
#' tlr2xy(tlr2xy(df,coord_tern()),coord_tern(),inverse=TRUE)
