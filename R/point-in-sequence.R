#' Put Points in Sequence
#'
#' The \code{point.in.sequence} function takes numeric input vectors \code{x} and \code{y} or a 
#' \code{\link{data.frame}} object, and orders the values in such way that they are correctly sequenced by the angle subtended between each point,
#' and, the centroid of the total set. If the data is provided in the format of a \code{data.frame}, then it must 
#' containing columns named \code{x} and \code{y}, else an error will be thrown.
#' 
#' The arguments \code{x} and \code{y} represent cartesian coordinates. This is useful if a path is sought that 
#' passes through each point in the ordered set, however, no two lines in the total path cross over each other. 
#' Uses the \code{\link{atan2}} function to determine the angle (theta) between each point (x,y) and the centroid 
#' of the data, it then orders based on increasing values of theta. 
#' 
#' @param x vector of numeric \code{x} values
#' @param y vector of numeric \code{y} values
#' @param ... not used
#' @param df data.frame containing colums \code{x} and \code{y}
#' @param close logical value (default \code{FALSE}), as to whether the set should be closed by adding (duplicating) 
#' the first row (after ordering) to the end of the set.
#' @return \code{data.frame} object containing the re-ordered input set.
#' @export  
point.in.sequence <- function(x,y,...,df=data.frame(x=x,y=y),close=FALSE){
  #If first argument is provided as data.frame, re-assign to df
  if(!missing(x))
    if(class(x) == "data.frame")
      df = x
  
  #Check df is dataframe
  if(class(df) != "data.frame")
    stop("df must be a data.frame",call.=F)
  
  #Check 2 or more unique rows exist.
  if(nrow(unique(df)) <= 1)
    stop("df must contain at least two unique rows",call.=FALSE)
  
  #Check correct columns exist
  sapply(c("x","y"),function(X){
    if(!X %in% colnames(df))
      stop(paste("df must contain column",X),call.=F)
    if(!is.numeric(df[,X]))
      stop(paste("df must contain column",X,"and must be numeric"),call.=F)
  })
  
  #Check close argument is logical
  close = ifthenelse(is.logical(close),close[1],FALSE)
  
  #Center point
  c.x = mean(df$x)
  c.y = mean(df$y)
  
  #Determine angle with center point
  df$theta <- apply(df[,c("x","y")],1,function(r) atan2(r[2] - c.y,r[1] - c.x)*180/pi)
  
  #Put in correct order and
  df <- df[with(df,base::order(theta)), ]
  
  #Make a closed loop if desired 
  #by duplicating the first row on the bottom
  if(close)
    df <- rbind(df,df[1,])
  
  #Remove theta column
  df$theta=NULL
  
  #And return
  df
}