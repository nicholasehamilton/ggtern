#' Log-ratio Transformations
#' 
#' Log-ratio and Inverse Log-ratio transformations, for use as part of the \code{\link{geom_confidence}} geometry.
#' @rdname logratio
#' @keywords internal
#' @name logratio
NULL

#' @details \code{isomLR} is a function which executes the Log-ratio transformation as per the algorithm based 
#' on that provided in \code{\link[robCompositions]{isomLR}}.
#' @param  x 2D object to transform 
#' @aliases log-ratio
#' @rdname logratio
#' @keywords internal
#' @return \code{isomLRinv} returns the transformed data
#' @references References are provided in the original \code{\link[robCompositions]{isomLR}} documentation.
isomLR <- function(x){
  geometricMean <- function(x){
    if(!is.numeric(x)) stop("x has to be a vector of class numeric")
    if (any(na.omit(x == 0))){0}
    else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
  }
  x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
  D=ncol(x)
  for (i in 1:ncol(x.ilr)){x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(apply(as.matrix(x[,(i+1):D]), 1, geometricMean)/(x[,i]))} 
  return(x.ilr)
}

#' @details \code{isomLRinv} is a function which executes the Inverse isometric log-ratio transformation 
#' as per the algorithm based on that provided in \code{\link[robCompositions]{isomLRinv}}.
#' 
#' The above functions are based on those provided as part of the robCompositions package.
#' @param x.ilr 2D object to transform
#' @keywords internal
#' @rdname logratio
#' @return \code{isomLRinv} returns the inverse-transformed data
isomLRinv <- function(x.ilr){
  y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
  D=ncol(x.ilr)+1
  y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
  for(i in 2:ncol(y))
    for(j in 1:(i-1))
      y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
  for (i in 2:(ncol(y)-1))
    y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
  yexp=exp(y)
  x.back=yexp/apply(yexp,1,sum)
  return(x.back)
}