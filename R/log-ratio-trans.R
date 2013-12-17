#' Log-ratio Transformations
#' 
#' Log-ratio transformation, algorithm based on that provided in \code{\link[robCompositions]{isomLR}}
#' @param  x 2D object to transform 
#' @aliases log-ratio logratio
#' @rdname logratio
#' @return transformed data
#' @references provided in \code{\link[robCompositions]{isomLR}}
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

#' Log-ratio Transformations
#' 
#' Inverse isometric log-ratio transformation, algorithm based on that provided in \code{\link[robCompositions]{isomLRinv}}
#' @param x.ilr 2D object to transform
#' @rdname logratio
#' @return transformed data
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