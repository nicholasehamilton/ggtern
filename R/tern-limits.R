#' Restrict Ternary Limits
#' 
#' \code{tern_limits} (or its aliasses) appends new \code{T}, \code{L} and \code{R} ternary continuous scales, 
#' where the maximum scale value is specified, and, where the minimums for each are solved.
#' @param T numeric value of the maximum T species scale value
#' @param L numeric value of the maximum L species scale value
#' @param R numeric value of the maximum R species scale value
#' @param ... other arguments to pass to ALL of \code{scale_X_continuous}, where \code{X = T, L, R}
#' @param verbose report the solved values
#' @seealso \code{\link{scale_T_continuous}}, \code{\link{scale_L_continuous}} and \code{\link{scale_R_continuous}}
#' @examples 
#' plot <- ggtern(data=data.frame(x=runif(100),
#'                                y=runif(100),
#'                                z=runif(100)), aes(x,y,z)) + 
#'         geom_point() + 
#'         tern_limits(0.7,0.3,0.4)
#' plot
#' @name   coord_tern.limit_tern
#' @rdname tern_limits
NULL

#' @rdname tern_limits
#' @export
tern_limits <- function(T=1,L=1,R=1,...,verbose=F){
  ret <- list()
  tryCatch({
    if(!is.numeric(T) | !is.numeric(L) | !is.numeric(R)){stop("T, L and R input parameters must be a numeric scalar.")}
    
    .report <- function(x,src="")
      if(x > 1 | x < 0)
        message(paste0("Maximum ",src," is outside the limit [0,1] (Currently it is ",x,") and will be truncated accordingly."))
    .report(T,"T")
    .report(L,"L")
    .report(R,"R")
    
    T <- max(min(1,T),0)[1]
    L <- max(min(1,L),0)[1]
    R <- max(min(1,R),0)[1]
    
    #Square matrix
    d <- as.matrix(data.frame(t=c(0,1,1),
                              l=c(1,0,1),
                              r=c(1,1,0)))
    
    #vector coeficients
    v <- c(1-T,1-L,1-R)
    
    #solve
    minima <- round(solve(d,v),3)
    if(verbose)
      print(minima)
    if(min(minima) < 0 | max(minima) > 1.0)
      stop("Invalid Maximum T, L and R values.")
    
    #Return.
    ret <- list( scale_T_continuous(limits=c(minima[1],T),...),
                 scale_L_continuous(limits=c(minima[2],L),...),
                 scale_R_continuous(limits=c(minima[3],R),...))
  },error=function(e){
    warning(e)
  })
  
  return(ret)
}

#'\code{limits_tern} is an alias for \code{tern_limits}
#'@rdname tern_limits
#'@export 
limits_tern <- function(...){tern_limits(...)}

#'\code{limit_tern} is an alias for \code{tern_limits}
#'@rdname tern_limits
#'@export 
limit_tern <- function(...){tern_limits(...)}

#'\code{tern_limit} is an alias for \code{tern_limits}
#'@rdname tern_limits
#'@export 
tern_limit <- function(...){tern_limits(...)}


