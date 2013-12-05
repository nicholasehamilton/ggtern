

#' Restrict Ternary Limits
#' 
#' Appends new T, L and R ternary continuous scales, where the maximum scale value is specified, and, where the minimums for each are solved.
#' @param T numeric value of the maximum T species scale value
#' @param L numeric value of the maximum L species scale value
#' @param R numeric value of the maximum R species scale value
#' @param ... other arguments to pass to ALL of \code{scale_X_continuous}, where \code{X = T, L, R}
#' @seealso \code{\link{scale_T_continuous}}, \code{\link{scale_L_continuous}} and \code{\link{scale_R_continuous}}
#' @examples 
#' plot <- ggtern(data=data.frame(x=runif(100),y=runif(100),z=runif(100)),aes(x,y,z)) + geom_point() + tern_limits(0.7,0.3,0.4)
#' plot
#' @export
tern_limits <- function(T=1,L=1,R=1,...){
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
  minima <- solve(d,v)
  #check validity
  if(min(minima) < 0 | max(minima) > 1.0){stop("Invalid Maximum T, L and R values.")}
  
  #Return.
  list(
    scale_T_continuous(limits=c(minima[1],T),...),
    scale_L_continuous(limits=c(minima[2],L),...),
    scale_R_continuous(limits=c(minima[3],R),...)
  )
}