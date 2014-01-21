#' Ternary Error Bars
#' 
#' \code{geom_errorbarT}, \code{geom_errorbarL} and \code{geom_errorbarR} are geometries to render error bars
#' for the top, left and right apex species respectively, analogous to \code{\link[ggplot2]{geom_errorbar}} and/or 
#' \code{\link[ggplot2]{geom_errorbarh}} as provided in the base ggplot2 package.
#' 
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics: 
#' Additional required aethetics are, for each respectively:
#' 
#' \code{geom_errorbarT:}
#' \itemize{
#'  \item \code{Tmax}
#'  \item \code{Tmin}
#' }
#' \code{geom_errorbarL:}
#' \itemize{
#'  \item \code{Lmax}
#'  \item \code{Lmin}
#' }
#' \code{geom_errorbarR:}
#' \itemize{
#'  \item \code{Rmax}
#'  \item \code{Rmin}
#' }
#' 
#' @rdname ternaryerrorbars
#' @examples
#' \donttest{
#' tmp <- data.frame(x=1/3,
#' y=1/3,
#' z=1/3,
#' Min=1/3-1/6,
#' Max=1/3+1/6)
#' ggtern(data=tmp,aes(x,y,z)) + 
#'   geom_point() + 
#'   geom_errorbarT(aes(Tmin=Min,Tmax=Max))+
#'   geom_errorbarL(aes(Lmin=Min,Lmax=Max))+
#'   geom_errorbarR(aes(Rmin=Min,Rmax=Max)) 
#' }
#' @export
geom_errorbarT <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  ggint$GeomErrorbarT$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

ggint$GeomErrorbarT <- proto(Geom,{
  objname <- "errorbarT"
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x","y","z","Tmax","Tmin")
  draw <- function(., data, scales, coordinates, height = NULL, ...) {
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_errorbarT")
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    IX <- coordinates$T
    data$LMAX = data[,IX] - data$Tmax
    data$LMIN = data[,IX] - data$Tmin
    
    df <- with(data, data.frame( 
      x        = ifthenelse(IX=="x",
                            as.vector(rbind(Tmax,x,Tmin)),
                            as.vector(rbind(x+LMAX/2,x,x+LMIN/2))
                 ),
      y        = ifthenelse(IX=="y",
                            as.vector(rbind(Tmax,y,Tmin)),
                            as.vector(rbind(y+LMAX/2,y,y+LMIN/2))
                 ),
      z        = ifthenelse(IX=="z",
                            as.vector(rbind(Tmax,z,Tmin)),
                            as.vector(rbind(z+LMAX/2,z,z+LMIN/2))
                 ),
      colour   = rep(colour,each = 3),
      alpha    = rep(alpha, each = 3),
      size     = rep(size,  each = 3),
      linetype = rep(linetype, each = 3),
      group    = rep(1:(nrow(data)), each = 3),
      stringsAsFactors = FALSE, 
      row.names = 1:(nrow(data)*3)
    ))
    GeomPath$draw(df,scales,coordinates,...)
  }
})