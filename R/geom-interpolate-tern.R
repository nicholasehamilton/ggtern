#' Ternary Interpolation
#' 
#' This is the heavily requested geometry for interpolating between ternary values, results being
#' rendered using contours on a ternary mesh. 
#' 
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "InterpolateTern")}
#' @inheritParams geom_polygon_tern
#' @inheritParams ggplot2::geom_density2d
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggtern::stat_density_tern
#' @seealso \code{\link{stat_interpolate_tern}}
#' @aliases GeomInterpolateTern InterpolateTern geom_interpolation
#' @examples
#' data(Feldspar)
#' ggtern(Feldspar,aes(x=Or,y=An,z=Ab)) + 
#' geom_interpolate_tern(aes(value=P.Gpa,color=..level..),binwidth=20) +
#' geom_point()
#' @export
geom_interpolate_tern <- function(mapping = NULL, data = NULL, stat = "InterpolateTern", position = "identity",
                              lineend  = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE,
                              n        = getOption('tern.mesh.size'),
                              buffer   = getOption('tern.mesh.buffer'),
                              ...){
  GeomInterpolateTern$new(mapping = mapping, data = data, stat = stat, position = position,
                      lineend = lineend, linejoin = linejoin, 
                      linemitre = linemitre, na.rm = na.rm,buffer=buffer,n=n, ...)
}

GeomInterpolateTern <- proto(GeomPolygonTern,{
  objname        <- "interpolate_tern"
  required_aes   <- c("x","y","z","value")
  default_aes    <- function(.) aes(colour="#3366FF", fill='transparent', size = 0.5, linetype = 1, alpha = NA,value=1)
  default_stat   <- function(.) StatInterpolateTern
})
