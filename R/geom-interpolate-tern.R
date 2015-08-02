#' Interpolate between values are render using contours
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "contour")}
#'
#' @inheritParams geom_path
#' @export
#' # See stat_contour for examples
geom_interpolate_tern <- function(mapping = NULL, data = NULL, stat = "InterpolateTern", position = "identity",
                              lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...){
  GeomInterpolateTern$new(mapping = mapping, data = data, stat = stat, position = position,
                      lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomInterpolateTern <- proto(GeomPolygonTern,{
  objname        <- "interpolate_tern"
  required_aes   <- c("x","y","z","value")
  default_aes    <- function(.) aes(colour="#3366FF", fill='transparent', size = 0.5, linetype = 1, alpha = NA,value=1)
  default_stat   <- function(.) StatInterpolateTern
})
