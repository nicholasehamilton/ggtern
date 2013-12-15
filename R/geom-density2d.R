#' Contours from a 2d density estimate - (ggtern version)
#'
#' Perform a 2D kernel density estimatation using kde2d and display the
#' results with contours.
#' 
#' This can be useful for dealing with overplotting.
#'
#' @aliases Density2dTern
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density2d")}
#'
#' @seealso \code{\link{geom_contour}} for contour drawing geom, 
#'  \code{\link{stat_sum}} for another way of dealing with overplotting
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @export
geom_density2d <- function (mapping = NULL, data = NULL, stat = "Density2dTern", position = "identity", 
                            lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) { 
  GeomDensity2dTern$new(mapping = mapping, data = data, stat = stat, position = position, 
                    lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

#' @rdname undocumented
GeomDensity2dTern <- proto(ggint$GeomPath,{
  objname <- "Density2dTern"
  default_stat <- function(.) StatDensity2dNew
  default_aes <- function(.) aes(colour="#3366FF", size = 0.5, linetype = 1, alpha = NA)
})