#' Contours from a 2d density estimate - (ggtern version)
#'
#' Perform a 2D kernel density estimatation using kde2d and display the
#' results with contours.
#' 
#' This can be useful for dealing with overplotting. Additional weight aesthetic permits better weighting if desired
#'
#' @aliases Density2dTern
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "density2d")}
#' Additional optional aethetic in the patched version (if not provided, existing behaviour applies)
#' \itemize{
#'  \item \code{weight}
#' }
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @aliases GeomDensity2dtern
#' @export
geom_density2d <- function (mapping = NULL, data = NULL, stat = "density2dtern", position = "identity", 
                            lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...) { 
  GeomDensity2dtern$new(mapping = mapping, data = data, stat = stat, position = position, 
                    lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomDensity2dtern <- proto(ggint$GeomPath,{
  objname <- "density2dtern"
  default_stat <- function(.) StatDensity2dNew
  default_aes <- function(.) aes(colour="#3366FF", size = 0.5, linetype = 1, alpha = NA)
})