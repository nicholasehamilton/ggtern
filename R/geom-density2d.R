#' Contours from a 2d density estimate - (ggtern version)
#'
#' Perform a 2D kernel density estimatation using kde2d and display the
#' results with contours.
#' 
#' This can be useful for dealing with overplotting. Additional weight aesthetic (see aesthetic section below) permits better weighting if desired
#'
#' @aliases Density2dTern GeomDensity2dtern
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "density2dtern")}
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @export
geom_density2d <- function (mapping = NULL, data = NULL, stat = "density2d", position = "identity",
                            lineend = "butt", linejoin = "round", linemitre = 1,n=100, 
                            na.rm = FALSE,buffer= getOption('tern.densitygrid.buffer'),...){
  GeomDensity2dtern$new(mapping = mapping, data = data, 
                        stat = iflasttern(paste0(stat,'tern'),stat), buffer=buffer,position = position, 
                        lineend = lineend, linejoin = linejoin, linemitre = linemitre, stripOutside = TRUE, na.rm = na.rm,n = n,...)
}

GeomDensity2dtern <- proto(ggint$GeomPath,{
  objname <- "density2dtern"
  default_stat <- function(.) StatDensity2dtern
  default_aes  <- function(.) aes(colour="#3366FF",colourOutside="transparent",size = 0.5,linetype = 1, alpha = NA,weight=1,fill="transparent")
})






