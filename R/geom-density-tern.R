#' Contours from a 2d density estimate - (ggtern version)
#'
#' Perform a 2D kernel density estimatation using kde2d and display the
#' results with contours.
#' 
#' This can be useful for dealing with overplotting. Additional weight aesthetic (see aesthetic section below) permits better weighting if desired
#'
#' @aliases DensityTern GeomDensityTern
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "DensityTern")}
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggtern::stat_density_tern
#' @export
geom_density_tern <- function ( mapping   = NULL, 
                                data      = NULL, 
                                stat      = "DensityTern",
                                position  = "identity",
                                lineend   = "butt", 
                                linejoin  = "round", 
                                linemitre = 1, 
                                na.rm     = FALSE,
                                n         = getOption('tern.mesh.size'),
                                buffer    = getOption('tern.mesh.buffer'),...){
  GeomDensityTern$new(  mapping   = mapping, data = data, 
                        stat      = stat, 
                        buffer    = buffer,
                        position  = position, 
                        lineend   = lineend, 
                        linejoin  = linejoin, 
                        linemitre = linemitre, 
                        na.rm     = na.rm,
                        n = n
                        ,...)
}

GeomDensityTern <- proto(ggint$GeomPath,{
  objname <- "density_tern"
  default_stat <- function(.) StatDensityTern
  default_aes  <- function(.) aes(colour="#3366FF",size = 0.5,linetype = 1, alpha = NA,weight=1,fill="transparent")
  draw         <- function(., data,
                           scales,
                           coordinates,
                           arrow     = NULL,
                           lineend   = "butt",
                           linejoin  = "round",
                           linemitre = 1,
                           na.rm     = FALSE,
                           ...){
    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    req.aes <- unique(c(.$required_aes,coordinates$required_aes))
    check_required_aesthetics(req.aes,names(data),.$objname)
    
    return(GeomPolygonTern$draw(.,data   = data,scales=scales,coordinates=coordinates,
                                arrow    = arrow,
                                lineend  = lineend,
                                linejoin = linejoin,linemitre=linemitre,na.rm=na.rm,...))
  }
})






