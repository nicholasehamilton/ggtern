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
geom_density_tern <- function ( mapping   = NULL, 
                                data      = NULL, 
                                stat      = "DensityTern",
                                position  = "identity",
                                lineend   = "butt", 
                                linejoin  = "round", 
                                linemitre = 1,
                                n         = 100, 
                                na.rm     = FALSE,
                                buffer    = getOption('tern.densitygrid.buffer'),...){
  GeomDensityTern$new(  mapping   = mapping, data = data, 
                        stat      = stat, 
                        buffer    = buffer,
                        position  = position, 
                        lineend   = lineend, 
                        linejoin  = linejoin, 
                        linemitre = linemitre, 
                        na.rm     = na.rm,n = n,...)
}

GeomDensityTern <- proto(ggint$GeomPath,{
  objname <- "density_tern"
  default_stat <- function(.) StatDensityTern
  default_aes  <- function(.) aes(colour="#3366FF",size = 0.5,linetype = 1, alpha = NA,weight=1,fill="transparent")
  draw         <- function(., data,scales,coordinates,...){
    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    req.aes <- unique(c(.$required_aes,coordinates$required_aes))
    check_required_aesthetics(req.aes, names(data),.$objname)
    data <- remove_missing(data, na.rm = na.rm, required_aes,name = .$objname)
      
    #Check
    if(!inherits(coordinates,"ternary")){
      warning("Coordinates Must be Ternary, returning zerogrob")
      return(.zeroGrob)
    }
    
    #Which to Ply On 
    plyon = c("PANEL","group")
    
    ##Build the List of Grobs in Sequence
    contours = dlply(data,plyon,function(df.orig){
      
      #Kill the Colours
      df  <- suppressColours(df.orig,coordinates,remove=FALSE)
      
      #Default Grobs
      polygrob = pathgrob = .zeroGrob
      
      if(nrow(df) > 0){
        #Polygons, with no border (color)
        df.poly <- subset(df,!(fill == 'transparent') | fill == NA)
        if(nrow(df.poly) > 0){
          df.poly$colour <- NA
          polygrob <- (GeomPolygon)$draw(data=df.poly, scales=scales, coordinates=coordinates,...)
        }
        
        #Paths, to immitate the border
        pathgrob   <- (GeomPath)$draw(data=df, scales=scales, coordinates=coordinates,...)
      }
      
      #Done
      list(polygrob,pathgrob)
    })
    
    # Ensure al least minimum grobs are available to return
    if(length(contours) == 0){ contours = list(.zeroGrob) }
    
    #Put in gtree format and return
    gTree(children = do.call("gList",unlist(contours,recursive=F)))
  }
})






