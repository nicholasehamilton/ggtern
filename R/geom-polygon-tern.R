
#' Polygon Geometry (Ternary Version)
#' 
#' Add polygons to the ternary surface
#' @inheritParams ggplot2::geom_polygon
#' @aliases GeomPolygonTern
#' @export
geom_polygon_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  GeomPolygonTern$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomPolygonTern <- proto( ggint$GeomPolygon, {
  objname      <- "polygon_tern"
  required_aes <- c("x", "y")
  default_stat <- function(.) StatIdentity
  default_aes  <- function(.) aes(colour="NA", fill="grey20", size=0.5, linetype=1, alpha = NA)
  guide_geom   <- function(.) "polygon"
  draw         <- function(., data, scales, coordinates,...) {
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_confidence")
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes),name = "geom_confidence")
    if(empty(data)) return(.zeroGrob)
    
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