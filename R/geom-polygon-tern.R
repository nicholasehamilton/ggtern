
#' Polygon Geometry (Ternary Version)
#' 
#' Add polygons to the ternary surface
#' @aliases PolygonTern GeomPolygonTern
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "PolygonTern")}
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
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
    #Run the Checks
    if(!inherits(coordinates,"ternary")){ return(.zeroGrob) }
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_confidence")
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes),name = "geom_confidence")
    if(empty(data)) return(.zeroGrob)
    
    ##Build the List of Grobs in Sequence
    contours = dlply(data,c("PANEL","group","level","piece"),function(df){
      
      #Kill the Colours
      if('point.in.polygon.status' %in% names(df)){
        ix <- which(df$point.in.polygon.status %in% c(0,2,3))
        if(length(ix) > 0) df[ix,"colour"] = 'transparent'
      }else{
        df  <- suppressColours(df,coordinates,remove=FALSE)
      }
      
      #Default Grobs
      polygrob = pathgrob = .zeroGrob
      
      if(nrow(df) > 0){
        
        #Check the linetype is ok
        okLineType = all(df$linetype %in% c(1))
        
        #Polygons, with no border (color)
        if(okLineType){
          df.poly <- subset(df,!(fill == 'transparent') | fill == NA)
          if(nrow(df.poly) > 0){
            df.poly$colour <- 'transparent'
            polygrob <- (GeomPolygon)$draw(data=df.poly, scales=scales, coordinates=coordinates,...)
          }
          pathgrob   <- (GeomPath)$draw(data=df, scales=scales, coordinates=coordinates,...)
        
        }else{
          polygrob  <- (GeomPolygon)$draw(data=df, scales=scales, coordinates=coordinates,...)
        }
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