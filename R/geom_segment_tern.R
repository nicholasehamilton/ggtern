
#----------------------------------------
#PROTO FOR GEOM SEGMENT TERN
#----------------------------------------
geom_segment_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {
  GeomSegmentTern$new(mapping = mapping, data = data, stat = stat,position = position, arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
}
GeomSegmentTern <- proto(ggplot2:::Geom,{
  objname <- "segment_tern"
  draw_groups <- function(. ,...){ .$draw(...) }
  draw <- function(., data, scales, coordinates, arrow = NULL,lineend = "butt", na.rm = FALSE, ...) {  
    print(class(scales))
    print(class(coordinates))
    
    data <- remove_missing(data,na.rm,c("x","y","z","xend","yend","zend"),name = "geom_segment_tern")
    if (empty(data)) return(zeroGrob())
    data[,c("x","y")]       <- transformTernToCart(data=data[,c("x","y","z")])
    data[,c("xend","yend")] <- transformTernToCart(data=data[,c("xend","yend","zend")])    
    ggplot2:::GeomSegment$draw(.=., data=data, scales=scales, coordinates=coordinates,arrow = arrow, lineend = lineend, na.rm = na.rm,...)
  }
  draw_legend <- function(., data, ...) { ggplot2:::GeomSegment$draw_legend(.,data,...) }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z","xend","yend","zend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
  
})