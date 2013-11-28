#----------------------------------------
#PROTO FOR GEOM Polygon
#----------------------------------------
geom_polygon_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, ...) {
  GeomPolygonTern$new(mapping = mapping, data = data, stat = stat, position = position, na.rm = na.rm, ...)
}
GeomPolygonTern <- proto(ggplot2:::Geom,{
  objname <- "polygon_tern"
  draw_groups <- function(. ,...){ .$draw(...) }
  draw <- function(., data, scales, coordinates, ...) {  
    data <- remove_missing(data,na.rm,c("x","y","z"),name = "geom_polygon_tern")
    if (empty(data)) return(zeroGrob())
    data[,c("x","y")] <- transformTernToCart(data=data[,c("x","y","z")])
    ggplot2:::GeomPolygon$draw(.=.,data=data,scales=scales,coordinates=coordinates,...)
  }
  draw_legend <- function(., data, ...) {
    ggplot2:::GeomPolygon$draw_legend(., data, ...)
  }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z")
  default_aes <- function(.) aes(colour="NA", fill="grey20", size=0.5,linetype=1,alpha=1)
  guide_geom <- function(.) "polygon"
})