#----------------------------------------
#PROTO FOR GEOM POINT TERN
#----------------------------------------
geom_point_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, ...){
  GeomPointTern$new(mapping = mapping, data = data, stat = stat, position = position, na.rm = na.rm, ...)
}
GeomPointTern <- proto(ggplot2:::Geom,{
  objname <- "point_tern"
  draw_groups <- function(. ,...){.$draw(...)}
  draw <- function(.,data, scales, coordinates, ... ,na.rm = FALSE) {
    data <- remove_missing(data,na.rm,c("x","y","z"),name = "geom_point_tern")
    if (empty(data)) return(zeroGrob())
    data[,c("x","y")] <- transformTernToCart(data=data[,c("x","y","z")])
    ggplot2:::GeomPoint$draw(.=.,data=data,scales=scales,coordinates=coordinates,...,na.rm=na.rm)
  }
  draw_legend <- function(., data, ...) {
    ggplot2:::GeomPoint$draw_legend(.=.,data=data,...)
  }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z")
  default_aes <- function(.) aes(shape=16,colour="black",size=2,fill = NA,alpha = NA)
})
