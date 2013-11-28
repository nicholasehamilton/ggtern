#----------------------------------------
#PROTO FOR GEOM POINT TERN
#----------------------------------------
geom_path_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, ...) {
  GeomPathTern$new(mapping = mapping, data = data, stat = stat, position = position, na.rm = na.rm, ...)
}
GeomPathTern <- proto(ggplot2:::Geom,{
  objname <- "path_tern"
  draw_groups <- function(. ,...){ .$draw(...) }
  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, ..., na.rm = FALSE) {  
    data <- remove_missing(data,na.rm,c("x","y","z"),name = "geom_path_tern")
    if (empty(data)) return(zeroGrob())
    data[,c("x","y")] <- transformTernToCart(data=data[,c("x","y","z")])
    ggplot2:::GeomPath$draw(.=., data=data, scales=scales, coordinates=coordinates,arrow = arrow, lineend = lineend, linejoin = linejoin, linemitre = linemitre, ..., na.rm = na.rm)
  }
  draw_legend <- function(., data, ...) {
    ggplot2:::GeomPath$draw_legend(.,data,...)
  }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA,fill=NA)
  guide_geom <- function(.) "path"
})


