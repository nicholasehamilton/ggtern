#----------------------------------------
# PROTO FOR GEOM TEXT TERN
#----------------------------------------
geom_text_tern <- function(mapping=NULL,data=NULL,stat="identity",position="identity",na.rm=FALSE,...){
  GeomTextTern$new(mapping = mapping, data = data, stat = stat, position = position, na.rm = na.rm, ...)
}
GeomTextTern <- proto(ggplot2:::Geom,{
  objname <- "text_tern"
  draw_groups <- function(. ,...){ .$draw(...) }
  draw <- function(., data, scales, coordinates,...,parse=FALSE,na.rm = FALSE) {  
    data <- remove_missing(data,na.rm,c("x","y","z","label"),name = "geom_text_tern")
    if (empty(data)) return(zeroGrob())
    lab <- data$label
    if (parse) { lab <- parse(text = lab)}
    data[,c("x","y")] <- transformTernToCart(data=data[,c("x","y","z")])
    #data <- coord_transform(coordinates,data,scales)
    ggplot2:::GeomText$draw(.=.,data=data, scales=scales, coordinates=coordinates,...,parse=parse,na.rm = na.rm)
  }
  draw_legend <- function(., data, ...) {
    ggplot2:::GeomText$draw_legend(.=.,data=data,...=...)
  }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z","label")
  default_aes <- function(.) aes(colour="black", size=5 , angle=0, hjust=0.5, vjust=0.5, alpha = NA, family="", fontface=1, lineheight=1.2,fill=NA)
  guide_geom <- function(x) "text"
})