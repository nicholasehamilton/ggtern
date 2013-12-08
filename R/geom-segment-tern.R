#' Geom Segment for Ternary System
#' 
#' This is the ternary equivalent for the geom_segment proto, requiring \code{zend} and \code{z} additional mandatory aesthetic mappings.
#' @seealso \code{\link{geom_segment}}
#' @export
geom_segment_tern <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {
  GeomSegmentTern$new(mapping = mapping, data = data, stat = stat,position = position, arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
}
GeomSegmentTern <- proto(ggplot2:::Geom,{
  objname <- "segment_tern"
  draw_groups <- function(. ,...){ .$draw(...) }
  draw <- function(., data, scales, coordinates, arrow = NULL,lineend = "butt", na.rm = FALSE, ...) {      
    data <- remove_missing(data,na.rm,c("x","y","z","xend","yend","zend"),name = "geom_segment_tern")
    if (empty(data)) return(zeroGrob())
    
    if(!inherits(coordinates,"ternary")){stop("geom_segment_tern is only applicable for ternary coordinate system.")}
    
    ix <- c(as.character(coordinates$T),as.character(coordinates$L),as.character(coordinates$R))

    tmp <- data[,ix];          colnames(tmp) <- c("T","L","R")
    data[,c("x","y")]       <- transform_tern_to_cart(T=tmp$T,L=tmp$L,R=tmp$R,Tlim=coordinates$limits$T,Llim=coordinates$limits$L,Rlim=coordinates$limits$R)
    
    tmp <- data[,paste0(ix,"end")]; colnames(tmp) <- c("T","L","R")
    data[,c("xend","yend")] <- transform_tern_to_cart(T=tmp$T,L=tmp$L,R=tmp$R,Tlim=coordinates$limits$T,Llim=coordinates$limits$L,Rlim=coordinates$limits$R)
    
    ggplot2:::GeomSegment$draw(.=., data=data, scales=scales,
                               coordinates=ggplot2:::coord_cartesian(xlim=coordinates$limits$x,
                                                                     ylim=coordinates$limits$y),
                               arrow = arrow, lineend=lineend,na.rm = na.rm,...)
  }
  draw_legend <- function(., data, ...) { ggplot2:::GeomSegment$draw_legend(.,data,...) }
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y","z","xend","yend","zend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})