#' Single line segments (Ternary Version).
#' 
#' Modified geom_segment geometry
#' @section Aesthetics: 
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "segment")}
#' In addition, the Ternary Version Requires:
#' \itemize{
#'  \item \strong{z}
#'  \item \strong{zend}
#' }
#'
#' @inheritParams ggplot2::geom_segment
#' @seealso \code{\link[ggplot2]{geom_segment}}
#' @export
geom_segment <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {
  ggint$GeomSegment$new(mapping = mapping, data = data, stat = stat,
                  position = position, arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
}

ggint$GeomSegment <- proto(Geom, {
  objname <- "segment"
  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", na.rm = FALSE, ...){
    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    required_aes <- apply(merge(required_aes,c("","end")),1,function(x){paste(x,collapse="")})
    check_required_aesthetics(required_aes, names(data),"geom_segment")
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes,"linetype", "size", "shape"),name = "geom_segment")
    if (empty(data)) return(.zeroGrob)
    
    if(is.linear(coordinates)) {
      #SPLIT The data by required aes.
      mid <- length(required_aes)/2
      data.start <- data[,required_aes[1:mid]]
      data.end   <- data[,required_aes[(mid+1):length(required_aes)]];       
      colnames(data.end) <- colnames(data.start)
      
      #do the transform
      data.start <- coord_transform(coordinates,data.start,scales)
      data.end   <- coord_transform(coordinates,data.end,  scales); 
      
      #Intersect by rownames to ensure cbind will deliver
      colnames(data.end) <- paste0(colnames(data.end),"end")
      ix <- intersect(intersect(rownames(data.start),
                                rownames(data.end)),
                      rownames(data))
      
      #If no entries, return empty grob
      if(length(ix) == 0){return(.zeroGrob)}
      
      #assembole
      data <- cbind(data[ix,which(!colnames(data) %in% required_aes)],data.start[ix,],data.end[ix,])
      
      #plot
      return(with(data, 
                  segmentsGrob(x, y, xend, yend, default.units="native",
                               gp = gpar(col=alpha(colour, alpha), fill = alpha(colour, alpha),
                                         lwd=size*.pt, lty=linetype, lineend = lineend),
                               arrow = arrow)
      ))
    }
    
    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"), warn_missing = FALSE)
    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]
    GeomPath$draw_groups(pieces, scales, coordinates, arrow = arrow, ...)
  }
  required_aes <- c("x", "y")
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})






