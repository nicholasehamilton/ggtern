#' @rdname geom_TLRline
#' @aliases GeomRline
#' @export
geom_Rline <- function (mapping = NULL, data = NULL, stat = "Rline", position = "identity", show_guide = FALSE, ...) { 
  GeomRline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomRline <- proto(Geom, {
  objname <- "Rline"
  new <- function(., data = NULL, mapping = NULL, Rintercept = NULL, ...) {
    if (is.numeric(Rintercept)) {
      data <- data.frame(Rintercept = Rintercept)
      Rintercept <- NULL
      mapping <- aes_all(names(data))
    }
    .super$new(., data = data, mapping = mapping, inherit.aes = FALSE, Rintercept = Rintercept, ...)
  }
  draw <- function(., data, scales, coordinates, ...) {
    data <- unique(data[,which(!colnames(data) %in% "Rintercept")])
    ggint$GeomSegment$draw(data,scales,coordinates,...)
  }
  default_stat <- function(.) StatRline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})