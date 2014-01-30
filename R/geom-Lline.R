#' @rdname geom_TLRline
#' @aliases GeomLline
#' @export
geom_Lline <- function (mapping = NULL, data = NULL, stat = "Lline", position = "identity", show_guide = FALSE, ...) { 
  GeomLline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomLline <- proto(Geom, {
  objname <- "Lline"
  new <- function(., data = NULL, mapping = NULL, Lintercept = NULL, ...) {
    if (is.numeric(Lintercept)) {
      data <- data.frame(Lintercept = Lintercept)
      Lintercept <- NULL
      mapping <- aes_all(names(data))
    }
    .super$new(., data = data, mapping = mapping, inherit.aes = FALSE, Lintercept = Lintercept, ...)
  }
  draw <- function(., data, scales, coordinates, ...) {
    data <- unique(data[,which(!colnames(data) %in% "Lintercept")])
    ggint$GeomSegment$draw(data,scales,coordinates,...)
  }
  default_stat <- function(.) StatLline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})