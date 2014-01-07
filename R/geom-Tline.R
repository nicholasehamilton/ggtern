#' Add constant lines 
#' 
#' In ggplot2, one can add horizontal and vertical lines via the \code{\link{geom_hline}} and \code{\link{geom_vline}} prototypes, and these are the analogue
#' in the ternary space, however, they are not 'horizontal' or 'vertical' as such, rather, lines for constant values of one of the three species.
#' 
#' 
#' 
#' @aliases geom_Tline geom_Lline geom_Rline stat_Tline stat_Rline stat_Lline
#' @inheritParams ggplot2::geom_hline
#' @inheritParams ggplot2::stat_hline
#' @rdname constant-line
#' @name   constant-line
#' @param Tintercept for geom_Tline and stat_Tline, the constant value of the THS apex constituent.
#' @param Lintercept for geom_Lline and stat_Lline, the constant value of the LHS apex constituent.
#' @param Rintercept for geom_Rline and stat_Rline, the constant value of the RHS apex constituent.
#' @section Tline, Lline and Rline:
#' Convenience functions, \code{Tline(...), Lline(...)} and \code{Rline(...)} have been created so that constant lines can be quickly created:
#' @examples
#' #Base usage
#' ggtern()  + 
#'  geom_Lline(Lintercept=0.5,color="red") + 
#'  geom_Tline(Tintercept=0.5,color="green") + 
#'  geom_Rline(Rintercept=0.5,color="blue")
#'  
#' #Convenience Functions
#' ggtern() +  
#'  Tline(0.5,color="red") + 
#'  Lline(0.5,color="green") + 
#'  Rline(0.5,color="blue")
#' @export
NULL

#' @rdname constant-line
#' @aliases GeomTline
#' @export
geom_Tline <- function (mapping = NULL, data = NULL, stat = "Tline", position = "identity", show_guide = FALSE, ...) { 
  GeomTline$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomTline <- proto(Geom, {
  objname <- "Tline"
  new <- function(., data = NULL, mapping = NULL, Tintercept = NULL, ...) {
    if(is.numeric(Tintercept)){
      data <- data.frame(Tintercept = Tintercept)
      Tintercept <- NULL
      mapping <- aes_all(names(data))
    }
    .super$new(., data = data, mapping = mapping, inherit.aes = FALSE, Tintercept = Tintercept, ...)
  }
  draw <- function(., data, scales, coordinates, ...){
    data <- unique(data[,which(!colnames(data) %in% "TinterceptXXX")])
    ggint$GeomSegment$draw(data,scales,coordinates,...)
  }
  default_stat <- function(.) StatTline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})
