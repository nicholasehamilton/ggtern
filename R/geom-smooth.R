#' Add a smoothed conditional mean (MODIFIED)
#' 
#' \code{geom_smooth} is a function that provides additional functionality to the standard \code{\link[ggplot2]{geom_smooth}} function in the 
#' event that it is being used on a \code{ggtern} object. Parameters are the same, with the exception of the introduction of the \code{limitarea} parameter.
#' 
#' @param limitarea logical value to indicate whether smoothed data is discared outside of the ternary plot area, when this is set to \code{TRUE}, 
#' the value of \code{fullrange} has no meaning, since the full-range becomes essentially becomes limited by the ternary plot area. 
#' When set to \code{FALSE}, \code{fullarea} behaves as per normal.
#' @param fullrange extend the range to the edge of the plotting panel.
#' @aliases GeomSmoothTern geom_smooth
#' @inheritParams ggplot2::geom_smooth
#' @export
geom_smooth_tern <- function (mapping = NULL, data = NULL, stat = "SmoothTern", position = "identity",limitarea=TRUE,fullrange=FALSE,...) { 
  limitarea <- ifthenelse(!is.logical(limitarea),TRUE,limitarea[1])
  fullrange <- ifthenelse(limitarea,TRUE,fullrange) #FORCE FULL RANGE IN ORDER TO BE ABLE TO TRUNCATE
  GeomSmoothTern$new(mapping = mapping, data = data, stat = stat, position = position,limitarea=limitarea,fullrange=fullrange,...)
}

GeomSmoothTern <- proto(Geom, {
  objname <- "smooth_tern"
  draw <- function(., data, scales, coordinates,limitarea=F,...) { 
    #HACK 4 GGTERN
    if(limitarea & inherits(get_last_coord(),"ternary")){data <- remove_outside(data)}
    
    ribbon <- transform(data, colour = NA)
    path   <- transform(data, alpha  = NA)
    
    has_ribbon <- function(x) !is.null(data$ymax) && !is.null(data$ymin)
    
    notransform({ #ggtern hack
      ret = gList(
        if (has_ribbon(data)) GeomRibbon$draw(ribbon, scales, coordinates),
        GeomLine$draw(path, scales, coordinates))
    })
    ret
  }
  guide_geom <- function(.) "smooth"
  default_stat <- function(.) StatSmoothtern
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="#3366FF", fill="grey60", size=0.5, linetype=1, weight=1, alpha=0.4)
  

  draw_legend <- function(., data, params, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    data$fill <- alpha(data$fill, data$alpha)
    data$alpha <- 1
    
    if (is.null(params$se) || params$se) {
      gTree(children = gList(
        rectGrob(gp = gpar(col = NA, fill = data$fill)),
        GeomPath$draw_legend(data, ...)
      ))      
    } else {
      GeomPath$draw_legend(data, ...)
    }
  }
})
