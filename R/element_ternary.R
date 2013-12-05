
#' Theme element: ternary structure
#' 
#' Used to define the layout of some of the ggtern plot features which are unique to the ternary diagrams , and hence, this package.
#' 
#' Note that some of these items, unlike \code{\link{ggplot2}} which permits absolute distances (ie say unit(1,"cm")) via the \code{\link{grid}} package, 
#' are instead defined as a relative length to the limits imposed by the underlying \code{x} and \code{y} axes. 
#' 
#' For example, the ternary axes are rendered directly onto the plot area, (rather than using an exclusive grid component) and since the \code{x} limits 
#' (unless changed) are between \code{[0,1]}, then a relative length of \code{0.02} represents a length that is approximately \code{2\%} 
#' of the plot edges from tip to tip. Elements which are subject to this include the \code{padding}, \code{arrowsep}, \code{ticklength.major} and 
#' \code{ticklength.minor}. This is perhaps not ideal, however, seemed to be a necessary sacrifice in order to produce these plots.
#' 
#' @param showarrows logical whether to show the axis directional arrows
#' @param padding the padding around the plot area to make provision for axis labels, ticks and arrows, relative to the cartesian plane.
#' @param arrowstart the proportion along the ternary axis to start the directional arrow
#' @param arrowfinish the proportion along the ternary axis to stop the directional arrow
#' @param ticklength.major the length of the major ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. 
#' @param ticklength.minor the length of the minor ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area.
#' @export
element_ternary <- function(showarrows       =TRUE,
                            padding          =0.15,
                            arrowsep         =0.10,
                            arrowstart       =0.25,
                            arrowfinish      =0.75,
                            ticklength.major =0.020,
                            ticklength.minor =0.010){
  structure(
    list(padding         = padding,
         arrowsep        = arrowsep,
         showarrows      = showarrows,
         arrowstart      = min(arrowstart,arrowfinish),
         arrowfinish     = max(arrowstart,arrowfinish),
         ticklength.major= ticklength.major,
         ticklength.minor= ticklength.minor),
    class = c("element_ternary")
  )
}