
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
#' @param arrowsep the distance between ternary axis and ternary arrows
#' @param arrowstart the proportion along the ternary axis to start the directional arrow
#' @param arrowfinish the proportion along the ternary axis to stop the directional arrow
#' @param vshift shift the plot area vertically
#' @param hshift shift the plot area horizontally
#' @param ticklength.major the length of the major ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. SUPERCEDED
#' @param ticklength.minor the length of the minor ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. SUPERCEDED
#' @export
element_ternary <- function(showarrows       =TRUE,
                            padding          =0.10,
                            arrowsep         =0.075,
                            arrowstart       =0.3,
                            arrowfinish      =0.7,
                            vshift           =0.25*padding,
                            hshift           =0,
                            ticklength.major,
                            ticklength.minor){
  if(!missing(ticklength.major))
    tern_dep("1.0.1.3","ticklength.major is replaced by element 'axis.tern.ticklength.major'")
  if(!missing(ticklength.minor))
    tern_dep("1.0.1.3","ticklength.minor is replaced by element 'axis.tern.ticklength.minor'")
  
  structure(
    list(padding         = padding,
         arrowsep        = arrowsep,
         showarrows      = showarrows,
         arrowstart      = min(arrowstart,arrowfinish),
         arrowfinish     = max(arrowstart,arrowfinish),
         vshift          = vshift,
         hshift          = hshift),
    class = c("element_ternary")
  )
}



