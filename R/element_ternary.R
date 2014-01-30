
#' Theme element: ternary structure
#' 
#' 
#' \strong{NB:} This function has been entirely \strong{DEPRECIATED} as of version 1.0.1.3 and, has been replaced by individual theme elements:
#' \enumerate{
#'   \item \code{\link{axis.tern.showarrows}}
#'   \item \code{\link{axis.tern.padding}}
#'   \item \code{\link{axis.tern.arrowsep}}
#'   \item \code{\link{axis.tern.arrowstart}}
#'   \item \code{\link{axis.tern.arrowfinish}}
#'   \item \code{\link{axis.tern.vshift}}
#'   \item \code{\link{axis.tern.hshift}}
#'   \item \code{\link{axis.tern.ticklength.major}}
#'   \item \code{\link{axis.tern.ticklength.minor}}
#' }
#' 
#' Used to define the layout of some of the ggtern plot features which are unique to the ternary diagrams , and hence, this package.
#' 
#' @param showarrows logical whether to show the axis directional arrows DEPRECIATED
#' @param padding the padding around the plot area to make provision for axis labels, ticks and arrows, relative to the cartesian plane. DEPRECIATED
#' @param arrowsep the distance between ternary axis and ternary arrows DEPRECIATED
#' @param arrowstart the proportion along the ternary axis to start the directional arrow DEPRECIATED
#' @param arrowfinish the proportion along the ternary axis to stop the directional arrow DEPRECIATED
#' @param vshift shift the plot area vertically DEPRECIATED
#' @param hshift shift the plot area horizontally DEPRECIATED
#' @param ticklength.major the length of the major ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. DEPRECIATED
#' @param ticklength.minor the length of the minor ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. DEPRECIATED
#' @keywords internal
#' @export
element_ternary <- function(showarrows,
                            padding,
                            arrowsep,
                            arrowstart,
                            arrowfinish,
                            vshift,
                            hshift,
                            ticklength.major,
                            ticklength.minor){
  if(!missing(ticklength.major))
    tern_dep("1.0.1.3","ticklength.major has been replaced by element 'axis.tern.ticklength.major'")
  if(!missing(ticklength.minor))
    tern_dep("1.0.1.3","ticklength.minor has been replaced by element 'axis.tern.ticklength.minor'")
  if(!missing(showarrows))
    tern_dep("1.0.1.3","showarrows has been replaced by element 'axis.tern.showarrows'")
  if(!missing(padding))
    tern_dep("1.0.1.3","padding has been replaced by element 'axis.tern.showarrows'")
  if(!missing(arrowsep))
    tern_dep("1.0.1.3","arrowsep has been replaced by element 'axis.tern.arrowsep'")
  if(!missing(vshift))
    tern_dep("1.0.1.3","vshift has been replaced by element 'axis.tern.vshift'")
  if(!missing(hshift))
    tern_dep("1.0.1.3","hshift has been replaced by element 'axis.tern.hshift'")
  if(!missing(arrowstart))
    tern_dep("1.0.1.3","arrowstart has been replaced by element 'axis.tern.arrowstart'")
  if(!missing(arrowfinish))
    tern_dep("1.0.1.3","hshift has been replaced by element 'axis.tern.arrowfinish'")
    
  #Return the object.
  structure(
    list(),
    class = c("element_ternary")
  )
}



