##GLOBAL STORAGE FOR EXISTING COORDINATES
##this info wasn't available, either because set_last_plot hasn't been called yet.
##based on the .theme template used in ggplot2.
.coordinates <- (function(){
  coordinates <- coord_tern()
  list(
    get=function(){
      coordinates
    },
    set=function(new){
      old <- coordinates
      coordinates <<- new
      invisible(old)
    }
  )
})()

#' Last Coordinates
#' 
#' Internal functions to get and set the last coordinate system. An internal command for ggtern.
#' 
#' \code{get_last_coord} is a function that returns the last coordinate system used.
#' @rdname lastcoords
#' @return coordinate system
#' @export
get_last_coord <- .coordinates$get

#' Last Coordinates
#' 
#' \code{set_last_coord} set the last coordinate system.
#' @param new the coordinate system to set
#' @rdname lastcoords
#' @export
set_last_coord <- .coordinates$set
