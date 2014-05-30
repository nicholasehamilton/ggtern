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

#' \code{get_last_coord} is a function that returns the last coordinate system used.
#' @rdname undocumented
#' @keywords internal
get_last_coord <- .coordinates$get

#' \code{set_last_coord} set the last coordinate system.
#' @param new the coordinate system to set
#' @rdname undocumented
#' @keywords internal
set_last_coord <- .coordinates$set
