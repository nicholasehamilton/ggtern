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
get_last_coord <- .coordinates$get
set_last_coord <- .coordinates$set