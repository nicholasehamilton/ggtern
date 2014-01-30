
#' Stop Procedure
#' 
#' Internal Function, checks if the most recent coordinate system is ternary, and, if not, 
#' stops the current procedure, with a common message format
#' @param src character name of current procedure 
#' @keywords internal
tern_stop <- function(src="target"){
  lc <- get_last_coord()
  if(!inherits(lc,"ternary")){
    stop(paste(src,"only relevant for ternary coordinates."),call.=FALSE)
  }
}