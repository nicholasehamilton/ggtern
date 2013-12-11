.tern_clockwise <- function(x){
  if(!is.logical(x)){x=FALSE}
  coord <- get_last_coord()
  if(!inherits(coord,"ternary")){
    coord <- coord_tern()
  }
  coord$clockwise=x
  coord
  #theme(ternary.options.clockwise=x[1])
}

#' Direction of Ternary Rotation
#' 
#' \code{tern_clockwise} is a function that instructs the axes precession to be clockwise
#' @name   ternary_clockwise
#' @rdname ternary_clockwise
#' @examples plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_clockwise()
#' @export
tern_clockwise        <- function(){.tern_clockwise(TRUE)}

#' Direction of Ternary Rotation
#' 
#' \code{tern_anticlockwise} is a function that instructs the axes precession to be anticlockwise
#' @alias  ternary_clockwise
#' @rdname ternary_clockwise
#' @examples plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_antiwise()
#' @export
tern_anticlockwise    <- function(){.tern_clockwise(FALSE)}

#' Direction of Ternary Rotation
#' 
#' \code{tern_counterclockwise} is an alias for \code{tern_anticlockwise}
#' @alias  ternary_clockwise
#' @rdname ternary_clockwise
#' @examples plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_counterclockwise()
#' @export
tern_counterclockwise <- function(){.tern_clockwise(FALSE)}