.tern_clockwise <- function(x){theme(axis.tern.clockwise=x)}

#' Direction of Ternary Rotation
#' 
#' \code{tern_clockwise} is a function that instructs the axes precession to be clockwise
#' @rdname ternary_clockwise
#' @examples 
#' \donttest{
#' plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_clockwise()
#' plot
#' }
#' @export
tern_clockwise        <- function(){.tern_clockwise(TRUE)}

#' Direction of Ternary Rotation
#' 
#' \code{tern_anticlockwise} is a function that instructs the axes precession to be anticlockwise
#' @rdname ternary_clockwise
#' @examples 
#' \donttest{
#' plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_anticlockwise()
#' plot
#' }
#' @export
tern_anticlockwise    <- function(){.tern_clockwise(FALSE)}

#' Direction of Ternary Rotation
#' 
#' \code{tern_counterclockwise} is an alias for \code{tern_anticlockwise}
#' @rdname ternary_clockwise
#' @examples  
#' \donttest{
#' plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + 
#'                   geom_point() + 
#'                   tern_counterclockwise()
#' plot
#' }
#' @export
tern_counterclockwise <- function(){.tern_clockwise(FALSE)}

#' Direction of Ternary Rotation
#' 
#' \code{theme_clockwise} is an alias for \code{tern_clockwise}
#' @rdname ternary_clockwise
#' @export
theme_clockwise <- function(){tern_clockwise()}

#' Direction of Ternary Rotation
#' 
#' \code{theme_anticlockwise} is an alias for \code{tern_anticlockwise}
#' @rdname ternary_clockwise
#' @export
theme_anticlockwise <- function(){tern_anticlockwise()}

#' Direction of Ternary Rotation
#' 
#' \code{theme_counterclockwise} is an alias for \code{tern_counterclockwise}
#' @rdname ternary_clockwise
#' @export
theme_counterclockwise <- function(){tern_counterclockwise()}
