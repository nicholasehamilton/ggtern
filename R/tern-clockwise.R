.tern_clockwise <- function(clockwise){theme(axis.tern.showarrows=TRUE,axis.tern.clockwise=clockwise)}

#' Direction of Ternary Rotation
#' 
#' \code{theme_clockwise, theme_anticlockwise} (or their aliasses) are function that instructs the axes 
#' precession to be clockwise or anticlockwise respectively.
#' 
#' If the \code{\link{axis.tern.showarrows}} value is \code{FALSE}, these functions will set it to \code{TRUE}.
#' @rdname theme_clockwise
#' @name theme_clockwise
NULL

#' @rdname theme_clockwise
#' @export
theme_clockwise        <- function(){.tern_clockwise(TRUE)}

#' @rdname theme_clockwise
#' @export
theme_anticlockwise    <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @export
theme_counterclockwise <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @export
tern_clockwise         <- function(){.tern_clockwise(TRUE)}

#' @rdname theme_clockwise
#' @export
tern_anticlockwise     <- function(){.tern_clockwise(FALSE)}

#' @rdname theme_clockwise
#' @export
tern_counterclockwise  <- function(){.tern_clockwise(FALSE)}
