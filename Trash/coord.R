#' New coordinate system.
#'
#' Internal use only.
#'
#' @param ... object fields
#' @keywords internal
#' 
#' @name coord_aspect
#' @alias coord
#' @rdname coord
coord_aspect <- function(coord, ranges) UseMethod("coord_aspect")

#' @name coord_labels
#' @alias coord
#' @rdname coord
coord_labels <- function(coord, scales) UseMethod("coord_labels")

#' @name coord_render_fg
#' @alias coord
#' @rdname coord
coord_render_fg <- function(coord, scales, theme) UseMethod("coord_render_fg")

#' @name coord_render_bg
#' @alias coord
#' @rdname coord
coord_render_bg <- function(coord, scales, theme) UseMethod("coord_render_bg")

#' @name coord_render_axis_h
#' @alias coord
#' @rdname coord
coord_render_axis_h <- function(coord, scales, theme) UseMethod("coord_render_axis_h")

#' @name coord_render_axis_v
#' @alias coord
#' @rdname coord
coord_render_axis_v <- function(coord, scales, theme) UseMethod("coord_render_axis_v")

#' @name coord_range
#' @alias coord
#' @rdname coord
coord_range <- function(coord, scales)UseMethod("coord_range")

#' @name coord_train
#' @alias coord
#' @rdname coord
coord_train <- function(coord, scales) UseMethod("coord_train")

#' @name coord_transform
#' @alias coord
#' @rdname coord
coord_transform <- function(coord, data, range) UseMethod("coord_transform")

#' @name coord_distance
#' @alias coord
#' @rdname coord
coord_distance <- function(coord, x, y, details)UseMethod("coord_distance")

#' @name is.linear
#' @alias coord
#' @rdname coord
is.linear <- function(coord) UseMethod("is.linear")

#' @name coord_expand_defaults
#' @alias coord
#' @rdname coord
coord_expand_defaults <- function(coord, scale, aesthetic = NULL) UseMethod("coord_expand_defaults")



