#' Calculate contours of 3d data (Ternary Version)
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "contour")}
#'
#' @inheritParams stat_identity
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @return A data frame with additional column:
#'  \item{level}{height of contour}
#' @export
stat_contour_tern <- function (mapping = NULL, data = NULL, geom = "ContourTern", position = "identity", na.rm = FALSE, ...) {
  StatContourTern$new(mapping = mapping, data = data, geom = geom, position = position, na.rm = na.rm, ...)
}

StatContourTern <- proto(ggint$Stat, {
  objname      <- "contour_tern"
  required_aes <- c("x", "y", "z","value")
  default_geom <- function(.) GeomContourTern
  default_aes  <- function(.) aes(order = ..level..)
  calculate    <- function(., data, scales, bins=NULL, binwidth=NULL, breaks = NULL, complete = FALSE, na.rm = FALSE, ...) {
    # Run Some Basic Checks
    coord = enforceTernaryCoordinates()
    data  = remove_missing(data, na.rm, name = "stat_contour_tern", finite = TRUE)
    check_required_aesthetics(unique(c(.$required_aes,coord$required_aes)),names(data),"stat_contour_tern")
    
    # Compute Binwidths and Breaks
    if(is.null(bins) && is.null(binwidth) && is.null(breaks)) { breaks <- pretty(range(data$value), 10)}
    if(!is.null(bins)) {  binwidth <- diff(range(data$value)) / bins }
    if(is.null(breaks)) { breaks <- fullseq(range(data$value), binwidth) }
    
    #contour_lines_tern(data, breaks, complete = complete)
    data$level = 1
    data
  }
})

# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# breaks <- seq(95, 195, length = 10)
# contours <- contour_lines(v3d, breaks)
# qplot(x, y, data = contours, geom = "path") + facet_wrap(~ piece)
contour_lines_tern <- function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)
  
  cl <- contourLines(
    x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
    levels = breaks)
  
  if (length(cl) == 0) {
    warning("Not possible to generate contour data", call. = FALSE)
    return(data.frame())
  }
  
  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels  <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs      <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys      <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces  <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups  <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")
  
  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
}
