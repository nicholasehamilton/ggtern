StatContour <- proto(ggplot2:::Stat, {
  objname <- "contour"
  calculate <- function(., data, scales, bins=NULL, binwidth=NULL, breaks = NULL, complete = FALSE, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, name = "stat_contour", finite = TRUE)
    # If no parameters set, use pretty bins
    if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
      breaks <- pretty(range(data$z), 10)
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(range(data$z)) / bins
    }
    # If necessary, compute breaks from binwidth
    if (is.null(breaks)) {
      breaks <- fullseq(range(data$z), binwidth)
    }
    contour_lines(data, breaks, complete = complete)
  }
  
  default_geom <- function(.) GeomPath
  default_aes <- function(.) aes(order = ..level..)
  required_aes <- c("x", "y", "z")
})

# v3d <- reshape2::melt(volcano)
# names(v3d) <- c("x", "y", "z")
#
# breaks <- seq(95, 195, length = 10)
# contours <- contour_lines(v3d, breaks)
# qplot(x, y, data = contours, geom = "path") + facet_wrap(~ piece)
contour_lines <- function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)
  
  cl <- contourLines(
    x = sort(unique(data$x)), 
    y = sort(unique(data$y)), 
    z = z, 
    levels = breaks
  )
  
  if (length(cl) == 0) {
    warning("Not possible to generate contour data", call. = FALSE)
    return(data.frame())
  }
  
  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")
  
  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
}