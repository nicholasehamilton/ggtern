

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @export
#' @method print ggplot
print.ggtern <- function(x, newpage = is.null(vp), vp = NULL, ...) {  
  options("tern.plot"=TRUE)
  ggplot2:::set_last_plot(x)
  if (newpage) grid.newpage()
  
  #data <- ggplot2:::ggplot_build(x)
  data <- ggtern_build(x)
  
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable) 
    upViewport()
  }
  options("tern.plot"=FALSE)
  invisible(data)
}

#' @rdname print.ggtern
#' @method plot ggplot
#' @export
plot.ggtern <- print.ggtern


#' Generate a ggplot2 plot grob.
#' 
#' @param x ggplot2 object
#' @keywords internal
#' @export
ggternGrob <- function(x) {
  writeLines("Producing Grob")
  ggtern_gtable(ggtern_build(x))
}
