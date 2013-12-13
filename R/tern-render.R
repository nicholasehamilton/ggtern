

#' Draw plot on current graphics device.
#'
#' @inheritParams print.ggplot
#' @export
#' @method print ggtern
print.ggtern <- function(x, newpage = is.null(vp), vp = NULL, ...) {  
  print.ggplot(x=x,newpage=newpage,vp=NULL,...)
}

#' Draw plot on current graphics device.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @export
#' @method print ggplot
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ggint$set_last_plot(x)
  if (newpage) grid.newpage()
  
  data <- ggplot_build(x)
  
  gtable <- ggplot_gtable(data)
  if (is.null(vp)) {
    grid.draw(gtable) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable) 
    upViewport()
  }
  invisible(data)
}
