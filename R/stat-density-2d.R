#' 2d density estimation - (ggtern version)
#'
#' Patched version of the 2d density estimation.
#' @inheritParams ggtern::stat_density2d
#' @importFrom MASS kde2d
#' @name stat_density2d
#' @export
#' @seealso \code{\link[ggtern]{stat_density2d}}
stat_density2d <- function (mapping = NULL, data = NULL, geom = "Density2dTern", position = "identity", na.rm = FALSE, contour = TRUE, n = 100, ...) {
  StatDensity2dTern$new(mapping = mapping, data = data,geom=geom,geometry=geom,position = position, na.rm = na.rm, contour = contour, n = n,...)
}

#' @rdname undocumented
StatDensity2dTern <- proto(Statnew, {
  objname <- "Density2dTern"
  default_geom <- function(.) GeomDensity2dTern
  default_aes <- function(.) aes(colour = "#3366FF", size = 0.5)
  required_aes <- c("x", "y")
  calculate <- function(., data, scales, na.rm = FALSE, contour = TRUE, n = 100, geometry="density2d",...) {
    if(empty(data)){return(data.frame())}
    last_coord <- get_last_coord()
    
    #ggtern
    if(inherits(last_coord,"ternary"))
      data <- trytransform(data,last_coord)
    
    df <- data.frame(data[, c("x", "y")])
    df <- remove_missing(df, na.rm, name = "stat_density2d", finite = TRUE)
    
    #ggtern
    xlim <- ifthenelse(inherits(last_plot(),"ggtern"),c(1,0),scale_dimension(scales$x))
    ylim <- ifthenelse(inherits(last_plot(),"ggtern"),c(1,0),scale_dimension(scales$y))
    
    dens <- safe.call(kde2d, list(x = df$x, y = df$y, n = n,lims = c(xlim,ylim), ...))
    df <- with(dens, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
    
    #ggtern
    if(inherits(last_coord,"ternary")){df <- sink_density(df,remove=!identical(geometry,"polygon"),coord=last_coord)}
    
    df$group <- data$group[1]
    
    if (contour) {
      StatContour$calculate(df, scales, ...)      
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
      df
    }
  }  
})