#' Build ggplot for rendering (MODIFIED for ggtern)
#'
#' This function takes the plot object, and performs all steps necessary to
#' produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{link{benchplot}} for 
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  plot <- ggplot2:::plot_clone(plot)
  
  #if we have ternary coordinate system but not ternary plot class, make it ternary.
  if(inherits(plot$coordinates,"ternary")){
    if(!inherits(plot,"ggtern")){
      class(plot) <- c("ggtern",class(plot))
      plot <- plot + theme_nocart()
    }
  }
  
  ##-------------------------------------------------------------------------------
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if(inherits(plot,"ggtern")){
    ##Check that there are layers.
    if(length(plot$layers) == 0){stop("No layers in plot",call.=F)}
    
    #The ternary axis names.
    scales.tern <- c("T","L","R")
    
    ##Add the missing scales
    ggplot2:::scales_add_missing(plot,scales.tern,environment())
    
    ##Add the ternary fixed coordinates if it doesn't exist
    if(!inherits(plot$coordinates,"ternary")){
      plot <- plot + coord_tern()
    }
    
    ##Update the coordinates limits from the scales.
    for(X in scales.tern){
      plot$coordinates$limits[[X]] <- is.numeric.or(plot$scales$get_scales(X)$limits,c(0,1))
    }
  }
  
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }
  
  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  panel <- ggplot2:::new_panel()
  panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, plot$data)
  data  <- ggplot2:::map_layout(panel, plot$facet, layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  data <- lapply(data, ggplot2:::add_group)
  
  # Transform all scales
  data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data  <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
  
  # Apply and map statistics
  data <- ggplot2:::calculate_stats(panel, data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  data <- lapply(data, ggplot2:::order_groups)
  
  # Make sure missing (but required) aesthetics are added
  ggplot2:::scales_add_missing(plot, c("x", "y"))
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))
  
  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))
  
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's 
  # displayed, or does it include the range of underlying data
  ggplot2:::reset_scales(panel)
  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data  <- ggplot2:::map_position(panel, data, scale_x(), scale_y())
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()  
  if (npscales$n() > 0) {
    lapply(data, ggplot2:::scales_train_df, scales = npscales)
    data <- lapply(data, ggplot2:::scales_map_df, scales = npscales)
  }
  
  # Train coordinate system
  panel <- ggplot2:::train_ranges(panel, plot$coordinates)
  
  list(data = data, panel = panel, plot = plot)
}



