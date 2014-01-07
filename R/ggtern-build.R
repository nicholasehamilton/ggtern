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
  plot <- ggint$plot_clone(plot)
  
  #if we have ternary coordinate system but not ternary plot class, make it ternary.
  if(inherits(plot$coordinates,"ternary")){
    if(!inherits(plot,"ggtern")){
      class(plot) <- c("ggtern",class(plot))
    }
    plot <- plot + .theme_nocart()
  }
  
  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  panel <- ggint$new_panel()
  #... CONTINUED BELOW...
  
  ##-------------------------------------------------------------------------------
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if(inherits(plot,"ggtern")){
    ##Strip Unapproved Ternary Layers
    plot$layers <- strip_unapproved(plot$layers)
    
    ##Check that there are layers.
    if(length(plot$layers) == 0){stop("No layers in plot",call.=F)}  
    
    #The ternary axis names.
    scales.tern <- c("T","L","R","x","y")
    
    #names(which(plot$coordinates=="y"))
    ix <- function(x){names(plot$coordinates)[which(plot$coordinates == x)]}
    
    ##Add the missing scales
    ggint$scales_add_missing(plot,scales.tern,environment())
    for(i in 1:3)
      assign(paste0("scale_",scales.tern[i]),plot$scales$get_scales(scales.tern[i]))
    
    ##Add the ternary fixed coordinates if it doesn't exist
    if(!inherits(plot$coordinates,"ternary")){plot <- plot + coord_tern()}
    
    ##Update the scales limits from the coordinate
    for(X in scales.tern)
      plot$coordinates$limits[[X]] <- is.numericor(.select.lim(plot$scales$get_scales(X)$limits,
                                                                plot$coordinates$limits[[X]]),c(0,1))
    
    #STORE COORDINATES FOR USE BY OTHER METHODS.
    set_last_coord(plot$coordinates)
    
    #THESE ARE A BIT OF A HACK. NORMALLY THIS INFO IS HANDLED IN THE GRID ARCHITECTURE.
    #BUT THIS IS ONE WAY OF PASSING IT THROUGH...
    panel <- .train_position_ternary(panel,get("scale_T"),get("scale_L"),get("scale_R"))
    panel$Wlabel = .Wlabel(panel,plot$labels)
    for(i in 1:3){
      x = scales.tern[i]
      panel[[paste0(x,"_scales")]]$name <- do.call(paste0(".",x,"label"),list(panel  = panel,labels = plot$labels))
    } 
  }else set_last_coord(NULL)
  
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
  
  #CONTINUED FROM ABOVE
  panel <- ggint$train_layout(panel, plot$facet, layer_data, plot$data)
  data  <- ggint$map_layout(panel, plot$facet, layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))  
  data <- lapply(data, ggint$add_group)
  
  # Transform all scales
  data <- lapply(data, ggint$scales_transform_df, scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  
  panel <- ggint$train_position(panel, data, scale_x(), scale_y())
  data  <- ggint$map_position(panel, data, scale_x(), scale_y())
  
  # Apply and map statistics
  data <- calculate_stats(panel, data, layers)
  
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  data <- lapply(data, ggint$order_groups)
  
  # Make sure missing (but required) aesthetics are added
  ggint$scales_add_missing(plot, c("x", "y"))
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))
  
  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))
  
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's 
  # displayed, or does it include the range of underlying data
  ggint$reset_scales(panel)
  panel <- ggint$train_position(panel, data, scale_x(), scale_y())
  data  <- ggint$map_position(panel, data, scale_x(), scale_y())
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()  
  if (npscales$n() > 0) {
    lapply(data, ggint$scales_train_df, scales = npscales)
    data <- lapply(data, ggint$scales_map_df, scales = npscales)
  }
  
  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)
  list(data = data, panel = panel, plot = plot)
}



