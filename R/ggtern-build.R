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
    if(inherits(plot,"ggplot")) class(plot) <- c("ggtern",class(plot)) 
    plot <- plot + .theme_nocart() #destroy any cartesian theme elements
  }
  
  # Initialise panels, add extra data for margins
  panel <- ggint$new_panel()
  
  ##-------------------------------------------------------------------------------
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if(inherits(plot,"ggtern")){
    ##Strip Unapproved Ternary Layers
    plot$layers <- strip_unapproved(plot$layers)
    
    ##Check that there are layers remaining after potentially stripping all layers
    if(length(plot$layers) == 0){stop("No layers in ternary plot",call.=F)}  
    
    ##Add the ternary fixed coordinates if it doesn't exist
    if(!inherits(plot$coordinates,"ternary")){plot <- plot + coord_tern()}
    
    #The ternary and cartesian axis names.
    scales.tern <- plot$coordinates$required_axes #c("T","L","R")
    scales.aes  <- plot$coordinates$required_aes  #c("x","y","z")
    scales.cart <- c("x","y")
    
    ##Add the missing scales
    ggint$scales_add_missing(plot,c(scales.tern,scales.cart),environment())
    
    ##Update the scale limits from the coordinate system
    for(X in c(scales.tern,scales.cart))
      plot$coordinates$limits[[X]] <- is.numericor(.select.lim(plot$scales$get_scales(X)$limits, plot$coordinates$limits[[X]]),c(0,1))
    
    #Store coordinates for use by other methods, AFTER the limits have been updated (ie the previous command)
    set_last_coord(plot$coordinates) 
    
    #Normally this info is handled in the by grid, however, this is one way of passing it through
    panel <- .train_position_ternary(panel,
                                     plot$scales$get_scales("T"),
                                     plot$scales$get_scales("L"),
                                     plot$scales$get_scales("R"))
    
    #get snapshot of panel so updates to panel dont interfere through the next loop.
    panel.bup <- panel 
    
    #Assign the names Ternary scales and Wlabel (arrow label suffix) to the panel
    for(X in scales.tern){
      #Resolve the 'effective' T, L and R index, for case of non default coord_tern T, L and R assignments
      XResolved <- scales.tern[which(scales.aes == plot$coordinates[[X]])] 
      #Make update scale names, on effective axes.
      #Executes .Tlabel, .Llabel and .Rlabel
      panel[[paste0(X,"_scales")]]$name <- do.call(paste0(".",X,"label"),list(panel = panel.bup,labels = plot$labels))
    }
    
    #Make update to axes arrow suffix label
    panel$Wlabel = .Wlabel(panel,labels = plot$labels)
    
    #DONE
  }else{ set_last_coord(NULL) }
  
  layers     <- plot$layers
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
  data  <- ggint$map_position(panel,   data, scale_x(), scale_y())
  
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
  data  <- ggint$map_position(panel,   data, scale_x(), scale_y())
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()  
  if (npscales$n() > 0) {
    lapply(data, ggint$scales_train_df, scales = npscales)
    data <- lapply(data, ggint$scales_map_df, scales = npscales)
  }
  
  # Train coordinate system
  panel <- train_ranges(panel,plot$coordinates)
  
  #Remove colors if they are in proximity to the perimeter
  #data  <- suppressColours(data,plot$layers,plot$coordinates)
  
  #return
  list(data = data, panel = panel, plot = plot)
}

