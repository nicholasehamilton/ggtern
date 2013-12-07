ggtern_build <- function(plot){
  
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if("ggtern" %in% class(plot) & ("gg" %in% class(plot) | "ggplot" %in% class(plot))){
    
    ##Check that there are layers.
    if(length(plot$layers) == 0){stop("No layers in plot",call.=F)}
    
    ##Add the missing scales
    ggplot2:::scales_add_missing(plot,c("x","y","T","L","R"),environment())
    
    ##Add the ternary fixed coordinates
    ##One doesn't exist. So we create.
    ##This coordinate system controlls all the conversion from ternary to cartesian.
    if(!inherits(plot$coordinates,"ternary")){plot <- plot + coord_tern()}
    
    #Add the buffer for the labels, arrows, ticks and ternary axes.
    #This is normaly done via the expand parameter at the panel level, however, since
    #we are essentially nesting the ternary plot 'inside' the cartesian panel, this seems the only way.
    #note the arrows, ticks, axes (for x and y) are usually rendered OUTSIDE the plot panel, however, here, this
    #cannot be done by virtue of the shape of the ternary plot area.
    PADDING <- max(plot$theme$ternary.options$padding, 0)
    plot$coordinates$limits$x <- plot$coordinates$limits$x + c(-PADDING,PADDING)
    plot$coordinates$limits$y <- plot$coordinates$limits$y + c(-PADDING,PADDING)
    
    ##Update the coordinates limits from the scales.
    for(X in c("T","L","R")){lim <- plot$scales$get_scales(X)$limits; plot$coordinates$limits[[X]] <- ifthenelse(is.numeric(lim),lim,c(0,1))}
    
    #THE UNORTHADOX TERNARY COMPONENTS 
    tern.layer <- list(ternarrows(plot),ternlabels(plot),ternTicksGridAndAxes(plot))
    
    ##INSERT COMPONENTS UNDERNEATH LAYERS...
    plot$layers <- c(tern.layer,plot$layers)
    
    ##Destroy cartesian theme elements.
    plot <- plot + .theme_wipecartesian()
  }
  
  ##Execute the normal  building process.
  ggplot2:::ggplot_build(plot)
}


