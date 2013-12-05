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
    PADDING <- max(plot$theme$ternary.options$padding, 0)
    plot$coordinates$limits$x <- plot$coordinates$limits$x + c(-PADDING,PADDING)
    plot$coordinates$limits$y <- plot$coordinates$limits$y + c(-PADDING,PADDING)
    
    #Modify the labels accordingly....
    .val <- function(desired,fallback=""){ifthenelse(is.character(desired),desired,fallback)}
    plot <- plot + labs(T=.val(plot$labels[[plot$coordinates$T]],"T"), 
                        L=.val(plot$labels[[plot$coordinates$L]],"L"), 
                        R=.val(plot$labels[[plot$coordinates$R]],"R"),
                        W=.val(plot$labels$W,""))
    
    ##Update the coordinates limits from the scales.
    for(X in c("T","L","R")){plot$coordinates$limits[[X]] <- plot$scales$get_scales(X)$limits}
    
    #THE TERNARY COMPONENTS 
    tern.layer <- list(  ternbackground(plot)
                        ,ternborder(plot)
                        ,ternarrows(plot)
                        ,ternlabels(plot)
                        ,ternTicksGridAndAxes(plot))
    
    ##INSERT COMPONENTS UNDERNEATH LAYERS...
    plot$layers <- c(tern.layer,plot$layers)
    
    ##Destroy cartesian theme elements.
    plot <- plot + .theme_wipecartesian()
  }
  
  ##Execute the normal  building process.
  ggplot2:::ggplot_build(plot)
}


