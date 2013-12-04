ggtern_build <- function(plot){
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if("ggtern" %in% class(plot) & ("gg" %in% class(plot) | "ggplot" %in% class(plot))){
    ##Add the missing scales
    ggplot2:::scales_add_missing(plot,c("x","y","T","L","R"),environment())
    
    ##Determine x and y limits.
    xlim <- ylim <- c(0,1); 
    tryCatch({
      PADDING <- max(plot$theme$ternary.options$padding, 0)
      xlim <- c(0 - PADDING, 1 + PADDING)
      ylim <- c(as.numeric(transformTernToCart(-PADDING,0,   PADDING,scale=F)[2]),
                as.numeric(transformTernToCart( 1+PADDING,0,-PADDING,scale=F)[2]))
    },error=function(e){
      #NOTHING
    })
    
    ##Add the ternary fixed coordinates
    if(!inherits(plot$coordinates,"ternary")){plot <- plot + coord_tern(xlim=xlim,ylim=ylim)}
    
    ##Update the coordinates limits.
    for(X in c("T","L","R")){ plot$coordinates$limits[[X]] <- plot$scales$get_scales(X)$limits}
    
    #THE TERNARY COMPONENTS 
    tern.layer <- list(  ternbackground(plot)
                        ,ternborder(plot)
                        ,ternarrows(plot)
                        ,ternlabels(plot)
                        ,ternTicksGridAndAxes(plot))
    
    ##INSERT COMPONENTS UNDERNEATH LAYERS...
    plot$layers <- c(tern.layer,plot$layers)
    
  }
  
  ##Execute the normal  building process.
  ggplot2:::ggplot_build(plot)
}


