ggtern_build <- function(plot){
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...  
  if("ggtern" %in% class(plot) & ("gg" %in% class(plot) | "ggplot" %in% class(plot))){    
    #THE TERNARY COMPONENTS 
    tern.layer <- list( ternbackground(plot)
                       ,ternborder(plot)
                       ,ternarrows(plot)
                       ,ternlabels(plot)
                       ,terngridlines(plot)
                       ,ternticks(plot)
                )
    
    ##Add the missing scales
    ggplot2:::scales_add_missing(plot,c("x","y","T","L","R"),environment())
    
    ##INSERT COMPONENTS UNDERNEATH LAYERS...
    plot$layers <- c(tern.layer,plot$layers)
    
    xlim <- c(0,1); ylim <- c(0,1)
    tryCatch({
      PADDING <- max(plot$theme$ternary.options$padding, 0)
      #ARROWSEP<- max(plot$theme$ternary.options$arrowsep,0) 
      #BACKBY <- PADDING + ARROWSEP
      #YMAX <- as.numeric(transformTernToCart(1,0,0)[2])
      
      xlim <- c(0 - PADDING,1 + PADDING)
      ylim <- c(as.numeric(transformTernToCart(-PADDING,0,   PADDING,scale=F)[2]),
                as.numeric(transformTernToCart( 1+PADDING,0,-PADDING,scale=F)[2]))
    },error=function(e){
      #NOTHING
    })
    
    ##Add the ternary fixed coordinates
    #plot <- plot + ternfixedcoords(plot)
    if(!inherits(plot$coordinates,"ternary")){
      plot <- plot + coord_tern(xlim=xlim,ylim=ylim)
    }
    
    ##Update the coordinates limits.
    plot$coordinates$limits$T <- plot$scales$get_scales("T")$limits
    plot$coordinates$limits$L <- plot$scales$get_scales("L")$limits
    plot$coordinates$limits$R <- plot$scales$get_scales("R")$limits
  }
  
  ##Execute the normal  building process.
  ggplot2:::ggplot_build(plot)
}


