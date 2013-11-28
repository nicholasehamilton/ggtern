##FUNCTION TO HACK INTO THE STANDARD
#.build_hack <- function(){
  
#  ##RETRIEVE THE EXISTING FUNCTION
#  .ggplot_build <- ggplot2:::ggplot_build
  
  #PATCH.
#  ggplot_build <- function(plot){    
#    plot <- ggtern_build(plot) ##CALL THE TERN EQUIVALENT.
#    .ggplot_build(plot)        ##CALL THE EXISTING FUNCTION.
#  }
  
  #PUSH BACK.
#  unlockBinding("ggplot_build", asNamespace("ggplot2"))
#    assign("ggplot_build", ggplot_build, asNamespace("ggplot2"))
#  lockBinding("ggplot_build", asNamespace("ggplot2"))
#}

ggtern_build <- function(plot){
  #IF WE HAVE TERNARY OPTIONS SOMEWHERE...
  if(!identical(plot$theme$ternary.options,NULL) | !identical(theme_update()$ternary.options,NULL)){  
    #STORE EXISTING LAYERS & MAPPINGS AND RESET TEMPORARILY...
    layers.existing <- plot$layers;   
    plot$layers     <- list()
    
    #CREATE THE TERNARY FRAMEWORK.
    plot <- plot + ternbackground(plot) #Apply Background
    plot <- plot + ternborder(plot)     #Apply Ternary Axes.
    plot <- plot + ternarrows(plot)     #Arrow Marker
    plot <- plot + ternlabels(plot)     #Apply Species Labels.
    plot <- plot + ternfixedcoords(plot)#Fixed Ratio
    
    plot <- plot + terngridlines(plot)  #Apply Gridlines
    plot <- plot + ternticks(plot)      #APPLY TICKS
    
    #Stack New layers on top of the base layers
    ##THE NEW LAYERS, IE, BACKGROUND, TICKS, AXES, GRIDS ETC UNDER THE ACTUAL LAYERS TO PLOT.
    plot$layers <- c(plot$layers,layers.existing) 
  }
  #scales <- plot$scales
  #scale_z <- function()scales$get_scales("z")
  #ggplot2:::scales_add_missing(plot, c("z"))
  ggplot2:::ggplot_build(plot)
}


