.push_tweaks <- function(){
  #THEME ELEMENTS
  assignInNamespace(".element_tree",.element_tree,"ggplot2")
  assignInNamespace("theme_get",.theme_get,"ggplot2")
  assignInNamespace("theme_set",.theme_set,"ggplot2")
  
  #GEOMETRIES ETC.
  assignInNamespace("GeomSegment",ggint$GeomSegment,"ggplot2")
  assignInNamespace("StatSmooth"   ,ggint$StatSmooth,"ggplot2")
  
  assignInNamespace("panel_scales" ,.panel_scales,"ggplot2")
  
}


#MAIN BUILD FUNCTION
#assignInNamespace("train_ranges" ,ggint$train_ranges,"ggplot2")
#assignInNamespace("ggplot_build",ggplot_build,"ggplot2")
#assignInNamespace(".all_aesthetics",.all_aesthetics,"ggplot2")
#assignInNamespace("StatDensity2d",ggint$StatDensity2d,"ggplot2")