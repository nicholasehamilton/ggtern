.push_tweaks <- function(){
  #MAIN BUILD FUNCTION
  assignInNamespace("ggplot_build",ggplot_build,"ggplot2")
  
  #THEME ELEMENTS
  assignInNamespace(".element_tree",.element_tree,"ggplot2")
  assignInNamespace("theme_get",.theme_get,"ggplot2")
  assignInNamespace("theme_set",.theme_set,"ggplot2")
  assignInNamespace(".all_aesthetics",.all_aesthetics,"ggplot2")
  
  #GEOMETRIES ETC.
  assignInNamespace("StatSmooth",.StatSmooth,"ggplot2")
  assignInNamespace("GeomSegment",.GeomSegment,"ggplot2")
  assignInNamespace("StatDensity2d",.StatDensity2d,"ggplot2")
  

  assignInNamespace("panel_scales",.panel_scales,"ggplot2")
  assignInNamespace("train_ranges",.train_ranges,"ggplot2")
}

