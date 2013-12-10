.push_tweaks <- function(){
  #MAIN BUILD FUNCTION
  utils::assignInNamespace("ggplot_build",ggplot_build,"ggplot2")
  
  #THEME ELEMENTS
  utils::assignInNamespace(".element_tree",.element_tree,"ggplot2")
  utils::assignInNamespace("theme_get",.theme_get,"ggplot2")
  utils::assignInNamespace("theme_set",.theme_set,"ggplot2")
  utils::assignInNamespace(".all_aesthetics",.all_aesthetics,"ggplot2")
  
  #GEOMETRIES ETC.
  utils::assignInNamespace("StatSmooth",.StatSmooth,"ggplot2")
  utils::assignInNamespace("GeomSegment",.GeomSegment,"ggplot2")
  utils::assignInNamespace("StatDensity2d",.StatDensity2d,"ggplot2")
  

  utils::assignInNamespace("panel_scales",.panel_scales,"ggplot2")
  utils::assignInNamespace("train_ranges",.train_ranges,"ggplot2")
}

