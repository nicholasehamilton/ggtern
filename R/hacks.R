##Hack the coord_transform function.
.utilities_hack <- function(){
  unlockBinding("coord_transform", asNamespace("ggplot2"))
    assign("coord_transform", coord_transform, asNamespace("ggplot2"))
  lockBinding("coord_transform", asNamespace("ggplot2"))
}

##Hack into the theme elements.
.theme_hack <- function(){
  unlockBinding(".element_tree", asNamespace("ggplot2"))
    assign(".element_tree", .element_tree, asNamespace("ggplot2"))
  lockBinding(".element_tree", asNamespace("ggplot2"))
}

#Hack into the aesthetics.
.aes_hack <- function(){  
  unlockBinding(".all_aesthetics", asNamespace("ggplot2"))
  assign(".all_aesthetics", .all_aesthetics, asNamespace("ggplot2"))
  lockBinding(".all_aesthetics", asNamespace("ggplot2"))
}

.smooth_hack <- function(){
  unlockBinding("StatSmooth", asNamespace("ggplot2"))
    assign("StatSmooth", StatSmooth, asNamespace("ggplot2"))
  lockBinding("StatSmooth", asNamespace("ggplot2"))
  
  unlockBinding("StatDensity2d", asNamespace("ggplot2"))
    assign("StatDensity2d", StatDensity2d, asNamespace("ggplot2"))
  lockBinding("StatDensity2d", asNamespace("ggplot2"))
  
  unlockBinding("StatContour", asNamespace("ggplot2"))
    assign("StatContour", StatContour, asNamespace("ggplot2"))
  lockBinding("StatContour", asNamespace("ggplot2"))
  
  unlockBinding("contour_lines", asNamespace("ggplot2"))
    assign("contour_lines", contour_lines, asNamespace("ggplot2"))
  lockBinding("contour_lines", asNamespace("ggplot2"))
}