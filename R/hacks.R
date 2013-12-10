##Hack into the theme elements.
.theme_hack <- function(){
  unlockBinding(".element_tree", asNamespace("ggplot2"))
    assign(".element_tree", .element_tree, asNamespace("ggplot2"))
  lockBinding(".element_tree", asNamespace("ggplot2"))
  
  unlockBinding("theme_get", asNamespace("ggplot2"))
   assign("theme_get", theme_get, asNamespace("ggplot2"))
  lockBinding("theme_get", asNamespace("ggplot2"))
  
  unlockBinding("theme_set", asNamespace("ggplot2"))
    assign("theme_set", theme_set, asNamespace("ggplot2"))
  lockBinding("theme_set", asNamespace("ggplot2"))
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

.geom_hack <- function(){
  unlockBinding("GeomSegment", asNamespace("ggplot2"))
    assign("GeomSegment", GeomSegment, asNamespace("ggplot2"))
  lockBinding("GeomSegment", asNamespace("ggplot2"))
}

.panel_hack <- function(){
  #PANEL TRAINING
  unlockBinding("panel_scales", asNamespace("ggplot2"))
  assign("panel_scales", panel_scales, asNamespace("ggplot2"))
  lockBinding("panel_scales", asNamespace("ggplot2"))
  
  unlockBinding("train_ranges", asNamespace("ggplot2"))
  assign("train_ranges", train_ranges, asNamespace("ggplot2"))
  lockBinding("train_ranges", asNamespace("ggplot2"))
}

.ggplot_hack <- function(){
  unlockBinding("ggplot_build", asNamespace("ggplot2"))
  assign("ggplot_build", ggplot_build, asNamespace("ggplot2"))
  lockBinding("ggplot_build", asNamespace("ggplot2"))
}

