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