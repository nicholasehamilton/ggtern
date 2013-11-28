# Null default
# Analog of || from ruby
# 
# @keyword internal
# @name nulldefault-infix
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


validLabel <- function(desired,fallback=""){
  if(is.character(desired)){return(desired)}
  return(fallback)
}

arrow.label.formatter <- function(species,suffix){
  if(missing(suffix)){
    return(species)
  }else if(identical(suffix,NULL) | identical(suffix,"") | missing(suffix)){
    return(species)
  }else{
    return(paste(species,"/",suffix))
  }
}

calc_element_plot <- function(element,theme=theme_update(),verbose=F,...,plot=NULL){
  ret.plot  <- ggplot2:::calc_element(element,plot$theme,verbose=verbose)
  ret.theme <- ggplot2:::calc_element(element,theme=theme,verbose=verbose) 
  if(!identical(ret.plot,NULL)){
    ret <- ret.plot
  }else{
    ret <- ret.theme
  }
  return(ret)
}


pushback <- function(target,destination=target,namespace="ggplot2",check=T){
  if(check){if(!exists(target))stop("target does not exist")}
  if(class(target) != "character")stop("target must be specified as a character")
  writeLines(paste("patching:",target))
  unlockBinding(destination, asNamespace(namespace))
    assign(target, destination, asNamespace(namespace))
  lockBinding(destination, asNamespace(namespace))
}




















