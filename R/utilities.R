#convert ternary data to xy data, 
transformTernToCart <- function(T,L,R,data=data.frame(T=T,L=L,R=R),scale=TRUE){
  if(scale){
    d <- abs(data)
  }else{
    d <- data
  }
  s <- rowSums(d);
  
  #If scale to composition sum of 1
  if(scale){
    ix <- which(s <= 0)
    if(length(ix) > 0){
      #Consider 0,0,0 to be equal parts (not strictly true, but, to prevent divide by zero)
      d[ix,] <- c(1,1,1)/3
      s[ix]  <- 1.0
    }
    for(i in 1:ncol(d)){d[,i] <- d[,i]/s}
  }
  
  #Do the actual transformation
  out.Y <- d[,1]*tan(pi*60/180)*0.5
  out.X <- d[,3] + out.Y*tan(30*pi/180)
  
  return(data.frame(x=out.X,y=out.Y))
}


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




















