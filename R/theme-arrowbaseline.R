.theme_arrowbaseline.values <- c("axis","ticks","labels")
.theme_arrowbaseline <- function(value){
  value = value[1]
  if(class(value) == 'character'){
    if(!value %in% .theme_arrowbaseline.values){
      stop("Invalid value, must be either 'axis', 'ticks' or 'labels' (or numeric 0, 1 or 2)",call.=FALSE)
    }
    value = which(.theme_arrowbaseline.values == value) - 1 #convert to numeric
    value = as.numeric(value)
  }
  if(is.numeric(value))
    value = as.numeric(min(max(as.integer(value),0),2)) #ensure valid
  theme(axis.tern.arrowbaseline=value) #to return
}

#' Set The Ternary Arrow Baseline
#' 
#' The ternary arrows can have an offset unit value (see \code{\link{axis.tern.arrowsep}}), however, it is convenient to set this relative
#' to either the axis, ticks or axis ticklabels (since the latter two can be hidden / removed.). This function permits this to be set
#' @param x a character ('axis','ticks' or 'labels') or numeric (rounded to 0, 1 or 2) value to determine the relative location (labels is default)
#' if a character is provided, and it is not one of the above, an error will be thrown.
#' @export
#' @rdname theme_arrowbaseline
#' @examples
#' #Create plot object
#' plot <- ggtern(data=data.frame(x=1,y=1,z=1),mapping=aes(x,y,z)) +
#'   geom_point() + theme_rgbw() +
#'   tern_limits(labels=c(10,20,"","","","","",80,90,100),
#'              breaks=seq(0.1,1,by=0.1)) +
#'   theme(axis.tern.arrowstart=0.25,axis.tern.arrowfinish=0.75) +
#'   theme_ticksoutside() +
#'   #to illustrate the actual origin, set arrowsep to zero
#'   theme(axis.tern.arrowsep=unit(0,"npc")) 
#'  
#'  #Default behaviour
#'  plot + theme_arrowbaseline(0)
#'  ##Alternatives, Uncomment below
#'  #plot + theme_arrowbaseline(1)
#'  #plot + theme_arrowbaseline(2)
#'  #plot + theme_arrowbaseline("axis")
#'  #plot + theme_arrowbaseline("ticks")
#'  #plot + theme_arrowbaseline("labels")
theme_arrowbaseline <- function(x='labels'){.theme_arrowbaseline(x)}