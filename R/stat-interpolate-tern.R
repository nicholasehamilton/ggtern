#' Ternary Interpolation
#' 
#' This is the heavily requested statistic for interpolating between ternary values, results being
#' rendered using contours on a ternary mesh
#'
#' @section Model Formula:
#' By default, the interpolation is done using multivariate linear regression using the 
#' following expression: \code{formula=value~poly(x,y,degree=2)}, where \code{value} is the response (dependent)
#' variable, and the independent predictor variables, \code{x} and \code{y} represent the variables 
#' matching to twp (2) out of the three (3) ternary axis definitions (\code{T,L} or \code{R}) 
#' as documented and defined within \code{\link{coord_tern}}.
#' 
#' Having said the above, the model formula can be changed to anything that suits the user by 
#' including \code{method} and/or \code{formula} arguments
#' which get passed through to the model fitting and prediction components of the calculation routine.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("stat", "InterpolateTern")}
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @return A data frame with additional column:
#'  \item{level}{height of contour}
#' @inheritParams ggtern::stat_density_tern
#' @inheritParams ggplot2::stat_density2d
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_smooth
#' @param method smoothing method (function) to use, eg. \code{\link{lm}}, \code{\link{glm}}, \code{\link{gam}}, 
#' \code{link{loess}} or \code{\link{rlm}}
#' @param ... other arguments passed on to the \code{method} argument above
#' @aliases StatInterpolateTern stat_interpolation interpolation
#' @seealso \code{\link{geom_interpolate_tern}}, \code{\link{lm}}, \code{\link{loess}}
#' @export
stat_interpolate_tern <- function (mapping  = NULL, 
                                   data     = NULL, 
                                   geom     = "InterpolateTern", 
                                   position = "identity", 
                                   na.rm    = FALSE,
                                   n        = getOption('tern.mesh.size'),
                                   buffer   = getOption('tern.mesh.buffer'),
                                   formula  = value~poly(x,y, degree = 2, raw=TRUE),
                                   method   = "lm",
                                   ...) {
  StatInterpolateTern$new(mapping = mapping, data = data, geom = geom, 
                          position = position, na.rm = na.rm, buffer=buffer,formula=formula,method=method,n=n,...)
}

StatInterpolateTern <- proto(ggint$Stat, {
  objname      <- "interpolate_tern"
  required_aes <- c("x", "y", "z","value")
  default_geom <- function(.) GeomInterpolateTern
  default_aes  <- function(.) aes(order = ..level.. )
  calculate    <- function(., data, 
                           scales,
                           na.rm        = FALSE, 
                           contour      = TRUE, 
                           geometry     = "interpolate_tern",
                           buffer       = getOption('tern.mesh.buffer'),
                           n            = getOption('tern.mesh.size'),
                           method       = "lm",
                           formula      = value~poly(x,y, degree = 2, raw=TRUE),
                           ...) {
    
    #Check the Coords
    last_coord <- get_last_coord()
    if(empty(data) | is.null(last_coord) | inherits(last_coord,"ternary") == FALSE){ return(data.frame()) }
    
    #ggtern
    df <- remove_missing(data, na.rm, name = "stat_contour_tern", finite = TRUE)
    
    #Normalize from 0 to 1
    df[,.$required_aes[1:3]] = df[,.$required_aes[1:3]]/rowSums(df[,.$required_aes[1:3]])
    
    #Produce the Model
    if(class(method) == 'character'){ if(method == "lm"){ func = lm }else if(method == "loess"){ 
      func = loess}
    }else{func=method}
    
    #Local function to adjust the range, depending on if inverse or not
    getLim <- function(lim){
      if(is.null(lim)) lim=c(0,1)
      if( !diff(lim) ) lim=c(0,1)
      lim
    }
    
    #Fit the Model
    fit           = safe.call(func,list(formula=formula,data=df,...))
    
    #Determine the Limits
    lims = c(expandRange(getLim(last_coord$limits$L),buffer[1]),
             expandRange(getLim(last_coord$limits$T),if(length(buffer) > 1){buffer[2]}else{buffer[1]}))
    x             = seq(lims[1],lims[2],length.out = n[1])
    y             = seq(lims[3],lims[4],length.out = if(length(n) > 1){n[2]}else{n[1]})
    df            = data.frame(expand.grid(x = x, y = y),PANEL=df$PANEL[1],group=df$group[1]); 
    df$z          = 1 - (df$x + df$y)
    
    #Try and Do the Prediction
    tryCatch({
      prediction    = suppressWarnings(predict(fit,newdata=df))
      df$z          = as.numeric(prediction)
    },error=function(e){
      stop(as.character(e))
    })
    
    #Get the Finite Values
    df <- remove_missing(df, na.rm, name = "stat_contour_tern", finite = TRUE)
    
    #Do the Contour
    if(contour) {
      df <- StatContour$calculate(df,scales,...)
    } else {
      df$level   = 1
      df$group   = 1
      df$piece   = 1
    }
    
    #Transform to cartesian coordinates
    df[,c('x','y')] = trytransform(data.frame(x=df$x,y=df$y,z=(1 - df$x - df$y)),last_coord)
    
    #Clip the Polygons
    df = clipPolygons(df,last_coord,c('group',"piece","level"))
    
    #UNDO (will be redone later with standard transformation routine)
    undoCartesian(df,last_coord)
  } 
})
