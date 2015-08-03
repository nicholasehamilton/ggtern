#' Ternary Interpolation
#' 
#' This is the heavily requested statistic for interpolating between ternary values, results being
#' rendered using contours on a ternary mesh
#'
#' By default, the interpolation is done using multivariate linear regression using the 
#' following \code{formula=value~poly(x,y,z,degree=2)}, 
#' however this can be changed to anything that suits the user by including \code{method} and/or \code{formula} arguments
#' which get passed through to the model fitting and prediction components of the calculation routine.
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("stat", "InterpolateTern")}
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @return A data frame with additional column:
#'  \item{level}{height of contour}
#' @inheritParams ggplot2::stat_density2d
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_path
#' @seealso \code{\link{geom_interpolate_tern}}, \code{\link{lm}}, \code{\link{loess}}
#' @param buffer factor to buffer the mesh, to prevent ugly truncation of contours, 1.0 means no buffering
#' @param ... other arguments passed on to the \code{method}, such as  \code{\link{lm}} (default) or \code{\link{loess}}
#' @aliases StatInterpolateTern
#' @export
stat_interpolate_tern <- function (mapping  = NULL, 
                                   data     = NULL, 
                                   geom     = "InterpolateTern", 
                                   position = "identity", 
                                   na.rm    = FALSE,
                                   buffer   = getOption('tern.mesh.buffer'),
                                   ...) {
  StatInterpolateTern$new(mapping = mapping, data = data, geom = geom, 
                          position = position, na.rm = na.rm, buffer=buffer, ...)
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
                           n            = 200,
                           method       = "lm",
                           formula      = value~poly(x,y,z, degree = 2, raw=TRUE),
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
    
    #Fit the Model
    fit           = safe.call(func,list(formula=formula,data=df,...))
    
    #Determine the Limits
    lims = c(expandRange(c(1,0),buffer),expandRange(c(1,0),buffer))
    x             = seq(lims[1],lims[2],length.out = n)
    y             = seq(lims[3],lims[4],length.out = n)
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
