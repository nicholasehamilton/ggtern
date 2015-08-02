#' 2d density estimation - (ggtern version)
#'
#' Patched version of the 2d density estimation.
#' @inheritParams ggtern::stat_density2d
#' @importFrom MASS kde2d
#' @name stat_density2d
#' @aliases StatDensity2dtern
#' @export
#' @seealso \code{\link[ggtern]{geom_density2d}}
stat_density_tern <- function ( mapping  = NULL, 
                                data     = NULL,
                                stat     = "DensityTern",
                                position = "identity", 
                                na.rm    = FALSE, 
                                contour  = TRUE, 
                                n        = 200, 
                                buffer   = getOption('tern.densitygrid.buffer'),...) {
  StatDensityTern$new(
    mapping  = mapping,data = data, buffer = buffer,
    position = position, na.rm = na.rm, contour = contour, n = n,...)
             
}


StatDensityTern <- proto(Statnew, {
  objname      <- "density_tern"
  required_aes <- c("x", "y","z")
  desc         <- "Density Estimate for Ternary Diagram"
  default_geom <- function(.) GeomDensityTern
  calculate    <- function(., data, scales,
                           na.rm = FALSE, 
                           contour = TRUE, 
                           geometry="density_tern",
                           buffer= getOption('tern.densitygrid.buffer'),n = n,...) { 
    
    #Check the Coords
    last_coord <- get_last_coord()
    if(empty(data) | is.null(last_coord) | inherits(last_coord,"ternary") == FALSE){ return(data.frame()) }
    
    #ggtern
    df <- remove_missing(data, na.rm, name = "stat_density_tern", finite = TRUE)
    
    #Normalize from 0 to 1
    df[,.$required_aes] = df[,.$required_aes]/rowSums(df[,.$required_aes])
    
    #Determine the Limits
    lims = c(expandRange(c(1,0),buffer),expandRange(c(1,0),buffer))
    
    #Execute the Density on the 
    dens = safe.call(kde2d,list(x = df$x, y = df$y, n = n ,lims = lims, ...))
    
    #Create the Data Frame
    df   = with(dens, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
    
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
    df = clipPolygons(df,last_coord,c('level','piece','group'))
    
    #UNDO (will be redone later with standard transformation routine)
    undoCartesian(df,last_coord)
  } 
})