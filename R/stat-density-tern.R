#' 2d density estimation - (ggtern version)
#'
#' Patched version of the 2d density estimation.
#' @inheritParams ggtern::stat_density2d
#' @importFrom MASS kde2d
#' @name stat_density2d
#' @aliases StatDensity2dtern
#' @export
#' @seealso \code{\link[ggtern]{geom_density2d}}
stat_density_tern <- function (mapping = NULL, data = NULL,
                            position = "identity", na.rm = FALSE, contour = TRUE, n = 100,weight=NULL, 
                            buffer = getOption('tern.densitygrid.buffer'),...) {
  #geometry = iflasttern('density2dtern','density2d'); print(geometry)
  StatDensityTern$new(
    mapping  = mapping,data = data, buffer = buffer,
    geometry = 'DensityTern',
    position = position, na.rm = na.rm, contour = contour, n = n,...)
             
}



StatDensityTern <- proto(Statnew, {
  objname      <- "density_tern"
  required_aes <- c("x", "y")
  desc         <- "Density Estimate for Ternary Diagram"
  default_geom <- function(.) GeomDensityTern
  calculate    <- function(., data, scales,...,na.rm = FALSE, contour = TRUE, 
                           geometry="density_tern",buffer= getOption('tern.densitygrid.buffer'),n = 100*buffer) { 
  
  # geom="Density2dTern"
    last_coord <- get_last_coord()
    if(empty(data) | is.null(last_coord)){ return(data.frame()) }
    
    #ggtern
    if(inherits(last_coord,"ternary")){ data <- trytransform(data,last_coord) }
    df <- data.frame(data[, which(colnames(data) %in% c("x", "y","weight"))])
    df <- remove_missing(df, na.rm, name = "stat_density2d", finite = TRUE)
    
    #Determine the Limits
    getLim <- function(x=TRUE) ifthenelse(inherits(last_plot(),"ggtern"),c(1,0)*if(x){1}else{coord_aspect.ternary()},scale_dimension(scales[if(x){'x'}else{'y'}]))
    lims    = c(expandRange(getLim(T),buffer),expandRange(getLim(F),buffer))
    
    #Weighted or not
    wght = is.numericor(df$weight,1)
    func = if(is.numeric(wght) & length(unique(wght)) > 1 & length(wght) == nrow(df)){kde2d.weighted}else{kde2d}
    dens = safe.call(func,list(x = df$x, y = df$y, n = n,lims = lims,w=wght, ...))
    #wr = range(w)
    #if(diff(wr) != 0){ dens$z <- (dens$z - min(dens$z))/(diff(range(dens$z)))*diff(wr) + wr[1] }

    #Create the Data Frame
    df        = with(dens, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
    df$group  = data$group[1]
    
    #Do the Contour
    if(contour) {
      df <- StatContour$calculate(df,scales,...)   
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level = 1
      df$piece = 1
    }
    
    #Clip the Polygons
    df = clipPolygons(df,last_coord,c('level','piece','group'))
    
    #UNDO (will be redone later with standard transformation routine)
    undoCartesian(df,last_coord)
  }  
})