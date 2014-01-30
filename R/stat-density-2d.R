#' 2d density estimation - (ggtern version)
#'
#' Patched version of the 2d density estimation.
#' @inheritParams ggtern::stat_density2d
#' @importFrom MASS kde2d
#' @name stat_density2d
#' @aliases StatDensity2dtern
#' @export
#' @seealso \code{\link[ggtern]{geom_density2d}}
stat_density2d <- function (mapping = NULL, data = NULL, geom = "density2dtern", position = "identity", na.rm = FALSE, contour = TRUE, n = 100, ...) {
  StatDensity2dtern$new(mapping = mapping, data = data,geom=geom,geometry=geom,position = position, na.rm = na.rm, contour = contour, n = n,...)
}

StatDensity2dtern <- proto(Statnew, {
  objname <- "density2dtern"
  default_geom <- function(.) GeomDensity2dtern
  required_aes <- c("x", "y")
  calculate <- function(., data, scales, na.rm = FALSE, contour = TRUE, n = 100,geometry="density2dtern",...) { # geom="Density2dTern"
    if(empty(data))
      return(data.frame())
    
    #ggtern
    last_coord <- get_last_coord()
    if(inherits(last_coord,"ternary"))
      data <- trytransform(data,last_coord)
    
    df <- data.frame(data[, which(colnames(data) %in% c("x", "y","weight"))])
    df <- remove_missing(df, na.rm, name = "stat_density2d", finite = TRUE)
    
    #ggtern
    xlim <- ifthenelse(inherits(last_plot(),"ggtern"),c(1,0),scale_dimension(scales$x))
    ylim <- ifthenelse(inherits(last_plot(),"ggtern"),c(1,0),scale_dimension(scales$y))
    
    #Weighted or not
    w <- is.numericor(df$weight,1)
    if(is.numeric(w) & length(unique(w)) > 1){
      dens <- safe.call(kde2d.weighted, list(x = df$x, y = df$y, w=w, n = n,lims = c(xlim,ylim), ...))
      if(min(w) != max(w)){
        dens$z <- (dens$z - min(dens$z))/(max(dens$z) - min(dens$z))
        dens$z <- dens$z*diff(range(w)) + min(w)
      }
    }else{
      dens <- safe.call(kde2d, list(x = df$x, y = df$y, n = n,lims = c(xlim,ylim), ...))
    }
    DF <- with(dens, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
    #ggtern
    if(inherits(last_coord,"ternary")){
      
      #create the density sinking function.
      .sink_density <- function(data,remove=TRUE,coord=stop("coord is required")){
        bup <- data
        tryCatch({
          if(inherits(coord,"ternary")){ #ONLY FOR ggtern object
            transfm <- transform_tern_to_cart(data=get_tern_extremes(coord),Tlim=coord$limits$T,Llim=coord$limits$L,Rlim=coord$limits$R)
            inorout <- sp::point.in.polygon(data$x,data$y,transfm$x,transfm$y)
            if(remove)
              data <- data[which(inorout > 0),]
            else
              data[which(inorout <= 0),which(names(data) == "z")] <- 0
          }
        },error=function(e){
          message(e)
          return(bup)
        })
        data
      }
      #Sink the densities
      remove <- !identical(geometry,"polygon")
      DF <- .sink_density(data=DF,remove=remove,coord=last_coord)
    }
    DF$group <- data$group[1]
    
    if(contour) {
      ret <- StatContour$calculate(DF, scales,...)     
    } else {
      names(DF) <- c("x", "y", "density", "group")
      DF$level <- 1
      DF$piece <- 1
      ret <- DF
    }
    
    #UNDO (will be redone later with standard transformation routine)
    if(inherits(last_coord,"ternary")){
      Tlim = last_coord$limits$T
      Rlim = last_coord$limits$R
      Llim = last_coord$limits$L
      ret[,c(last_coord$T,last_coord$L,last_coord$R)] <- transform_cart_to_tern(data=ret,Tlim=Tlim,Llim=Llim,Rlim=Rlim)
    }
    
    return(ret)
  }  
})