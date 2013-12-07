
#' Ternary Coordinate System
#' 
#' \code{coord_tern} is a function which creates a transformation mechanism between the ternary system, and, the cartesian system.
#' It inherits from the fixed coordinate system, employing fixed ratio between x and y axes once transformed.
#' @param T the Top Mapping (default x)
#' @param L the Left Mapping (default y)
#' @param R the Right Mapping (default z)
#' @param xlim the range of x in the cartesian space
#' @param ylim the range of y in the cartesian space
#' @param Tlim the range of T in the ternary space
#' @param Llim the range of L in the ternary space
#' @param Rlim the range of R in the ternary space
#' @return ternary coordinate system object.
coord_tern <- function(T = "x",L="y",R="z",xlim=c(0,1),ylim=c(0,1),Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1)) {
  
  ##Validate x and y lims...
  xlim <- ifthenelse(!is.numeric(xlim),c(0,1),xlim)
  ylim <- ifthenelse(!is.numeric(ylim),c(0,1),ylim)
  xlim <- sort(xlim); ylim <- sort(ylim);
  if(diff(xlim) != diff(ylim)){
    warning("Error in xlim and ylim ratios, adjusting ymax to maintain aspect.",call.=FALSE)
    ylim <- c(min(ylim),min(ylim) + diff(xlim))
  }
  ylim <- c(min(ylim),min(ylim) + diff(ylim)*coord_aspect.ternary())
  
  
  all.coords <- c("x","y","z")
  if(length(which(!c(T,L,R) %in% all.coords)) > 0){stop("Options for T, L and R are x,y and z")}
  if(length(unique(c(T,L,R))) != 3){stop("x, y and z must be assigned to T, L and R in some order and NOT duplicated")}
  
  T <- match.arg(T, all.coords)
  L <- match.arg(L, all.coords[which(!all.coords %in% c(T  ))]) #T is picked
  R <- match.arg(R, all.coords[which(!all.coords %in% c(T,L))]) #T & L are picked
  
  coord(
    T = T, 
    L = L,
    R = R,
    limits = list(x = xlim, 
                  y = ylim,
                  T = Tlim,
                  L = Llim,
                  R = Rlim),
    subclass = c("ternary","fixed")
  )
}


#' @S3method rename_data ternary
rename_data.ternary <- function(coord,data){
  tryCatch({
    to <- c("T","L","R"); 
    names(to) <- c(coord$T,coord$L,coord$R)
    data <- rename(data,to,warn_missing=FALSE)
  },error=function(e){
    stop(e)
  })
  data
}

#' @S3method coord_range ternary
coord_range.ternary <- function(coord, scales){}

#' @S3method coord_transform ternary
coord_transform.ternary <- function(coord, data, details){  
  bup    <- data #Original Data Backup.
  data   <- rename_data.ternary(coord, data)
  ix.tern <- c("T","L","R"); 
  ix.cart <- c("x","y")
  
  if(length(which(ix.tern %in% colnames(data))) == length(ix.tern)){
    ##Execute the transformation to cartesian
    data[,c("x","y")] <- transform_tern_to_cart(data = data[,ix.tern],
                                     Tlim = coord$limits$T,
                                     Llim = coord$limits$L,
                                     Rlim = coord$limits$R)[,c("x","y")]
    
    #only keep records in poly
    if(getOption("tern.discard.external")){
      #Get the extremes to determine if points are outside the plot area.
      data.extremes <-transform_tern_to_cart(data = get_tern_extremes(coord)[,ix.tern],Tlim = coord$limits$T,Llim = coord$limits$L,Rlim = coord$limits$R)[,c("x","y")]
      in.poly <- apply(data[,c("x","y")],1,function(P){point_in_triangle(as.numeric(P),x=as.numeric(data.extremes$x),y=as.numeric(data.extremes$y))})
      data <- data[which(in.poly),]
    }
  }else if(length(which(ix.cart %in% colnames(bup))) == length(ix.cart)){
    data <- bup #writeLines("Ternary plot requires x, y and z aesthetics, however, reverting to cartesian.") 
  }else{
    stop("Neither Ternary or Cartesian Data has been provided.")
  }
  data <- ggplot2:::coord_transform.cartesian(coord,data,details)
  #print(data[1:3,])
  data
}

#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scale, aesthetic){ggplot2:::expand_default(scale)}

#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales){
  ret <- c(ggplot2:::train_cartesian(scales$x, coord$limits$x, "x"),
           ggplot2:::train_cartesian(scales$y, coord$limits$y, "y"))
  ret <- ret[c("x.range","y.range")]
  ret
}

##' @S3method coord_aspect tern
coord_aspect.ternary <- function(coord, details){sin(pi/3)}

#' @S3method coord_distance ternary
coord_distance.ternary <- function(coord,x,y,details){ggplot2:::coord_distance.cartesian(coord,x,y,details)}


##-----------------------------------------------------------------------
## FOR RENDERING COMPONENTS.
#' @S3method coord_render_axis_v ternary
coord_render_axis_v.ternary <- function(coord, details, theme) {
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_axis_h polar
coord_render_axis_h.ternary <- function(coord, details, theme) {
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_fg ternary
coord_render_fg.ternary <- function(coord,details,theme){
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_bg ternary
coord_render_bg.ternary <- function(coord,details,theme){
  items <- list()
  
  #The limits.
  data.extreme <- data.frame(T=c(1,0,0),L=c(0,1,0),R=c(0,0,1))
  data.extreme <- transform_tern_to_cart(data=data.extreme)
  data.extreme <- ggplot2:::coord_transform.cartesian(coord,data.extreme,details)
  
  #--------------------------------------------------
  #BACKGROUND...
  data.background <- data.extreme
  data.background$id =  1
  
  ##Function to create new axis grob
  .render <- function(name,items){
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour   <- e$colour
      fill     <- e$fill
      size     <- ifthenelse(!is.numeric(e$size),0,e$size)
      linetype <- e$linetype
      alpha    <- ifthenelse(!is.numeric(e$alpha),1,e$alpha)
      grob     <- polygonGrob(  data.background$x, 
                                data.background$y, 
                                default.units = "native",
                                id   = data.background$id,
                                gp   = gpar(  col  = colour,
                                              fill = alpha(fill,alpha),
                                              lwd  = size * ggplot2:::.pt,
                                              lty  = linetype
                                )
      )
      
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){print(e)})
    return(items)
  }
  #process the axes
  items <- .render("panel.background.tern",items)
  
  #--------------------------------------------------
  #BORDER
  data.border <- data.extreme
  ##Function to create new axis grob
  .render <- function(name,s,f,items){
    tryCatch({
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour   <- e$colour
      size     <- e$size
      linetype <- e$linetype
      lineend  <- e$lineend
      grob     <- segmentsGrob(
        x0 = data.border$x[s], 
        x1 = data.border$x[f],
        y0 = data.border$y[s], 
        y1 = data.border$y[f],
        default.units="native",
        gp = gpar(col = colour, 
                  lty = linetype,
                  lineend=lineend,
                  lwd = size*ggplot2:::.pt)
      )
      
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){ warning(e)})
    return(items)
  }
  
  #process the axes
  items <- .render("axis.tern.line.T",3,1,items)
  items <- .render("axis.tern.line.L",1,2,items)
  items <- .render("axis.tern.line.R",2,3,items)
  
  #render.
  ggplot2:::ggname("background",gTree(children = do.call("gList", items)))
}





