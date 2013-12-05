coord_tern <- function(T = "x",L="y",R="z",xlim=c(0,1),ylim=c(0,1),Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1)) {
  all.coords <- c("x","y","z")
  T <- match.arg(T, all.coords)
  L <- match.arg(L, all.coords[which(!all.coords %in% c(T  ))]) #T is picked
  R <- match.arg(R, all.coords[which(!all.coords %in% c(T,L))]) #T & L are picked
  
  cart.coords <- c("x","y")
  x <- match.arg(T,cart.coords)
  y <- match.arg(L,cart.coords[which(!cart.coords %in% c(T  ))]) #T is picked
  
  coord(
    T = T, 
    L = L,
    R = R,
    ratio=1,
    limits = list(x = xlim, 
                  y = ylim,
                  T = Tlim,
                  L = Llim,
                  R = Rlim),
    subclass = c("ternary","fixed")
  )
}

scale_transform.ternary <- function(){writeLines("scale_transform.ternary")}

#' @S3method rename_data ternary
rename_data.ternary <- function(coord,data){
  tryCatch({
    to <- c("T","L","R"); 
    names(to) <- c(coord$T,coord$L,coord$R)
    rename(data,to,warn_missing=FALSE)
  },error=function(e){
    stop(e)
  })
}

#' @S3method coord_transform ternary
coord_transform.ternary <- function(coord, data, details){
  bup    <- data #Original Data Backup.
  data   <- rename_data.ternary(coord, data)
  ix.tern <- c("T","L","R")
  ix.cart <- c("x","y")
  
  #Get the extremes to determine if points are outside the plot area.
  data.extremes <-get_tern_extremes(coord)
  data.extremes <-transform_tern_to_cart(data = data.extremes[,ix.tern],
                                         Tlim = coord$limits$T,
                                         Llim = coord$limits$L,
                                         Rlim = coord$limits$R)[,c("x","y")]
  
  if(length(which(ix.tern %in% colnames(data))) == length(ix.tern)){
    ##Execute the transformation to cartesian
    tmp    <- transform_tern_to_cart(data = data[,ix.tern],
                                     Tlim = coord$limits$T,
                                     Llim = coord$limits$L,
                                     Rlim = coord$limits$R)
    ##and update cartesian.
    data$x <- tmp$x
    data$y <- tmp$y
    
    #only keep records in poly
    if(getOption("tern.discard.external")){
      in.poly <- apply(data[,c("x","y")],1,function(P){point_in_triangle(as.numeric(P),
                                                                       x=as.numeric(data.extremes$x),
                                                                       y=as.numeric(data.extremes$y))})
      data <- data[which(in.poly),]
    }
    
  }else if(length(which(ix.cart %in% colnames(bup))) == length(ix.cart)){
    data <- bup
    writeLines("Ternary plot requires x, y and z aesthetics, however, reverting to cartesian.") 
  }else{
    stop("Neither Ternary or Cartesian Data has been provided.")
  }
  
  data <- ggplot2:::coord_transform.cartesian(coord,data,details)
  data
}

#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scale, aesthetic) {
  ggplot2:::expand_default(scale)
}

#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales){
  ret <- c(ggplot2:::train_cartesian(scales$x, coord$limits$x, "x"),
           ggplot2:::train_cartesian(scales$y, coord$limits$y, "y"))
  ret
}

##' @S3method coord_aspect tern
coord_aspect.ternary <- function(coord, details){sin(pi/3)}

#' @S3method coord_distance ternary
coord_distance.ternary <- function(coord,x,y,details){ ggplot2:::coord_distance.cartesian(coord,x,y,details)}







