coord_tern <- function(T = "x",L="y",R="z") {
  all.coords <- c("x","y","z")
  T <- match.arg(T, all.coords)
  L <- match.arg(L, all.coords[which(!all.coords %in% c(T  ))]) #T is picked
  R <- match.arg(R, all.coords[which(!all.coords %in% c(T,L))]) #T & L are picked
  coord(
    T = T, 
    L = L,
    R = R,
    limits=list(x=c(0,1),
                y=c(0,transformTernToCart(1,0,0)[,2])),
    #ratio=1,
    subclass = c("fixed","ternary")
  )
}

scale_transform.ternary <- function(){
  writeLines("scale_transform.ternary")
}

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

rename_data.tern <- function(coord,data){
  to <- c("T","L","R"); names(to) <- c(coord$T,coord$L,coord$R)
  rename(data,to,warn_missing=FALSE)
}

#' @S3method coord_transform polar
coord_transform.ternary <- function(coord, data, details) {
  data   <- rename_data.tern(coord, data)
  tmp    <- transformTernToCart(data=data[,c("T","L","R")])
  data$x <- tmp$x; 
  data$y <- tmp$y
  data
}

#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scale, aesthetic) {
  ggplot2:::expand_default(scale,c(0, 0),c(0, 0))
}

#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales) {
  
  ret <- list(x = list(), 
              y = list(),
              z = list())
  for (n in c("x","y")) {
    
    scale  <- scales[[n]]
    #if(is.null(scale)){
    #  writeLines(paste("Creating",n,"scale."))
    #  scale = do.call(paste0("scale_",n,"_continuous"),args=list())
    #}
    
    limits <- coord$limits[[n]]
    
    if (is.null(limits)) {
      expand <- ggplot2:::coord_expand_defaults(coord, scale, n)
      range  <- ggplot2:::scale_dimension(scale, expand)
    } else {
      range <- range(scale_transform(scale, limits))
    }
    
    out <- ggplot2:::scale_break_info(scale, range)
    ret[[n]]$range  <- out$range
    ret[[n]]$major  <- out$major_source
    ret[[n]]$minor  <- out$minor_source
    ret[[n]]$labels <- out$labels
  }
  
  details <- list(
    x.range = ret$x$range, 
    y.range = ret$y$range,
    z.range = ret$z$range,
    x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
    y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels,
    z.major = ret$z$major, z.minor = ret$z$minor, z.labels = ret$z$labels
  )
  
  ix <- list(); 
  ix[c(coord$T,coord$L,coord$R)] <- c("T","L","R");
  names(details) <- gsub("x\\.", paste0(ix["x"],"."), names(details))
  names(details) <- gsub("y\\.", paste0(ix["y"],"."), names(details))
  names(details) <- gsub("z\\.", paste0(ix["z"],"."), names(details))
    
  details
}

#' @S3method coord_aspect tern
coord_aspect.ternary <- function(coord, details) 1

#' @S3method coord_distance ternary
coord_distance.ternary <- function(coord,x,y,details) {
  #print(coord)
  #print(x)
  #print(y)
  #print(details)
  #ggplot2:::coord_distance.cartesian(coord,x,y,details)
}







