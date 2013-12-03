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
      range <- range(ggplot2:::scale_transform(scale, limits))
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