coord_munch <- function(coord, data, range, segment_length = 0.01,do.transform=T) {
  if (ggplot2:::is.linear(coord)){
    if(do.transform){
      return(ggplot2:::coord_transform(coord, data, range))
    }else{
      return(data)
    }
  }
  
  # range has theta and r values; get corresponding x and y values
  ranges <- ggplot2:::coord_range(coord, range)
  
  # Convert any infinite locations into max/min
  # Only need to work with x and y because for munching, those are the 
  # only position aesthetics that are transformed
  data$x[data$x == -Inf] <- ranges$x[1]
  data$x[data$x == Inf]  <- ranges$x[2]
  data$y[data$y == -Inf] <- ranges$y[1]
  data$y[data$y == Inf]  <- ranges$y[2]
  
  # Calculate distances using coord distance metric
  dist <- ggplot2:::coord_distance(coord, data$x, data$y, range)
  dist[data$group[-1] != data$group[-nrow(data)]] <- NA
  
  # Munch and then transform result
  munched <- ggplot2:::munch_data(data, dist, segment_length)
  if(do.transform){
    ggplot2:::coord_transform(coord, munched, range)
  }else{
    munched
  }
}