
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