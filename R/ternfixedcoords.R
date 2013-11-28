ternfixedcoords <- function(plot){
  PADDING <- max(plot$theme$ternary.options$padding, 0)
  ARROWSEP<- max(plot$theme$ternary.options$arrowsep,0) 
  BACKBY <- PADDING + ARROWSEP
  YMAX <- as.numeric(transformTernToCart(1,0,0)[2])
  
  ys <- as.numeric(transformTernToCart(-PADDING,0,   PADDING,scale=F)[2])
  yf <- as.numeric(transformTernToCart( 1+PADDING,0,-PADDING,scale=F)[2])
  xs <- -PADDING
  xf <- 1+PADDING
  coord_fixed(
      ylim=c(ys,yf),
      xlim=c(xs,xf)
  )
  
  #RETURN
  #list( 
    #coord_fixed(xlim = c(min(-BACKBY,0),max(1+BACKBY,1.0)), 
    #            ylim = c(min(-BACKBY,0),max(YMAX,YMAX+BACKBY)))#,
        #scale_x_continuous(expand=c(0,0)),
        #scale_y_continuous(expand=c(0,0))
  #)
}