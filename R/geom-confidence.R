#' Confidence Interval
#' 
#' Calculates the confidence intervals, via the Mahalnobis Distance and use of the \code{\link[=logratio]{Log-Ratio Transformation}}.
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_density2d
#' @param breaks the confidence intervals, default to 50, 90 and 95 percent.
#' @param n the numer of points in the approximation for each confidence interval.
#' @rdname geomconfidence
#' @examples
#' \donttest{
#'   data(Feldspar)
#'   ggtern(data=Feldspar,aes(An,Ab,Or)) + geom_point() + geom_confidence()
#' }
#' @export
geom_confidence <- function (mapping = NULL, data = NULL,breaks=c(0.50,0.90,0.95),n=500,stat = "confidence", position = "identity",na.rm = FALSE, ...) {
  GeomConfidence$new(mapping = mapping, data = data, breaks=breaks,stat = stat, n=n,position = position, na.rm = na.rm, ...)
}

#' @rdname undocumented
GeomConfidence <- proto(Geom, {
  objname <- "confidence"
  draw <- function(.,data,scales,coordinates,arrow = NULL,lineend = "butt",na.rm = FALSE,...){    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_confidence")
    
    #SINK THE POLYGON COMPONENTS OUT OF RANGE>
    data <- cullAndConstrain(data)
    if (empty(data)) return(.zeroGrob)
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes,"linetype", "size", "shape"),name = "geom_confidence")
    if (empty(data)) return(.zeroGrob)
    GeomPolygon$draw(data=data, scales=scales, coordinates=coordinates, ...)
  }
  default_stat <- function(.) StatConfidence
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour=black, size=0.5, linetype=2,alpha = NA,fill="transparent")
  guide_geom <- function(.) "path"
})

#' @rdname undocumented
StatConfidence <- proto(ggint$Stat, {
  objname <- "confidence"
  calculate_groups <- function(., data, scales, na.rm = FALSE,breaks=c(0.50,0.90,0.95),n=500) {
    data <- remove_missing(data, na.rm, name = "stat_confidence", finite = TRUE)
    
    if(length(breaks) == 0 | !is.numeric(breaks)){return(data.frame())}
    breaks <- sort(breaks,decreasing=T)
    
    #get the last coordinates
    lc <- get_last_coord();
    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,lc$required_aes)))
    check_required_aesthetics(required_aes, names(data),"stat_confidence")
    
    #Ternary is a special case.
    is.tern <- inherits(lc,"ternary")
    
    RESULT <- data.frame()
    ret  <- within(data,by(data,data[,c("PANEL","group")],function(df){
      z  <- ifthenelse(is.tern,isomLR(df[,ifthenelse(lc$clockwise,c(lc$R,lc$L,lc$T),c(lc$L,lc$R,lc$T))]),df[,.$required_aes])
      mu <- colMeans(z)
      cm <- cov(z)
      dat<- mahalanobisDistance(z, mu, cm, whichlines=breaks,m=n)
      grp<- unique(df$group)
      
      TMP<- data.frame()
      for(i in 1:length(breaks)){
        if(is.tern){
          e <- isomLRinv(cbind(dat$mdX[,i], dat$mdY[,i]))
          xp1 <- e[, 2] + e[, 3]/2
          yp1 <- e[, 3] * sqrt(3)/2
        }else{
          xp1 <- dat$mdX[,i]
          yp1 <- dat$mdY[,i]
        }
        TMP <- rbind(TMP, data.frame(x=xp1,y=yp1,group=paste(grp,i,sep="-"),level=breaks[i]))
      }
      TMP$PANEL   = unique(df$PANEL)
      RESULT <<- rbind(RESULT,TMP)
    }))
    
    #Transform RESULT back to ternary
    if(is.tern){
      input <- RESULT[,.$required_aes]
      THS <- input$y/coord_aspect.ternary()
      RHS <- input$x - input$y*tan(pi*30/180)
      LHS <- 1 - THS - RHS      
      
      RESULT$x = THS
      RESULT$y = ifthenelse(lc$clockwise,RHS,LHS)
      RESULT$z = ifthenelse(lc$clockwise,LHS,RHS)
    }
    RESULT
  }
  default_geom <- function(.) GeomConfidence
  default_aes <- function(.) aes(order =..level..)
  required_aes <- c("x", "y")
})

#internal function
#' @rdname undocumented
cullAndConstrain <- function(data){
  lc <- get_last_coord()
  ##Remove data outside plotting area.
  if(inherits(lc,"ternary")){
    TOL <- getOption("tern.pip.tollerance")
    TLIM <- lc$limits$T; LLIM <- lc$limits$L; RLIM <- lc$limits$R
    TTOL <- TOL*diff(TLIM); LTOL <- TOL*diff(LLIM); RTOL <- TOL*diff(RLIM)
    
    #select in values in bound
    data <- data[which(data$x < (max(TLIM)+TTOL) & data$x > (min(TLIM)-TTOL)),]
    data <- data[which(data$y < (max(LLIM)+LTOL) & data$y > (min(LLIM)-LTOL)),]
    data <- data[which(data$z < (max(RLIM)+RTOL) & data$z > (min(RLIM)-RTOL)),]
    
    #For values within the TOLerance, reduce or increase to the hard limit.
    data$x <- pmin(data$x,max(TLIM)) 
    data$x <- pmax(data$x,min(TLIM))
    data$y <- pmin(data$y,max(LLIM)) 
    data$y <- pmax(data$y,min(LLIM))
    data$z <- pmin(data$z,max(RLIM)) 
    data$z <- pmax(data$z,min(RLIM))
  }
  data
}
