#' Confidence Interval
#' 
#' Calculates the confidence intervals, via the Mahalnobis Distance and use of the \code{\link[=logratio]{Log-Ratio Transformation}}.
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams ggplot2::geom_density2d
#' @param breaks the confidence intervals, default to 50, 90 and 95 percent.
#' @param n the numer of points in the approximation for each confidence interval.
#' @rdname geomconfidence
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "confidence")}
#' @aliases GeomConfidence StatConfidence
#' @examples
#' \donttest{
#'   data(Feldspar)
#'   ggtern(data=Feldspar,aes(An,Ab,Or)) + geom_point() + geom_confidence()
#' }
#' @export
geom_confidence <- function (mapping = NULL, data = NULL,breaks=c(0.50,0.90,0.95),n=500,stat = "confidence", position = "identity",na.rm = FALSE, ...) {
  GeomConfidence$new(mapping = mapping, data = data, breaks=breaks,stat = stat, n=n,position = position, na.rm = na.rm, ...)
}

GeomConfidence <- proto(Geom, {
  objname <- "confidence"
  draw <- function(.,data,scales,coordinates,arrow = NULL,lineend = "butt",na.rm = FALSE,...){    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_confidence")
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes),name = "geom_confidence")
    if(empty(data)) 
      return(.zeroGrob)
    
    ##Create two grobsets, one for the polygon and the other for the paths.
    polygrob <- .zeroGrob
    pathgrob <- .zeroGrob
    
    #The polygons
    fills   <- unique(data$fill)
    if(length(fills) >0 & length(fills) > length(which(is.na(fills)))){
      #Polygon will be handled slightly different (no colour, only fill), but it is based off the same data.
      data.poly <- data; data.poly$colour <- NA
      polygrob <- GeomPolygon$draw(data=data.poly, scales=scales, coordinates=coordinates, ...)
    }
    
    #The paths
    colours <- unique(data$colour)
    if(length(colours) > 0 & length(colours) > length(which(is.na(colours))))
      pathgrob <- GeomPath$draw(data=data, scales=scales, coordinates=coordinates, ...)
    
    #return the complete grobs, paths on top of polygons.
    gTree(children = gList(polygrob,pathgrob))
  }
  default_stat <- function(.) StatConfidence
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="black", size=0.5,linetype=2,alpha = NA,fill=NA)
  guide_geom <- function(.) "path"
})

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
      z  <- ifthenelse(is.tern,isomLR(df[,c(lc$L,lc$R,lc$T)]),df[,.$required_aes])
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
      
      RESULT[,lc$T] = THS
      RESULT[,lc$L] = LHS
      RESULT[,lc$R] = RHS
    }
    RESULT
  }
  default_geom <- function(.) GeomConfidence
  default_aes <- function(.) aes(level =..level..)
  required_aes <- c("x", "y")
})
