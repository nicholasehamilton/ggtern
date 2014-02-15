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
    
    #Remove na.rm values
    data <- remove_missing(data, na.rm, name = "stat_confidence", finite = TRUE)
    
    #Check breaks are valid
    if(length(breaks) == 0 | !is.numeric(breaks))
      return(data.frame())
    
    #Breaks in decreasing order since largest confidence value covers largest area
    breaks <- sort(breaks,decreasing=T)
    
    #get the last coordinates
    lc <- get_last_coord()
    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    required_aes <- sort(unique(c(.$required_aes,lc$required_aes)))
    check_required_aesthetics(required_aes, names(data),"stat_confidence")
    
    #Ternary is a special case.
    is.tern <- inherits(lc,"ternary")
    
    #Empty data set to store results.
    RESULT <- data.frame()
    
    #For each panel and group, do the transformation, appending each calculated set to the RESULT set.
    within(data,by(data,data[,c("PANEL","group")],function(df){
      
      #If ternary conduct isometricLogRatio
      z  <- ifthenelse(is.tern,isomLR(df[,c(lc$L,lc$R,lc$T)]),df[,.$required_aes])
      
      #
      mu <- colMeans(z)
      cm <- cov(z)
      dat<- mahalanobisDistance(z, mu, cm, whichlines=breaks,m=n)
      
      #The panel
      panel <- unique(df$PANEL)
      
      #The main group
      group <- unique(df$group)
      
      #Data frame to store result for each break inside each panel.
      tmp <- data.frame()
      
      #for each break
      for(i in c(1:length(breaks))){
        #The subgroup for index i
        group_i = paste(group,i,sep="-")
        level_i = breaks[i]
        
        #default x and y from mahalanobis distance for break 'i'
        xp1 <- dat$mdX[,i]
        yp1 <- dat$mdY[,i]
        
        #if ternary conduct inverse isometric log ratio
        if(is.tern){
          inv <- isomLRinv(cbind(xp1,yp1))
          xp1 <- inv[, 2] + inv[, 3]/2
          yp1 <- inv[, 3] * sqrt(3)/2
        }
        
        #Create the data including panel and break, append it to existing set.
        tmp <- rbind(tmp,data.frame(x=xp1,y=yp1,group=group_i,level=level_i,PANEL=panel))
      }
      
      #Append
      RESULT <<- rbind(RESULT,tmp)
    }))
    
    #Transform RESULT back to ternary
    if(is.tern)
      RESULT[,as.character(lc[lc$required_axes])] <- transform_cart_to_tern(data=RESULT[,.$required_aes])
    
    #Return the final transformation.
    RESULT
  }
  default_geom <- function(.) GeomConfidence
  default_aes <- function(.) aes(level =..level..)
  required_aes <- c("x", "y")
})
