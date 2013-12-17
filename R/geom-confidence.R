geom_confidence <- function (mapping = NULL, data = NULL,breaks=c(0.5,0.75,0.95),stat = "confidence", position = "identity",na.rm = FALSE, ...) {
  GeomConfidence$new(mapping = mapping, data = data, breaks=breaks,stat = stat, position = position, na.rm = na.rm, ...)
}

GeomConfidence <- proto(Geom, {
  objname <- "confidence"
  draw <- function(.,data,scales,coordinates,arrow = NULL,lineend = "butt",na.rm = FALSE,...){    
    ##DO THE VARIABLE AESTHETIC CHECK x and y for cartesian, and x,y,z for ternary...
    #required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    #required_aes <- apply(merge(required_aes,c("")),1,function(x){paste(x,collapse="")})
    #check_required_aesthetics(required_aes, names(data),"geom_segment")
    
    ##REMOVE MISSING DATA.
    data <- remove_missing(data, na.rm = na.rm,c(required_aes,"linetype", "size", "shape"),name = "geom_segment")
    if (empty(data)) return(.zeroGrob)
    GeomPolygon$draw(data=data, scales=scales, coordinates=coordinates, ...)
  }
  default_stat <- function(.) StatConfidence
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1,alpha = NA,fill="transparent")
  guide_geom <- function(.) "path"
})

StatConfidence <- proto(ggint$Stat, {
  objname <- "confidence"
  calculate_groups <- function(., data, scales, na.rm = FALSE,breaks=c(0.5,0.75,0.95)) {
    data <- remove_missing(data, na.rm, name = "stat_confidence", finite = TRUE)
    
    if(length(breaks) == 0 | !is.numeric(breaks)){return(data.frame())}
    breaks <- sort(breaks,decreasing=T)
    
    #get the last coordinates
    lc <- get_last_coord();
    if(!inherits(lc,"ternary")){
      warning("confidence geometry applicble for ternary coordinates only.")
      return(data.frame())
    }
    RESULT <- data.frame()
    ret <- within(data,by(data,data[,c("PANEL","group")],function(df){
      z  <- isomLR(df[,c(lc$L,lc$R,lc$T)])
      mu <- colMeans(z)
      cm <- cov(z)
      dat<- mahalanobisDistance(z, mu, cm, whichlines=breaks)
      TMP<- data.frame()
      grp<- unique(df$group)
      
      for(i in 1:length(breaks)){
        e <- isomLRinv(cbind(dat$mdX[,i], dat$mdY[,i]))
        xp1 <- e[, 2] + e[, 3]/2
        yp1 <- e[, 3] * sqrt(3)/2
        TMP <- rbind(TMP,
                     data.frame(x=xp1,y=yp1,group=paste(grp,i,sep="-"),level=breaks[i]))
      }
      #TMP$fill    = NULL
      TMP$PANEL   = unique(df$PANEL)
      RESULT <<- rbind(RESULT,TMP)
    }))
    RESULT
  }
  
  default_geom <- function(.) GeomConfidence
  default_aes <- function(.) aes(order = ..level..)
  required_aes <- c("x", "y", "z")
})
