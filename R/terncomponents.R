##THE TERNARY FIXED COORDINATES
#ternfixedcoords <- function(plot){
#  PADDING <- max(plot$theme$ternary.options$padding, 0)
#  ARROWSEP<- max(plot$theme$ternary.options$arrowsep,0) 
#  BACKBY <- PADDING + ARROWSEP
#  YMAX <- as.numeric(transformTernToCart(1,0,0)[2])
  
#  ys <- as.numeric(transformTernToCart(-PADDING,0,   PADDING,scale=F)[2])
#  yf <- as.numeric(transformTernToCart( 1+PADDING,0,-PADDING,scale=F)[2])
#  xs <- -PADDING
#  xf <- 1+PADDING
  
  #RETURN
#  coord_fixed(
#    ylim=c(ys,yf),
#    xlim=c(xs,xf)
#  )
#}



#THE BACKGROUND
ternbackground <- function(plot) {
  .ternbackground$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")
}
.ternbackground <- proto(ggplot2:::Geom, {
  objname <- "background_tern"
  draw_groups <- function(., data, scales, coordinates, plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
        
    #get the data.
    data.background <- getTernExtremes(coordinates)
    #data.background    <- transformTernToCart(data=data.background)
    data.background    <- coord_transform(coordinates, data.background, scales)
    data.background$id =  1
    
    .ifthenelse <- function(a,b,c){if(a){b}else{c}}
    
    ##Function to create new axis grob
    .render <- function(name,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour   <- e$colour
        fill     <- e$fill
        size     <- .ifthenelse(!is.numeric(e$size),0,e$size)
        linetype <- e$linetype
        alpha    <- .ifthenelse(!is.numeric(e$alpha),1,e$alpha)
        grob     <- polygonGrob(  data.background$x, 
                                  data.background$y, 
                                  default.units = "native",
                                  id   = data.background$id,
                                  gp   = gpar(  col  = colour,
                                                fill = alpha(fill,alpha),
                                                lwd  = size * ggplot2:::.pt,
                                                lty  = linetype
                                  )
        )
        
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){print(e)})
      return(items)
    }
    
    #process the axes
    items <- .render("panel.background.tern",items)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill="grey90",size=0,colour="transparent",linetype=1)
})

#THE ARROWS
ternarrows <- function(plot) {
  .ternarrows$new(mapping=NULL,data=data.frame(x=NA),geom_params = list(plot=plot),show_guide=FALSE,inherit.aes=FALSE,stat="identity")
}
.ternarrows <- proto(ggplot2:::Geom,{
  objname      <- "ternarrows_tern"
  draw_groups  <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    if(!is.logical(plot$theme$ternary.options$showarrows)){
      ##BYPASS IF NULL
    }else if(plot$theme$ternary.options$showarrows){
      .ifthenelse <- function(i,y,n){if(i){y}else{n}}
      e <- calc_element_plot("ternary.options",theme=theme_update(),verbose=F,plot=plot)
      
      #get the extermes
      D <- getTernExtremes(coordinates)
      
      ##GET THE DATA.
      b <-.ifthenelse(is.numeric(e$arrowsep),e$arrowsep[1],0)
      
      f.T <- abs(diff(coordinates$limits$T))*b; if(!is.numeric(f.T)){f.T = b}
      f.L <- abs(diff(coordinates$limits$L))*b; if(!is.numeric(f.L)){f.L = b}
      f.R <- abs(diff(coordinates$limits$R))*b; if(!is.numeric(f.R)){f.R = b}
      
      #START
      d.s <- data.frame(T=c(0   , +f.T, -f.T), 
                        L=c(-f.L,  0  , +f.L), 
                        R=c(+f.R, -f.R,  0     )) + D[c(3,1,2),]
      #FINISH
      d.f <- data.frame(T=c(0   ,  +f.T, -f.T),   
                        L=c(-f.L,   0  , +f.L),   
                        R=c(+f.R,  -f.R,  0    )) + D[c(1,2,3),]
      
      #Cut down to relative proportion.
      s <- min(max(e$arrowstart, 0.0),1.0)
      f <- max(min(e$arrowfinish,1.0),0.0)
      d.diff <- d.f - d.s
      d.f <- d.f - (1-f)*d.diff
      d.s <- d.s +   (s)*d.diff
      
      
      #d.s <- data.frame(T=c(s,1-s-b,+b),
      #                  L=c(b,s,1-s-b),
      #                  R=c(1-s-b,+b,s))
      #FINISH
      #d.f <- data.frame(T=c(f,1-f-b,b), 
      #                  L=c(b,f,1-f-b),
      #                  R=c(1-f-b,b,f))
      
      ##TRANSFORM
      d <- coord_transform(coordinates,rbind(d.s,d.f),scales); 
      
      ix <- which(colnames(d) %in% c("x","y"))
      d <- cbind(d[1:3,ix],d[4:6,ix]);
      colnames(d) <- c("x","y","xend","yend")
      
      d$xmn   <- rowMeans(d[,c("x","xend")])
      d$ymn   <- rowMeans(d[,c("y","yend")])
      d$L     <- c(plot$labels$T,
                   plot$labels$L,
                   plot$labels$R)
      d$W     <- plot$labels$W
      
      ##Function to create new axis grob
      .render.arrow <- function(name,ix,items){
        tryCatch({  
          e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
          colour   <- e$colour
          size     <- e$size
          linetype <- e$linetype
          lineend  <- e$lineend
          grob     <- segmentsGrob(
            x0 = d$x[ix], 
            x1 = d$xend[ix],
            y0 = d$y[ix], 
            y1 = d$yend[ix],
            default.units="native",
            arrow=lineend,
            gp = gpar(col    = colour, 
                      lty    = linetype,
                      lineend="butt",
                      lwd    = size*ggplot2:::.pt)
          )
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        },error = function(e){ warning(e)})
        return(items)
      }
      
      ##Function to create new axis grob
      .render.label <- function(name,ix,items){
        tryCatch({  
          e         <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
          colour    <- e$colour
          size      <- e$size
          lineheight<- e$lineheight
          family    <- e$family
          face      <- e$face
          hjust     <- e$hjust
          vjust     <- e$vjust
          angle     <- e$angle
          grob      <- textGrob( label = arrow.label.formatter(d$L[ix],d$W[ix]), 
                                 x     = d$xmn[ix], 
                                 y     = d$ymn[ix], 
                                 hjust = hjust, 
                                 vjust = vjust, 
                                 rot   = angle, 
                                 default.units="native", 
                                 gp   = gpar(col        = colour, 
                                             fontsize   = size * ggplot2:::.pt,
                                             fontfamily = family, 
                                             fontface   = face, 
                                             lineheight = lineheight))
          
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        },error = function(e){})
        return(items)
      }
      
      #process the axes
      items <- .render.arrow("axis.tern.arrow.T",1,items)
      items <- .render.arrow("axis.tern.arrow.L",2,items)
      items <- .render.arrow("axis.tern.arrow.R",3,items)
      #MARKERS
      items <- .render.label("axis.tern.arrow.text.T",1,items)
      items <- .render.label("axis.tern.arrow.text.L",2,items)
      items <- .render.label("axis.tern.arrow.text.R",3,items)
      
      #render.
      gTree(children = do.call("gList", items))
    }
  }
  default_stat <- function(.) StatIdentity
  default_aes  <- function(.) aes(colour = "black", size = 0.5, linetype = 1, alpha = 1)
})

#THE BORDER
ternborder <- function(plot) {
  .ternborder$new(mapping=NULL,data=data.frame(x=NA),geom_params = list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")
}
.ternborder <- proto(ggplot2:::Geom,{
  objname <- "border_tern"
  draw_groups <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #get the data.
    data.border <- getTernExtremes(coordinates)
    #data.border <- transformTernToCart(data=data.border)
    data.border <- coord_transform(coordinates, data.border, scales)
    
    ##Function to create new axis grob
    .render <- function(name,s,f,items){
      tryCatch({
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour   <- e$colour
        size     <- e$size
        linetype <- e$linetype
        lineend  <- e$lineend
        grob     <- segmentsGrob(
          x0 = data.border$x[s], 
          x1 = data.border$x[f],
          y0 = data.border$y[s], 
          y1 = data.border$y[f],
          default.units="native",
          gp = gpar(col = colour, 
                    lty = linetype,
                    lineend=lineend,
                    lwd = size*ggplot2:::.pt)
        )
        
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    #process the axes
    items <- .render("axis.tern.line.T",3,1,items)
    items <- .render("axis.tern.line.L",1,2,items)
    items <- .render("axis.tern.line.R",2,3,items)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size = 0.5, linetype = 1, alpha = 1)
  #guide_geom <- function(.) "none"
})

#THE GRIDS
terngridlines <- function (plot) {
  .terngridlines$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")
}
.terngridlines <- proto(ggplot2:::Geom, {
  objname <- "grid_tern"
  draw_groups <- function(.,data,scales,coordinates,plot,...){
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    scale_X = function(scales,X){
      ret = scales$get_scales(X); 
      if(!identical(ret,NULL)){
        ret
      }else{
        do.call(paste0("scale_",X,"_continuous"),args=list())
      }
    }
    scale_T <- scale_X(plot$scales,"T"); 
    scale_L <- scale_X(plot$scales,"L"); 
    scale_R <- scale_X(plot$scales,"R")
    
    ##COORDINATES OF MAJOR SEGMENTS
    d.major.T <- scale_T$breaks; 
    d.major.L <- scale_L$breaks; 
    d.major.R <- scale_R$breaks
    
    ##COORDINATES OF MINOR SEGMENTS.
    d.minor.L <- scale_L$minor_breaks; 
    d.minor.T <- scale_T$minor_breaks; 
    d.minor.R <- scale_R$minor_breaks
    
    #Strip minors which are occupied by majors.
    d.minor.T <- d.minor.T[which(!d.minor.T %in% d.major.T)]
    d.minor.L <- d.minor.L[which(!d.minor.L %in% d.major.L)]
    d.minor.R <- d.minor.R[which(!d.minor.R %in% d.major.R)]
    
    #FUNCTION TO RENDER.
    .render.grid <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour   <- e$colour
        size     <- e$size
        linetype <- e$linetype
        lineend  <- e$lineend
        grob     <- segmentsGrob(
          x0 = d$x, 
          x1 = d$xend,
          y0 = d$y, 
          y1 = d$yend,
          default.units="native",
          gp = gpar(col     = colour, 
                    lty     = linetype,
                    lineend = lineend,
                    lwd     = size*ggplot2:::.pt)
        )
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    #HELPER
    .process.set <- function(name,set,index,items,scale=NULL){
      limits <- scale$limits; if(!is.numeric(limits)){limits <- c(0,1)}
      set <- set[which(set > min(limits) & set <= max(limits))]
      if(length(set) > 0){
        V <- set
        tryCatch({
          if(index == 1){
            d.s <- data.frame(T=V,L=0,R=1-V)
            d.f <- data.frame(T=V,L=1-V,R=0)
          }else if(index == 2){
            d.s <- data.frame(T=1-V,L=V,R=0)
            d.f <- data.frame(T=0,L=V,R=1-V)
          }else if(index == 3){
            d.s <- data.frame(T=0,L=1-V,R=V)
            d.f <- data.frame(T=1-V,L=0,R=V)
          }else{stop("index out of range")}
          t.1 <- coord_transform(coordinates,d.s,scales)
          t.2 <- coord_transform(coordinates,d.f,scales)
          
          data.final <- data.frame(t.1[,c("x","y")],t.2[,c("x","y")])
          colnames(data.final) <- c("x","y","xend","yend")
          items <- .render.grid(name,items=items,d=data.final)  
          
        },error=function(e){})
      }
      #})
      return(items)
    }
    #MAJOR GRID
    items <- .process.set("panel.grid.tern.major.T",d.major.T,1,items=items,scale=scale_T)
    items <- .process.set("panel.grid.tern.major.L",d.major.L,2,items=items,scale=scale_L)
    items <- .process.set("panel.grid.tern.major.R",d.major.R,3,items=items,scale=scale_R)
    #MINOR GRID
    items <- .process.set("panel.grid.tern.minor.T",d.minor.T,1,items=items,scale=scale_T)
    items <- .process.set("panel.grid.tern.minor.L",d.minor.L,2,items=items,scale=scale_L)
    items <- .process.set("panel.grid.tern.minor.R",d.minor.R,3,items=items,scale=scale_R)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(color="black",size=0.5,linetype=2,alpha=1)
})

#THE LABELS
ternlabels <- function (plot) {
  .ternlabels$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")
}
.ternlabels <- proto(ggplot2:::Geom, {
  objname <- "ternlabels_tern"
  #draw_groups <- function(.,...).$draw(...)
  draw <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    #get the data.
    d    <- getTernExtremes(coordinates)
    #d    <- transformTernToCart(data=d)
    d    <- coord_transform(coordinates,d, scales)
    d$L  <- as.character(c(plot$labels$T,
                           plot$labels$L,
                           plot$labels$R))
    
    .ifthenelse <- function(a,b,c){if(a){b}else{c}}
    
    ##Function to create new axis grob
    .render <- function(name,ix,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour    <- e$colour
        fill      <- e$fill
        size      <- e$size
        lineheight<- e$lineheight
        family    <- validLabel(e$family,"sans")
        face      <- e$face
        hjust     <- e$hjust
        vjust     <- e$vjust
        angle     <- e$angle
        grob      <- textGrob( label = d$L[ix], 
                               x = d$x[ix], 
                               y = d$y[ix], 
                               default.units="native", 
                               hjust=hjust, 
                               vjust=vjust, 
                               rot  =angle, 
                               gp   = gpar(col      = colour, 
                                           fontsize   = size * ggplot2:::.pt,
                                           fontfamily = family, 
                                           fontface   = face, 
                                           lineheight = lineheight))
        
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    #process the axes
    items <- .render("axis.tern.title.T",1,items)
    items <- .render("axis.tern.title.L",2,items)
    items <- .render("axis.tern.title.R",3,items)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(color="black",size=4,alpha=1,family="sans",lineheight=1)
})

#THE TICKS
ternticks <- function (plot) {
  .ternticks$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")
}
.ternticks <- proto(ggplot2:::Geom, {
  objname <- "ticks_tern"
  draw_groups <- function(.,...){draw(.,...)}
  draw <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme    #PLOT SPECIFIC
    theme.current <- theme_update()#CURRENT THEME
    
    ##ASSEMBLE THE DATA FOR THE TICKS.
    .vett.tl <- function(x){if(!is.numeric(x)){x <- 0 } else{x <- max(x[1],0)}}
    tl.major <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.major)
    tl.minor <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.minor)
    
    
    d.extremes <- getTernExtremes(coordinates)
    .getData <- function(X,existing=NULL,major=TRUE,angle=0){
      S <- scale_X(X,plot)
      breaks <- if(major){S$breaks}else{S$minor_breaks}
      if(length(breaks) == 0)return(existing) #BYPASS
      
      id <- max(existing$ID,0) + 1
      
      labels <- if(major){vett.labels(S)}else{""}
      tryCatch({
        limits <- as.numeric(coordinates$limits[[X]]) 
      },error=function(e){
        limits <- c(0,1)
      })
      if(!is.numeric(limits)){limits=c(0,1)}
      new <- data.frame(ID = id,Scale=X,Breaks=breaks,Labels=labels,Major=major)
      MAX <- max(limits); MIN <- min(limits)
      new <- new[which(new$Breaks > MIN & new$Breaks <= MAX),]
      new$Lower=MIN
      new$Upper=MAX
      new$Prop = (new$Breaks - new$Lower) / (new$Upper - new$Lower)
      new$TickLength = abs(diff(limits))*if(major){tl.major}else{tl.minor}
      
      #The theme items to call later.
      new$NameTicks <- paste0("axis.tern.ticks.",if(major){"major"}else{"minor"},".",X)
      new$NameText  <- paste0("axis.tern.text.",X)
      
      ##Start and finish positions of scale.
      ix.order  <- c("T","L","R")
      finish <- which(ix.order == X)
      start  <- if(finish == 1){3}else{finish-1}
      finish <- d.extremes[,ix.order[finish]]
      start  <- d.extremes[,ix.order[start]]
      new$Scale.TS <- start[1]; new$Scale.TF <- finish[1]; 
      new$Scale.LS <- start[2]; new$Scale.LF <- finish[2]; 
      new$Scale.RS <- start[3]; new$Scale.RF <- finish[3];
      new$T <- new$Prop*(new$Scale.TF-new$Scale.TS) + new$Scale.TS
      new$L <- new$Prop*(new$Scale.LF-new$Scale.LS) + new$Scale.LS
      new$R <- new$Prop*(new$Scale.RF-new$Scale.RS) + new$Scale.RS
      
      #The tick angles.
      new$Angle <- angle
      
      ##ADD TO EXISTING
      rbind(existing,new)
    }    
    
    ##get the base data.
    d <- NULL
    d <- .getData("T",d,T,angle=  0); d <- .getData("T",d,F,angle=  0); 
    d <- .getData("L",d,T,angle=120); d <- .getData("L",d,F,angle=120); 
    d <- .getData("R",d,T,angle=240); d <- .getData("R",d,F,angle=240); 
    
    ##Do the coordinate transformation
    d    <- coord_transform(coordinates,d, scales)
    
    ##Determine the tick finish positions for segments.
    d$xend <- cos(d$Angle*pi/180)*d$TickLength + d$x
    d$yend <- sin(d$Angle*pi/180)*d$TickLength + d$y
    
    #FUNCTION TO RENDER.
    .render.ticks <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme.current,verbose=F,plot=plot)
        colour   <- e$colour
        size     <- e$size
        linetype <- e$linetype
        lineend  <- e$lineend
        grob     <- segmentsGrob(
          x0 = d$x, 
          x1 = d$xend,
          y0 = d$y, 
          y1 = d$yend,
          default.units="native",
          gp = gpar(col     = colour, 
                    lty     = linetype,
                    lineend = lineend,
                    lwd     = size*ggplot2:::.pt)
        )
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),])}
    
    .render.labels <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour    <- e$colour
        fill      <- e$fill
        size      <- e$size
        lineheight<- ifthenelse(is.numeric(e$lineheight),e$lineheight,1)
        family    <- validLabel(e$family,"sans")
        face      <- e$face
        hjust     <- ifthenelse(is.numeric(e$hjust),e$hjust,0)
        vjust     <- ifthenelse(is.numeric(e$vjust),e$vjust,0)
        angle     <- ifthenelse(is.numeric(e$angle),e$angle,0)
        grob      <- textGrob( label = as.character(d$Labels), 
                               x = d$xend, 
                               y = d$yend, 
                               default.units="native", 
                               hjust=hjust, 
                               vjust=vjust, 
                               rot  =angle, 
                               gp   = gpar(col      = colour, 
                                           fontsize   = size * ggplot2:::.pt,
                                           fontfamily = family, 
                                           fontface   = face, 
                                           lineheight = lineheight))
        
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    for(n in unique(d$NameText)){items <- .render.labels(name=n,items=items,d=d[which(d$NameText == n),])}
    
    
    ##Process
    return(gTree(children = do.call("gList", items)))
    
    ###############################################################
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    scale_X = function(X){
      ret = plot$scales$get_scales(X); 
      if(!identical(ret,NULL)){
        ret
      }else{
        do.call(paste0("scale_",X,"_continuous"),args=list())
      }
    }
    scale_T <- scale_X("T"); 
    scale_L <- scale_X("L"); 
    scale_R <- scale_X("R")
    
    ##COORDINATES OF MAJOR SEGMENTS
    d.major.T <- scale_T$breaks; 
    d.major.L <- scale_L$breaks; 
    d.major.R <- scale_R$breaks;
    
    d.major.T.labels <- vett.labels(scale_T)
    d.major.L.labels <- vett.labels(scale_L)
    d.major.R.labels <- vett.labels(scale_R)
    
    ##COORDINATES OF MINOR SEGMENTS.
    d.minor.L <- scale_L$minor_breaks; 
    d.minor.T <- scale_T$minor_breaks; 
    d.minor.R <- scale_R$minor_breaks
    
    #Strip minors which are occupied by majors.
    d.minor.T <- d.minor.T[which(!d.minor.T %in% d.major.T)]
    d.minor.L <- d.minor.L[which(!d.minor.L %in% d.major.L)]
    d.minor.R <- d.minor.R[which(!d.minor.R %in% d.major.R)]
    
    #The Relative Proportions.
    f.T <- abs(diff(coordinates$limits$T)); if(!is.numeric(f.T)){f.T = 1}
    f.L <- abs(diff(coordinates$limits$L)); if(!is.numeric(f.L)){f.L = 1}
    f.R <- abs(diff(coordinates$limits$R)); if(!is.numeric(f.R)){f.R = 1}
    
    #FUNCTION TO RENDER.
    .render.ticks <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour   <- e$colour
        size     <- e$size
        linetype <- e$linetype
        lineend  <- e$lineend
        grob     <- segmentsGrob(
          x0 = d$x, 
          x1 = d$xend,
          y0 = d$y, 
          y1 = d$yend,
          default.units="native",
          gp = gpar(col     = colour, 
                    lty     = linetype,
                    lineend = lineend,
                    lwd     = size*ggplot2:::.pt)
        )
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    #TICKLENGTH
    .vett.tl <- function(x){if(!is.numeric(x)){x <- 0 } else{x <- max(x[1],0)}}
    tl.major <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.major)
    tl.minor <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.minor)
    
    #HELPER
    .process.ticks <- function(name,set,index,items,tl=0,scale=NULL){
      limits = scale$limits; if(!is.numeric(limits)){limits = c(0,1)}
      if(tl > 0){
        MIN <- min(limits)
        MAX <- max(limits)
        set <- set[which(set > MIN & set <= MAX)]
        if(length(set) > 0){ 
          V <- set
          #V <- (set-MIN)/(MAX-MIN)
          tryCatch({
            if(index == 1){
              d.s <- data.frame(T=V,L=0,R=1-V)
                d.f <- d.s; d.f$L <- d.s$L - tl; d.f$R <- d.s$R + tl
            }else if(index == 2){
              d.s <- data.frame(T=1-V,L=V,R=0)
                d.f <- d.s; d.f$T <- d.s$T + tl; d.f$R <- d.s$R - tl
            }else if(index == 3){
              d.s <- data.frame(T=0,L=1-V,R=V)
                d.f <- d.s; d.f$T <- d.s$T -tl; d.f$L <- d.s$L + tl
            }else{stop("index out of range")}
            
            t.s <- coord_transform(coordinates,d.s,scales) 
            t.f <- coord_transform(coordinates,d.f,scales)
            ix <- c("x","y")
            data.final <- data.frame(t.s[,ix],t.f[,ix])
            colnames(data.final) <- c("x","y","xend","yend")
            items <- .render.ticks(name,items=items,d=data.final)            
          },error=function(e){
            #NOTHING.
          })
        }
      }
      return(items)
    }
    
    #MAJOR GRID
    items <- .process.ticks("axis.tern.ticks.major.T",set=d.major.T,index=1,items=items,tl=tl.major*f.T,scale=scale_T)
    items <- .process.ticks("axis.tern.ticks.major.L",set=d.major.L,index=2,items=items,tl=tl.major*f.L,scale=scale_L)
    items <- .process.ticks("axis.tern.ticks.major.R",set=d.major.R,index=3,items=items,tl=tl.major*f.R,scale=scale_R)
    
    items <- .process.ticks("axis.tern.ticks.minor.T",set=d.minor.T,index=1,items=items,tl=tl.minor*f.T,scale=scale_T)
    items <- .process.ticks("axis.tern.ticks.minor.L",set=d.minor.L,index=2,items=items,tl=tl.minor*f.L,scale=scale_L)
    items <- .process.ticks("axis.tern.ticks.minor.R",set=d.minor.R,index=3,items=items,tl=tl.minor*f.R,scale=scale_R)
    
    
    .ifthenelse <- function(x,a,b){if(x){a}else{b}}
    
    #FUNCTION TO RENDER.
    .render.labels <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour    <- e$colour
        fill      <- e$fill
        size      <- e$size
        lineheight<- .ifthenelse(is.numeric(e$lineheight),e$lineheight,1)
        family    <- validLabel(e$family,"sans")
        face      <- e$face
        hjust     <- .ifthenelse(is.numeric(e$hjust),e$hjust,0)
        vjust     <- .ifthenelse(is.numeric(e$vjust),e$vjust,0)
        angle     <- .ifthenelse(is.numeric(e$angle),e$angle,0)
        grob      <- textGrob( label = as.character(d$labels), 
                               x = d$xend, 
                               y = d$yend, 
                               default.units="native", 
                               hjust=hjust, 
                               vjust=vjust, 
                               rot  =angle, 
                               gp   = gpar(col      = colour, 
                                           fontsize   = size * ggplot2:::.pt,
                                           fontfamily = family, 
                                           fontface   = face, 
                                           lineheight = lineheight))
        
        ##Add to the items.
        items[[length(items) + 1]] <- grob
      },error = function(e){ warning(e)})
      return(items)
    }
    
    .process.labels <- function(name,set,index,items,tl=0,labels,scale=NULL){
      limits = scale$limits; if(!is.numeric(limits)){limits = c(0,1)}
      ix <- which(set > min(limits) & set <= max(limits))
      set    <- set[ix]
      labels <- labels[ix]
      if(length(set) > 0){
        V <- set;
        tryCatch({
          if(index == 1){
            d.s <- data.frame(T=V,L=0,R=1-V)
            d.f <- data.frame(T=V,L=-tl,R=1+tl-V)
          }else if(index == 2){
            d.s <- data.frame(T=1-V,L=V,R=0)
            d.f <- data.frame(T=1+tl-V,L=V,R=-tl)
          }else if(index == 3){
            d.s <- data.frame(T=0,L=1-V,R=V)
            d.f <- data.frame(T=-tl,L=1+tl-V,R=V)
          }else{stop("index out of range")}
          t.s <- coord_transform(coordinates,d.s,scales)
          t.f <- coord_transform(coordinates,d.f,scales)
          ix <- c("x","y")
          d <- data.frame(t.s[,ix],t.f[,ix],labels=labels)
          colnames(d) <- c("x","y","xend","yend","labels")
          items <- .render.labels(name,items=items,d=d)
          
        },error=function(e){
          message(e)
          #NOTHING.
        })
      }
      return(items)
    }
    
    #items <- .process.labels("axis.tern.text.T",d.major.T,1,items=items,tl=tl.major*f.T,labels=d.major.T.labels,scale=scale_T)
    #items <- .process.labels("axis.tern.text.L",d.major.L,2,items=items,tl=tl.major*f.L,labels=d.major.L.labels,scale=scale_L)
    #items <- .process.labels("axis.tern.text.R",d.major.R,3,items=items,tl=tl.major*f.R,labels=d.major.R.labels,scale=scale_R)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(color="black",size=0.5,linetype=1,alpha=1)
})



