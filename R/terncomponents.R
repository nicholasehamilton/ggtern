#THE DIRECTION ARROWS
ternarrows <- function(plot) {.ternarrows$new(mapping=NULL,data=data.frame(x=NA),geom_params = list(plot=plot),show_guide=FALSE,inherit.aes=FALSE,stat="identity")}
.ternarrows <- proto(ggplot2:::Geom,{
  objname      <- "ternarrows_tern"
  draw_groups  <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    
    if(!is.logical(plot$theme$ternary.options$showarrows)){
      ##BYPASS IF NULL
    }else if(plot$theme$ternary.options$showarrows){
      #get the extermes
      D <- get_tern_extremes(coordinates)
      
      #The basic data.
      d.f <- D[c(1,2,3),]
      d.s <- D[c(3,1,2),]; rownames(d.s) <- rownames(d.f) #Correct rownames
      d.diff <- d.f - d.s
      
      #Cut down to relative proportion.
      e <- calc_element_plot("ternary.options",theme=theme_update(),verbose=F,plot=plot)
      d.f <- d.f - (1-max(min(e$arrowfinish,1.0),0.0))*d.diff
      d.s <- d.s +   (min(max(e$arrowstart, 0.0),1.0))*d.diff
      
      ##TRANSFORM
      d <- coord_transform(coordinates,rbind(d.s,d.f),scales,discard=FALSE); 
      
      ix <- which(colnames(d) %in% c("x","y"))
      d <- cbind(d[1:3,ix],
                 d[4:6,ix]);
      colnames(d) <- c("x","y","xend","yend")
      
      #MOVE the Arrows Off the Axes.
      d[c("AT.T","AT.L","AT.R"),"angle"] <- c(0,120,240)
      d[c("AT.T","AT.L","AT.R"),"arrowsep"] <- ifthenelse(is.numeric(e$arrowsep),e$arrowsep[1],0) #*c(abs(diff(coordinates$limits$R)),
                                                                                                  #  abs(diff(coordinates$limits$L)),
                                                                                                  #  abs(diff(coordinates$limits$R)))
      d[,c("x","xend")] <- d[,c("x","xend")] + cos(pi*d$angle/180)*d$arrowsep
      d[,c("y","yend")] <- d[,c("y","yend")] + sin(pi*d$angle/180)*d$arrowsep
      
      #Centerpoints
      d$xmn   <- rowMeans(d[,c("x","xend")])
      d$ymn   <- rowMeans(d[,c("y","yend")])
      
      #Labels for arrowmarks
      d$L     <- c(plot$labels$T,
                   plot$labels$L,
                   plot$labels$R)
      
      #Arrowmarks Suffix.
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

#THE LABELS
ternlabels <- function (plot) {.ternlabels$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")}
.ternlabels <- proto(ggplot2:::Geom, {
  objname <- "ternlabels_tern"
  #draw_groups <- function(.,...).$draw(...)
  draw <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    #get the data.
    d    <- get_tern_extremes(coordinates)
    d    <- coord_transform(coordinates,d,scales,discard=FALSE)
    
    d$L  <- as.character(c(plot$labels$T, plot$labels$L, plot$labels$R))
    
    ##Function to create new axis grob
    .render <- function(name,ix,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
        colour    <- e$colour
        fill      <- e$fill
        size      <- e$size
        lineheight<- e$lineheight
        family    <- ifthenelse(is.character(e$family),e$family,"sans")
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
ternTicksGridAndAxes <- function (plot) {.ternTicksGridAndAxes$new(mapping=NULL,data=data.frame(x=NA),geom_params=list(plot=plot),inherit.aes=FALSE,show_guide=FALSE,stat="identity")}
.ternTicksGridAndAxes <- proto(ggplot2:::Geom, {
  objname <- "ticks_tern"
  draw_groups <- function(.,...){draw(.,...)}
  draw <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme    #PLOT SPECIFIC
    theme.current <- theme_update()#CURRENT THEME
    
    ##ASSEMBLE THE DATA FOR THE TICKS.
    .vett.tl <- function(x){if(!is.numeric(x)){x <- 0 } else{x <- max(x[1],0)}}
    .vett.labels <- function(scale){ x = scale$labels; y = scale$breaks; ifthenelse(identical(x,waiver()),100*y,x)}
    .scale_X <- function(X,plot){
      X <- X[1]
      if(!X %in% c("T","L","R")){stop("X must be either 'T', 'L' or 'R'")}
      if(!inherits(plot,"ggtern")){stop("plot must be of class 'ggtern'")}
      ret = plot$scales$get_scales(X); 
      ifthenelse(!identical(ret,NULL),ret,do.call(paste0("scale_",X,"_continuous"),args=list()))
    }
    
    tl.major <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.major)
    tl.minor <- .vett.tl(calc_element_plot("ternary.options",theme=theme.plot)$ticklength.minor)
  
    #THE TIPS OF THE TERNARY PLOT AREA
    d.extremes <- get_tern_extremes(coordinates)
    
    #ASSEMBLE THE GRID DATA.
    .getData <- function(X,existing=NULL,major=TRUE,angle=0){
      S <- .scale_X(X,plot)
      breaks <- if(major){S$breaks}else{S$minor_breaks}
      if(length(breaks) == 0)return(existing) #BYPASS
      
      id <- max(existing$ID,0) + 1
      
      labels <- if(major){.vett.labels(S)}else{""}
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
      new$Prop = (new$Breaks - new$Lower) / (new$Upper - new$Lower) #The relative position
      new$TickLength = abs(diff(limits))*if(major){tl.major}else{tl.minor}
      
      #The theme items to call later.
      new$NameText  <- paste0("axis.tern.text.",X)
      new$NameTicks <- paste0("axis.tern.ticks.",if(major){"major"}else{"minor"},".",X)
      new$NameGrid  <- paste0("panel.grid.tern.",if(major){"major"}else{"minor"},".",X)
      
      ##Start and finish positions of scale.
      ix.order  <- c("T","L","R")
      ix.at     <- c("AT.T","AT.L","AT.R")
      
      #FOR TICKS
      ix.s <- which(ix.order == X); ix.f <- if(ix.s == 1){3}else{ix.s-1}
      finish <- as.numeric(d.extremes[ix.at[ix.s],])
      start  <- as.numeric(d.extremes[ix.at[ix.f],])
      for(i in 1:length(ix.order)){new[,ix.order[i]] <- new$Prop*(finish[i]-start[i]) + start[i]}
      
      #FOR GRID
      ix.s <- which(ix.order == X); ix.f <- if(ix.s == 3){1}else{ix.s+1}
      finish <- as.numeric(d.extremes[ix.at[ix.s],])
      start  <- as.numeric(d.extremes[ix.at[ix.f],])
      for(i in 1:length(ix.order)){new[,paste0("grid.",ix.order[i],".end")] <- new$Prop*(finish[i]-start[i]) + start[i]}
      
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
    
    ##Grid data
    d.g  <- d[,c("grid.T.end","grid.L.end","grid.R.end")]; colnames(d.g) <- c("T","L","R"); 
    
    ##Do the coordinate transformation
    d    <- coord_transform(coordinates,d,  scales,discard=FALSE)
    d.g  <- coord_transform(coordinates,d.g,scales,discard=FALSE)[,c("x","y")]; colnames(d.g) <- c("xend.grid","yend.grid");
    d    <- cbind(d,d.g) ##Join
    
    ##Determine the tick finish positions for segments.
    d$xend <- cos(d$Angle*pi/180)*d$TickLength + d$x
    d$yend <- sin(d$Angle*pi/180)*d$TickLength + d$y
    
    #FUNCTION TO RENDER TICKS AND LABELS
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
    .render.labels <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme.current,verbose=F,plot=plot)
        colour    <- e$colour
        fill      <- e$fill
        size      <- e$size
        lineheight<- ifthenelse(is.numeric(e$lineheight),e$lineheight,1)
        family    <- ifthenelse(is.character(e$family),e$family,"sans")
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
    .render.grid <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme.current,verbose=F,plot=plot)
        colour   <- e$colour
        size     <- e$size
        linetype <- e$linetype
        lineend  <- e$lineend
        grob     <- segmentsGrob(
          x0 = d$x, 
          x1 = d$xend.grid,
          y0 = d$y, 
          y1 = d$yend.grid,
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
    
    #PROCESS TICKS AND LABELS
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),])}
    for(n in unique(d$NameText)){ items <- .render.labels(name=n,items=items,d=d[which(d$NameText == n),])}
    for(n in unique(d$NameGrid)){ items <- .render.grid(name=n,items=items,d=d[which(d$NameGrid == n),])}
    
    ##Process
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(color="black",size=0.5,linetype=1,alpha=1)
})



