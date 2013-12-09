
#' Ternary Coordinate System
#' 
#' \code{coord_tern} is a function which creates a transformation mechanism between the ternary system, and, the cartesian system.
#' It inherits from the fixed coordinate system, employing fixed ratio between x and y axes once transformed.
#' @param T the Top Mapping (default x)
#' @param L the Left Mapping (default y)
#' @param R the Right Mapping (default z)
#' @param xlim the range of x in the cartesian space
#' @param ylim the range of y in the cartesian space
#' @param Tlim the range of T in the ternary space
#' @param Llim the range of L in the ternary space
#' @param Rlim the range of R in the ternary space
#' @return ternary coordinate system object.
coord_tern <- function(T = "x",L="y",R="z",xlim=c(0,1),ylim=c(0,1),Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1)) {
  
  ##Validate x and y lims...
  xlim <- ifthenelse(!is.numeric(xlim),c(0,1),xlim)
  ylim <- ifthenelse(!is.numeric(ylim),c(0,1),ylim)
  xlim <- sort(xlim); ylim <- sort(ylim);
  if(diff(xlim) != diff(ylim)){
    warning("Error in xlim and ylim ratios, adjusting ymax to maintain aspect.",call.=FALSE)
    ylim <- c(min(ylim),min(ylim) + diff(xlim))
  }
  ylim <- c(min(ylim),min(ylim) + diff(ylim)*coord_aspect.ternary())
  
  
  all.coords <- c("x","y","z")
  if(length(which(!c(T,L,R) %in% all.coords)) > 0){stop("Options for T, L and R are x,y and z")}
  if(length(unique(c(T,L,R))) != 3){stop("x, y and z must be assigned to T, L and R in some order and NOT duplicated")}
  
  T <- match.arg(T, all.coords)
  L <- match.arg(L, all.coords[which(!all.coords %in% c(T  ))]) #T is picked
  R <- match.arg(R, all.coords[which(!all.coords %in% c(T,L))]) #T & L are picked
  
  coord(
    T = T, 
    L = L,
    R = R,
    required_aes=c("x","y","z"),
    limits = list(x = xlim, 
                  y = ylim,
                  T = Tlim,
                  L = Llim,
                  R = Rlim),
    subclass = c("ternary","fixed")
  )
}

#' @S3method is.linear cartesian
is.linear.ternary <- function(coord) TRUE

#' @S3method rename_data ternary
rename_data.ternary <- function(coord,data){
  bup <- data
  tryCatch({
    to   <- c("T","L","R"); 
    frm  <- c(coord$T,coord$L,coord$R)
    if(length(which(!frm %in% names(data))) == 0){
      names(to) <- frm
      data <- rename(data,to,warn_missing=FALSE)
    }
  },error=function(e){
    return(bup)
  })
  data
}

#' @S3method coord_range ternary
coord_range.ternary <- function(coord, scales){}

#' @S3method coord_transform ternary
coord_transform.ternary <- function(coord, data, details, verbose=F,revertToCart=T,adjustCart=T){
  bup    <- data #Original Data Backup.
  tryCatch({
    ggplot2:::check_required_aesthetics(coord$required_aes, names(data),"coord_tern")
    data   <- rename_data.ternary(coord, data)
    ix.tern <- c("T","L","R"); 
    ix.cart <- c("x","y")
    ##Execute the transformation to cartesian
    data[,c("x","y")] <- transform_tern_to_cart(
      data = data[,ix.tern],
      Tlim = coord$limits$T,
      Llim = coord$limits$L,
      Rlim = coord$limits$R)[,c("x","y")]
    #only keep records in poly
    if(getOption("tern.discard.external")){
      #Get the extremes to determine if points are outside the plot area.
      data.extremes <-transform_tern_to_cart(data = get_tern_extremes(coord)[,ix.tern],Tlim = coord$limits$T,Llim = coord$limits$L,Rlim = coord$limits$R)[,c("x","y")]
      data[,c("x","y")] <- round(data[,c("x","y")],3)
      in.poly <- point.in.polygon(data$x,data$y,as.numeric(data.extremes$x),as.numeric(data.extremes$y))
      data <- data[which(in.poly > 0),]
    }
  },error=function(e){
    if(!revertToCart){
      stop(as.character(e))
    }
    if(verbose){
      writeLines(as.character(e))
    }
    data <- bup
  })
  if(adjustCart){
    ggplot2:::coord_transform.cartesian(coord,data,details)
  }else{
    data
  }
}

#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scale, aesthetic){ggplot2:::expand_default(scale)}

#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales){
  p <- max(last_plot()$theme$ternary.options$padding,0)  #PADDING
  h <- max(last_plot()$theme$ternary.options$hshift, 0)  #hshift
  v <- max(last_plot()$theme$ternary.options$vshift, 0)  #vshift
  ret <- c(ggplot2:::train_cartesian(scales$x, coord$limits$x + c(-p,p) - h, "x"),
           ggplot2:::train_cartesian(scales$y, coord$limits$y + c(-p,p) - v, "y"))[c("x.range","y.range")]
  ret
}

##' @S3method coord_aspect tern
coord_aspect.ternary <- function(coord, details){sin(pi/3)}

#' @S3method coord_distance ternary
coord_distance.ternary <- function(coord,x,y,details){ggplot2:::coord_distance.cartesian(coord,x,y,details)}


##-----------------------------------------------------------------------
## FOR RENDERING COMPONENTS.
#' @S3method coord_render_axis_v ternary
coord_render_axis_v.ternary <- function(coord, details, theme) {
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_axis_h polar
coord_render_axis_h.ternary <- function(coord, details, theme) {
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_fg ternary
coord_render_fg.ternary <- function(coord,details,theme){
  ##NOT USED. RENDERED IN ggtern.build.R
  ggplot2:::zeroGrob()
}
#' @S3method coord_render_bg ternary
coord_render_bg.ternary <- function(coord,details,theme){
  items <- list()
  
  #The limits.
  data.extreme <- data.frame(T=c(1,0,0),L=c(0,1,0),R=c(0,0,1))
  data.extreme <- transform_tern_to_cart(data=data.extreme)
  data.extreme <- ggplot2:::coord_transform.cartesian(coord,data.extreme,details)
  
  #--------------------------------------------------
  #BACKGROUND...
  data.background <- data.extreme
  data.background$id =  1
  
  ##Function to create new axis grob
  .renderA <- function(name,items){
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour   <- e$colour
      fill     <- e$fill
      size     <- ifthenelse(!is.numeric(e$size),0,e$size)
      linetype <- e$linetype
      alpha    <- ifthenelse(!is.numeric(e$alpha),1,e$alpha)
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
  items <- .renderA("panel.background.tern",items)
  
  #--------------------------------------------------
  #BORDER
  data.border <- data.extreme
  ##Function to create new axis grob
  .renderB <- function(name,s,f,items){
    tryCatch({
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
  items <- .renderB("axis.tern.line.T",3,1,items)
  items <- .renderB("axis.tern.line.L",1,2,items)
  items <- .renderB("axis.tern.line.R",2,3,items)
  
  #--------------------------------------------------
  #ARROWS
  plot <- last_plot()
  if(1==0){
    ##Manual override
  }else if(!inherits(plot,"ggtern")){
    ##PYPASS IF NOT ggtern
  }else if(!is.logical(plot$theme$ternary.options$showarrows)){
    ##BYPASS IF NULL
  }else if(plot$theme$ternary.options$showarrows){
    #get the extermes
    #D <- get_tern_extremes(coord)
    
    #The basic data.
    d.f <- data.extreme[c(1,2,3),]
    d.s <- data.extreme[c(3,1,2),]; rownames(d.s) <- rownames(d.f) #Correct rownames
    d.diff <- d.f - d.s
    
    #Cut down to relative proportion.
    e <- calc_element_plot("ternary.options",theme=theme,verbose=F,plot=NULL)
    d.f <- d.f - (1-max(min(e$arrowfinish,1.0),0.0))*d.diff
    d.s <- d.s +   (min(max(e$arrowstart, 0.0),1.0))*d.diff
    
    ##TRANSFORM
    #options("tern.discard.external"=FALSE)
      d <- rbind(d.s,d.f)
      #d <- transform_tern_to_cart(data=d)
      #d <- ggplot2:::coord_transform.cartesian(coord,d,details)
    #options("tern.discard.external"=TRUE)
    
    ix <- which(colnames(d) %in% c("x","y"))
    d <- cbind(d[1:3,ix],
               d[4:6,ix]);
    colnames(d) <- c("x","y","xend","yend")
    rownames(d) <- c("AT.T","AT.L","AT.R")
    
    #MOVE the Arrows Off the Axes.
    d[c("AT.T","AT.L","AT.R"),"angle"] <- c(0,120,240)
    d[c("AT.T","AT.L","AT.R"),"arrowsep"] <- ifthenelse(is.numeric(e$arrowsep),e$arrowsep[1],0)
    d[,c("x","xend")] <- d[,c("x","xend")] + cos(pi*d$angle/180)*d$arrowsep
    d[,c("y","yend")] <- d[,c("y","yend")] + sin(pi*d$angle/180)*d$arrowsep
    
    #Centerpoints
    d$xmn   <- rowMeans(d[,c("x","xend")])
    d$ymn   <- rowMeans(d[,c("y","yend")])
    
    LABELS  <- ternlabs(plot)
    
    #Labels for arrowmarks
    d$L     <- c(LABELS$T,LABELS$L,LABELS$R)
    
    #Arrowmarks Suffix.
    d$W     <- LABELS$W
    
    ##Function to create new axis grob
    .render.arrow <- function(name,ix,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
        e         <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
  }
  
  #--------------------------------------------------
  #LABELS
  if(!inherits(plot,"ggtern")){
    #BYPASS
  }else{
    d = data.extreme
    
    #Modify the labels accordingly....
    LABELS  <- ternlabs(plot)
    
    d$L  <- as.character(c(LABELS$T,LABELS$L,LABELS$R))
    
    ##Function to create new axis grob
    .render <- function(name,ix,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
        colour    <- e$colour
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
  }
  
  #--------------------------------------------------
  #GRIDS
  ##ASSEMBLE THE DATA FOR THE TICKS.
  if(!inherits(plot,"ggtern")){
    #BYPASS
  }else{
    .vett.tl     <- function(x){if(!is.numeric(x)){x <- 0 } else{x <- max(x[1],0)}}
    .vett.labels <- function(scale){ x = scale$labels; y = scale$breaks; ifthenelse(identical(x,waiver()),100*y,x)}
    .scale_X     <- function(X,plot){
      X <- X[1]
      if(!X %in% c("T","L","R")){stop("X must be either 'T', 'L' or 'R'")}
      if(!inherits(plot,"ggtern")){stop("plot must be of class 'ggtern'")}
      ret = plot$scales$get_scales(X); 
      ifthenelse(!identical(ret,NULL),ret,do.call(paste0("scale_",X,"_continuous"),args=list()))
    }
    
    tl.major <- .vett.tl(calc_element_plot("ternary.options",theme=theme)$ticklength.major)
    tl.minor <- .vett.tl(calc_element_plot("ternary.options",theme=theme)$ticklength.minor)
    
    #THE TIPS OF THE TERNARY PLOT AREA
    #d.extremes <- get_tern_extremes(coord)
    d.extremes <- data.extreme; rownames(d.extremes) <- c("AT.T","AT.L","AT.R")
    
    #ASSEMBLE THE GRID DATA.
    .getData <- function(X,existing=NULL,major=TRUE,angle=0){
      S <- .scale_X(X,plot)
      breaks <- if(major){S$breaks}else{S$minor_breaks}
      if(length(breaks) == 0)return(existing) #BYPASS
      
      id <- max(existing$ID,0) + 1
      
      labels <- if(major){.vett.labels(S)}else{""}
      tryCatch({
        limits <- as.numeric(coord$limits[[X]]) 
      },error=function(e){
        limits <- c(0,1)
      })
      if(!is.numeric(limits)){limits=c(0,1)}
      new <- data.frame(ID = id,Scale=X,Breaks=breaks,Labels=labels,Major=major)
      MAX <- max(limits); 
      MIN <- min(limits)
      
      new <- new[which(new$Breaks > 1.001*MIN & new$Breaks <= MAX),]
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
      out       <- c("x","y")
      
      #FOR TICKS
      ix.s <- which(ix.order == X); 
      ix.f <- if(ix.s == 1){3}else{ix.s-1}
      finish <- as.numeric(d.extremes[ix.at[ix.s],])
      start  <- as.numeric(d.extremes[ix.at[ix.f],])
      for(i in 1:length(out)){new[,out[i]] <- new$Prop*(finish[i]-start[i]) + start[i]}
      
      #FOR GRID
      ix.s <- which(ix.order == X); 
      ix.f <- if(ix.s == 3){1}else{ix.s+1}
      finish <- as.numeric(d.extremes[ix.at[ix.s],])
      start  <- as.numeric(d.extremes[ix.at[ix.f],])
      for(i in 1:length(out)){new[,paste0(out[i],"end.grid")] <- new$Prop*(finish[i]-start[i]) + start[i]}
      
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
    
    ##Determine the tick finish positions for segments.
    d$xend <- cos(d$Angle*pi/180)*d$TickLength + d$x
    d$yend <- sin(d$Angle*pi/180)*d$TickLength + d$y
    
    #FUNCTION TO RENDER TICKS AND LABELS
    .render.ticks <- function(name,items,d){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
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
    for(n in unique(d$NameTicks)){items <- .render.ticks( name=n,items=items,d=d[which(d$NameTicks == n),])}
    for(n in unique(d$NameText)){ items <- .render.labels(name=n,items=items,d=d[which(d$NameText  == n),])}
    for(n in unique(d$NameGrid)){ items <- .render.grid(  name=n,items=items,d=d[which(d$NameGrid  == n),])}
  }
  
  #render.
  ggplot2:::ggname("background",gTree(children = do.call("gList", items)))
}





