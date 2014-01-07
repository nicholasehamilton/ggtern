
#' Ternary Coordinate System
#' 
#' \code{coord_tern} is a function which creates a transformation mechanism between the ternary system, and, the cartesian system.
#' It inherits from the fixed coordinate system, employing fixed ratio between x and y axes once transformed.
#' 
#' It is important to note that once the \code{coord_tern()} coordinate system has been applied, the base plot object is no longer strictly a ggplot object, 
#' rather, a ggtern object where several patches have been applied to facilitate correct plotting, including, some limitations on the types of geometries 
#' which can be used. One such essential patch is, for approved geometries previously requiring \code{x} and \code{y} coordinates, now require an additional \code{z} coordinate. 
#' \code{\link[ggtern]{geom_segment}} goes one step further in that it requires both an additional \code{z} and \code{zend} coordinate mappings.
#' @param T the Top Mapping (default x)
#' @param L the Left Mapping (default y)
#' @param R the Right Mapping (default z)
#' @param xlim the range of x in the cartesian space
#' @param ylim the range of y in the cartesian space
#' @param Tlim the range of T in the ternary space
#' @param Llim the range of L in the ternary space
#' @param Rlim the range of R in the ternary space
#' @param clockwise (Depreciated) logical (default \code{FALSE}) indicating whether the precession of axes is clockwise (\code{TRUE}) or counter-clockwise (\code{FALSE}).
#' @return ternary coordinate system object.
#' @export
coord_tern <- function(T = "x",L="y",R="z",xlim=c(0,1),ylim=c(0,1),Tlim=NULL,Llim=NULL,Rlim=NULL,clockwise) {
  if(!missing(clockwise)){
    tern_dep("1.0.1.3","clockwise is now controlled by the theme element 'axis.tern.clockwise'")
  }
  
  ##Validate x and y lims...
  xlim <- sort(is.numericor(ifthenelse(!is.numeric(xlim) & is.numeric(ylim),ylim,xlim),c(0,1)))
  ylim <- sort(is.numericor(ifthenelse(!is.numeric(ylim) & is.numeric(xlim),xlim,ylim),c(0,1)))
  
  ##Put into correct aspect.
  if(diff(xlim) != diff(ylim)){
    warning("Error in xlim and ylim ratios, adjusting ymax to maintain aspect.",call.=FALSE)
    ylim <- c(min(ylim),min(ylim) + diff(xlim))
  }
  ylim <- c(min(ylim), min(ylim) + diff(ylim)*coord_aspect.ternary())
  
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
    limits = list(x = xlim, y = ylim,T = Tlim,L = Llim,R = Rlim),
    required_aes=c("x","y","z"),
    subclass = c("ternary","fixed")
  )
}

#' S3 Method Is Linear
#'
#' @param coord coordinate system
#' @param data input data
#' @param details scales details
#' @param verbose verbose reporting
#' @param revertToCart fall back to cartesian data if error
#' @param adjustCart adjust for the cartesian scale or not
#' @param discard throw away data outside the plotting perimeter
#' @param scales plot scales 
#' @param scale plot scale
#' @param aesthetic mappings
#' @param x data
#' @param y data
#' @param theme net theme
#' @rdname coord
#' @keywords internal
#' @method is.linear ternary
#' @S3method is.linear ternary
is.linear.ternary <- function(coord) TRUE

#' S3 Method Coordinate Transform
#'
#' @rdname coord
#' @method coord_transform ternary
#' @S3method coord_transform ternary
coord_transform.ternary <- function(coord, data, details, verbose=FALSE,revertToCart=TRUE,adjustCart=TRUE,discard=getOption("tern.discard.external")){
  bup    <- data #Original Data Backup.
  tryCatch({
    check_required_aesthetics(coord$required_aes, names(data),"coord_tern")
    data   <- .rename_data_ternary(coord, data)
    ix.tern <- c("T","L","R"); 
    ix.cart <- c("x","y")
    
    ix.T <- "T"
    ix.L <- "L"
    ix.R <- "R"
    
    lim <- list(Tlim=coord$limits[[ix.T]],Llim=coord$limits[[ix.L]],Rlim=coord$limits[[ix.R]])
    
    ##Execute the transformation to cartesian
    data[,c("x","y")] <- transform_tern_to_cart(data = data[,ix.tern],Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,c("x","y")]
    #only keep records in poly
    if(discard){
      #EXPAND THE MAX LIMITS
      TOLLERANCE <- max(is.numericor(getOption("tern.pip.tollerance"),0.01))*max(as.numeric(sapply(lim,function(x)diff(x))))
      
      #Get the extremes (PLUS TOLLERANCE) to determine if points are outside the plot area.
      xtrm <- get_tern_extremes(coord,expand=TOLLERANCE)[,ix.tern]
      
      #Transform extremes to cartesian space
      data.extremes <-transform_tern_to_cart(data = xtrm,Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,c("x","y")]
      
      #In polygon or not.
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
  
  ##Default is to execute the cartesian transformation (DEFAULT)
  if(adjustCart & !missing(details)){
    ggint$coord_transform.cartesian(coord,data,details)
  }else{ ##however sometimes (say in an intermediate step), we may wish to suppress.
    data
  }
}

#' S3 Method Expand Deraults
#'
#' @rdname coord
#' @method coord_expand_defaults ternary
#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scale, aesthetic){
  ret <- ggint$expand_default(scale)
  ret
}

#' S3 Method Coordinate Train
#'
#' @rdname coord
#' @method coord_train ternary
#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales){

  el <- calc_element_plot("ternary.options",theme=theme_update(),verbose=F,plot=last_plot())
  p  <- convertUnit(calc_element_plot("axis.tern.padding",theme=theme_update(),verbose=F,plot=last_plot()),"npc",valueOnly=TRUE)
  h  <- convertUnit(calc_element_plot("axis.tern.hshift", theme=theme_update(),verbose=F,plot=last_plot()),"npc",valueOnly=TRUE)
  v  <- convertUnit(calc_element_plot("axis.tern.vshift", theme=theme_update(),verbose=F,plot=last_plot()),"npc",valueOnly=TRUE)
  
  #trimmed down cartesian coords
  ret <- c(ggint$train_cartesian(scales$x, coord$limits$x + c(-p,p) - h, "x"),
           ggint$train_cartesian(scales$y, coord$limits$y + c(-p,p)*coord_aspect.ternary() - v, "y"))[c("x.range","y.range")]
  #detailed ternary coords
  IX <- c("T","L","R")
  for(ix in IX) #breaks, ticks etc...
    ret <- c(ret,ggint$train_cartesian(scales[[ix]],coord$limits[ix],ix))
  for(ix in IX) #labels
    ret[paste0(ix,"label")] <- scales[[ix]]$name
  ret$Wlabel = scales$W
  ret
}

#' S3 Method Coordinate Aspect
#'
#' @rdname coord
#' @method coord_aspect ternary
##' @S3method coord_aspect ternary
coord_aspect.ternary <- function(coord, details){0.5*tan(60*pi/180)}

#' S3 Method Coordinate Distance
#'
#' @rdname coord
#' @method coord_distance ternary
#' @S3method coord_distance ternary
coord_distance.ternary <- function(coord,x,y,details){
  max_dist <- .dist_euclidean(details$x.range, details$y.range)
  .dist_euclidean(x, y) / max_dist
}


#' S3 Method Render Vertical Axis
#'
#' @rdname coord
#' @method coord_render_axis_v ternary
#' @S3method coord_render_axis_v ternary
coord_render_axis_v.ternary <- function(coord, details, theme) {  
  ##NOT USED. RENDERED IN ggtern.build.R
  .zeroGrob
}

#' S3 Method Render Horizontal Axis
#'
#' @rdname coord
#' @method coord_render_axis_h ternary
#' @S3method coord_render_axis_h ternary
coord_render_axis_h.ternary <- function(coord, details, theme) {
  ##NOT USED. RENDERED IN ggtern.build.R
  .zeroGrob
}

#' S3 Method Render Foreground
#'
#' @rdname coord
#' @method coord_render_fg ternary
#' @S3method coord_render_fg ternary
coord_render_fg.ternary <- function(coord,details,theme){
  #List to hold the grobs.
  #items <- list()
  
  #The limits.
  #data.extreme <- .get.data.extreme(coord,details)
  
  #render.
  #ggint$ggname("background",gTree(children = do.call("gList", items)))
}

#' S3 Method Render Background
#'
#' @rdname coord
#' @method coord_render_bg ternary
#' @S3method coord_render_bg ternary
coord_render_bg.ternary <- function(coord,details,theme){
  #List to hold the grobs.
  items <- list()
  
  #The limits.
  data.extreme <- .get.data.extreme(coord,details)
  
  #Build the plot region.
  items <- .render.background(data.extreme,items,theme)     #BACKGROUND...
  items <- .render.border(data.extreme,items,theme)         #BORDER
  items <- .render.grids(data.extreme,items,theme,details)  #GRIDS
  items <- .render.arrows(data.extreme,items,theme,details) #ARROWS
  items <- .render.titles(data.extreme,items,theme,details) #MAIN TITLES
  
  #render.
  ggint$ggname("background",gTree(children = do.call("gList", items)))
}

#----------------------------------------------------------------------------------
#Internals >>>> Rename ternary data.
#----------------------------------------------------------------------------------
.rename_data_ternary <- function(coord,data){
  bup <- data
  tryCatch({
    to   <- c("T","L","R"); 
    frm  <- c(coord$T,coord$L,coord$R)
    if(length(which(!frm %in% names(data))) == 0){
      names(to) <- frm
      data <- rename(data,to)#,warn_missing=FALSE)
    }
  },error=function(e){
    return(bup)
  })
  data
}

#----------------------------------------------------------------------------------
#Internals >>>> ANGLES
#----------------------------------------------------------------------------------
.get.angles <- function(clockwise){ifthenelse(clockwise,c(-180,-60,60),c(0,120,240))}
.get.angles.arrowmarker <- function(clockwise){ifthenelse(clockwise,c(60,0,-60),c(-60,60,0) )}
.get.angles.text <- function(clockwise){ifthenelse(clockwise,c(0,-60,60),c(0,-60,60))}

#----------------------------------------------------------------------------------
#Internals >>>> Theme flags.
#----------------------------------------------------------------------------------
.theme.get.clockwise <- function(theme){
  clockwise     <- theme$axis.tern.clockwise  
  clockwise     <- ifthenelse(is.logical(clockwise),clockwise[1],getOption("tern.clockwise"))
  clockwise
}
.theme.get.outside    <- function(theme){
  outside       <- calc_element_plot("axis.tern.ticks.outside",theme=theme)
  outside       <- ifthenelse(is.logical(outside),outside[1],getOption("tern.ticks.outside"))
  outside
}
.theme.get.showprimary <- function(theme){
  showprimary   <- calc_element_plot("axis.tern.ticks.showprimary",theme=theme)
  showprimary   <- ifthenelse(is.logical(showprimary), showprimary[1],getOption("tern.ticks.showprimary"))
  showprimary
}
.theme.get.showsecondary <- function(theme){
  showsecondary <- calc_element_plot("axis.tern.ticks.showsecondary",theme=theme)
  showsecondary <- ifthenelse(is.logical(showsecondary),showsecondary[1],getOption("tern.ticks.showsecondary"))
  showsecondary
}

#----------------------------------------------------------------------------------
#Internals >>>> Data Extremes.
#----------------------------------------------------------------------------------
.get.data.extreme <- function(coord,details){
  data.extreme <- transform_tern_to_cart(data = get_tern_extremes(coordinates=coord),
                                         Tlim = coord$limits$T,
                                         Llim = coord$limits$L,
                                         Rlim = coord$limits$R)
  data.extreme <- ggint$coord_transform.cartesian(coord,data.extreme,details)
  rownames(data.extreme) <- c("AT.T","AT.L","AT.R")
  data.extreme
}

#----------------------------------------------------------------------------------
#Internals >>>> Render Components
#----------------------------------------------------------------------------------
.render.background <- function(data.extreme,items,theme){
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
                                              lwd  = size *find_global(".pt"),
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
  items
}
.render.grids <- function(data.extreme,items,theme,details){
  #Process the flags.
  clockwise     <- .theme.get.clockwise(theme)
  outside       <- .theme.get.outside(theme)
  showprimary   <- .theme.get.showprimary(theme)
  showsecondary <- .theme.get.showsecondary(theme)
  shift         <- ifthenelse(!outside,180,0)
  
  #major & minor ticklength
  tl.major <- tl.minor <- 0
  tryCatch({
    tl.major <- convertUnit(theme$axis.tern.ticklength.major,"npc",valueOnly=T)
    tl.minor <- convertUnit(theme$axis.tern.ticklength.minor,"npc",valueOnly=T)
  },error=function(x){
    #handle qietly
  })
  
  #Top, Left Right sequence.
  seq.tlr <- c("T","L","R")
  
  #ASSEMBLE THE GRID DATA.
  .getData <- function(X,ix,existing=NULL,major=TRUE,angle=0,angle.text=0){
    breaks.major <- details[[paste0(X,".major_source")]]
    breaks.minor <- details[[paste0(X,".minor_source")]]
    breaks <- if(major){breaks.major}else{breaks.minor[which(!breaks.minor %in% breaks.major)]}
    
    #BYPASS IF NECESSARY
    if(length(breaks) == 0)return(existing)
    
    labels <- if(major){details[[paste0(X,".labels")]]}else{""}
    labels <- ifthenelse(identical(labels,waiver()),100*breaks,labels)
    
    #Assign new id.
    id <- (max(existing$ID,0) + 1)
    limits <- c(0,1)
    tryCatch({
      limits <- as.numeric(details[[paste0(X,".range")]]);
    },error=function(e){
      #quietly
    })
    limits <- is.numericor(limits,c(0,1))
    b <- limits[length(limits)]; a <- limits[1] #The max/min limits.
    ix <- min(ix,ifthenelse(major,length(tl.major),length(tl.minor)))
    majmin        <- ifthenelse(major,"major","minor")  #Major or Minor Element Name part.
    
    #The new dataframe
    new            <- data.frame(ID = id,Scale=X,Breaks=breaks,Labels=labels,Major=major)
    new            <- new[which(new$Breaks >= min(b,a) & new$Breaks <= max(b,a)),]
    new$Prop       <- (new$Breaks - a) / (b - a) #The relative position
    new$TickLength <- ifthenelse(major,tl.major[ix],tl.minor[ix])
    new$NameText   <- paste0("axis.tern.text.",X)
    new$NameTicks  <- paste0("axis.tern.ticks.",majmin,".",X)
    new$NameGrid   <- paste0("panel.grid.tern.",majmin,".",X)
    
    ##Start and finish positions of scale.
    ix.at     <- paste0("AT.",seq.tlr)
    out       <- c("x","y")
    
    #Start indexes.
    ix.s <- which(seq.tlr == X); 
    
    #FOR TICKS
    ix.f <- ifthenelse(clockwise,if(ix.s == 3){1}else{ix.s+1},if(ix.s == 1){3}else{ix.s-1})
    finish <- as.numeric(data.extreme[ix.at[ix.s],])
    start  <- as.numeric(data.extreme[ix.at[ix.f],])
    for(i in 1:length(out))
      new[,out[i]] <- new$Prop*(finish[i]-start[i]) + start[i]
    
    #FOR GRID
    ix.f <- ifthenelse(clockwise,if(ix.s == 1){3}else{ix.s-1},if(ix.s == 3){1}else{ix.s+1})
    finish <- as.numeric(data.extreme[ix.at[ix.s],])
    start  <- as.numeric(data.extreme[ix.at[ix.f],])
    for(i in 1:length(out))
      new[,paste0(out[i],"end.grid")] <- new$Prop*(finish[i]-start[i]) + start[i]
    
    #The tick angles.
    new$Angle      <- angle
    new$Angle.Text <- angle.text
    
    #Determine the tick finish positions for segments.
    new$xend <- cos(new$Angle*pi/180)*new$TickLength                        + new$x
    new$yend <- sin(new$Angle*pi/180)*new$TickLength/coord_aspect.ternary() + new$y
    
    #Determine the secondary tick start and finish positions.
    new$x.sec    <- new$xend.grid
    new$y.sec    <- new$yend.grid
    new$xend.sec <- cos((new$Angle+180)*pi/180)*new$TickLength                        + new$x.sec
    new$yend.sec <- sin((new$Angle+180)*pi/180)*new$TickLength/coord_aspect.ternary() + new$y.sec
    
    ##ADD TO EXISTING
    rbind(existing,new)
  }
  
  angles      <- .get.angles(clockwise) + shift
  angles.text <- .get.angles.text(clockwise)
  
  ##get the base data.
  d <- NULL
  for(j in 1:2)
    for(i in 1:length(seq.tlr))
      d <- .getData(X=seq.tlr[i],ix=i,existing=d,major = (j==1),angle = angles[i],angle.text = angles.text[i]);
  if(nrow(d) > 1){d <- d[nrow(d):1,]}  #REVERSE (minor under major)
  
  #FUNCTION TO RENDER TICKS AND LABELS
  .render.ticks <- function(name,items,d,primary=TRUE){
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour   <- e$colour
      size     <- e$size
      linetype <- e$linetype
      lineend  <- e$lineend
      grob     <- segmentsGrob(
        x0 = ifthenelse(!primary,d$x.sec,d$x), 
        x1 = ifthenelse(!primary,d$xend.sec,d$xend),
        y0 = ifthenelse(!primary,d$y.sec,d$y), 
        y1 = ifthenelse(!primary,d$yend.sec,d$yend),
        default.units="native",
        gp = gpar(col     = colour, 
                  lty     = linetype,
                  lineend = lineend,
                  lwd     = size*find_global(".pt"))
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
      hjust     <- .hjust.flip(ifthenelse(is.numeric(e$hjust),e$hjust,0),clockwise=clockwise)
      vjust     <- ifthenelse(is.numeric(e$vjust),e$vjust,0)
      angle     <- ifthenelse(is.numeric(e$angle),e$angle,0) + unique(d$Angle.Text)[1]
      grob      <- textGrob( label = as.character(d$Labels), 
                             x = ifthenelse(outside && showprimary,d$xend,d$x), 
                             y = ifthenelse(outside && showprimary,d$yend,d$y), 
                             default.units="native", 
                             hjust=hjust, 
                             vjust=vjust, 
                             rot  =angle, 
                             gp   = gpar(col      = colour, 
                                         fontsize   = size,
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
      if(!identical(e,element_blank())){
        colour   <- e$colour
        size     <- max(e$size,0)
        if(size > 0){
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
                      lwd     = size*find_global(".pt"))
          )
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        }
      }
    },error = function(e){ warning(e)})
    return(items)
  }
  
  #PROCESS TICKS AND LABELS
  for(n in unique(d$NameGrid)){ items <- .render.grid(  name=n,items=items,d=d[which(d$NameGrid  == n),])}
  if(showprimary)
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),],primary=TRUE)}
  if(showsecondary)
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),],primary=FALSE)}
  for(n in unique(d$NameText)){ items <- .render.labels(name=n,items=items,d=d[which(d$NameText  == n),])}
  items
}
.render.border <- function(data.extreme,items,theme){
  clockwise <- .theme.get.clockwise(theme) 
  .renderB  <- function(name,s,f,items){
    tryCatch({
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour   <- e$colour
      size     <- e$size
      linetype <- e$linetype
      lineend  <- e$lineend
      grob <- .zeroGrob
      tryCatch({
        grob     <- segmentsGrob(
          x0 = data.extreme$x[s], 
          x1 = data.extreme$x[f],
          y0 = data.extreme$y[s], 
          y1 = data.extreme$y[f],
          default.units="native",
          gp = gpar(col = colour, 
                    lty = linetype,
                    lineend=lineend,
                    lwd = size*find_global(".pt"))
        )
      },error=function(e){
        #just handle it.
      })
      
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){ warning(e)})
    return(items)
  }
  
  #process the axes
  if(clockwise){
    items <- .renderB("axis.tern.line.T",2,1,items)
    items <- .renderB("axis.tern.line.L",3,2,items)
    items <- .renderB("axis.tern.line.R",1,3,items)
  }else{
    items <- .renderB("axis.tern.line.T",3,1,items)
    items <- .renderB("axis.tern.line.L",1,2,items)
    items <- .renderB("axis.tern.line.R",2,3,items)
  }
  items
}
.render.arrows <- function(data.extreme,items,theme,details){
  axis.tern.showarrows <- theme$axis.tern.showarrows
  if(is.logical(axis.tern.showarrows) && (axis.tern.showarrows)){
    tryCatch({
      clockwise <- .theme.get.clockwise(theme)
      #The basic data.
      d.s <- data.extreme[ifthenelse(clockwise,c(2,3,1),c(3,1,2)),]
      d.f <- data.extreme[c(1,2,3),]
      rownames(d.s) <- rownames(d.f) #Correct rownames
      d.diff        <- d.f - d.s
      
      #arrow start and finish proportions
      arrowstart = theme$axis.tern.arrowstart[1]
      arrowfinish= theme$axis.tern.arrowfinish[1]
      
      #Cut down to relative proportion.
      d.f <- d.f -   (1-max(min(arrowfinish,1.0),0.0))*d.diff
      d.s <- d.s +   (min(max(arrowstart, 0.0),1.0)  )*d.diff
      d <- rbind(d.s,d.f)
      
      ixseq <- c("T","L","R")
      ixrow <- paste0("AT.",ixseq)
      ixcol <- c("x","y","xend","yend")
      ix    <- which(colnames(d) %in% ixcol[c(1:2)])
      d     <- cbind(d[1:3,ix],d[4:6,ix]);
      rownames(d) <- ixrow
      colnames(d) <- ixcol
      
      #The arrow seperation in npc units.
      arrowsep <- calc_element_plot("axis.tern.arrowsep",theme=theme,verbose=F,plot=NULL)
      if(length(arrowsep) != 3 && length(arrowsep) > 1)
        arrowsep <- arrowsep[1]
      arrowsep <- convertUnit(arrowsep,"npc",valueOnly=TRUE)
      
      #MOVE the Arrows Off the Axes.
      d[ixrow,"angle"]    <- .get.angles(clockwise)
      d[ixrow,"arrowsep"] <- arrowsep
      d[,ixcol[c(1,3)]]   <- d[,ixcol[c(1,3)]] + cos(pi*d$angle/180)*arrowsep
      d[,ixcol[c(2,4)]]   <- d[,ixcol[c(2,4)]] + sin(pi*d$angle/180)*arrowsep
      
      #Centerpoints, labels, arrowsuffix
      d$xmn   <- rowMeans(d[,ixcol[c(1,3)]])
      d$ymn   <- rowMeans(d[,ixcol[c(2,4)]])
      d$L     <- as.character(c(details$Tlabel,details$Llabel,details$Rlabel))
      d$W     <- as.character(c(details$Wlabel))
      d$A     <- .get.angles.arrowmarker(clockwise)
      
      ##Function to create new axis & label grob
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
                      lwd    = size*find_global(".pt"))
          )
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        },error = function(e){ warning(e)})
        return(items)
      }
      .render.label <- function(name,ix,items){
        tryCatch({  
          e         <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
          colour    <- e$colour
          size      <- e$size
          lineheight<- e$lineheight
          family    <- e$family
          face      <- e$face
          hjust     <- e$hjust
          vjust     <- ifthenelse(identical(name,"axis.tern.arrow.text.T"),e$vjust,.hjust.flip(e$vjust,clockwise=clockwise))
          angle     <- e$angle
          grob      <- textGrob( label = arrow_label_formatter(d$L[ix],d$W[ix]), 
                                 x     = d$xmn[ix], 
                                 y     = d$ymn[ix], 
                                 hjust = hjust, 
                                 vjust = vjust, 
                                 rot   = angle + d$A[ix], 
                                 default.units="native", 
                                 gp   = gpar(col        = colour, 
                                             fontsize   = size,
                                             fontfamily = family, 
                                             fontface   = face, 
                                             lineheight = lineheight))
          
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        },error = function(e){})
        return(items)
      }
      
      #process the axes
      for(i in 1:length(ixseq)){
        ix    <- ixseq[i]
        items <- .render.arrow(paste0("axis.tern.arrow.",     ix),i,items)#Arrows
        items <- .render.label(paste0("axis.tern.arrow.text.",ix),i,items)#Markers
      }
    },error=function(e){
      #handle quietly
    })
  }
  items
}
.render.titles <- function(data.extreme,items,theme,details){
  clockwise <- .theme.get.clockwise(theme) 
  
  d    <- data.extreme
  d$L  <- as.character(c(details$Tlabel,details$Llabel,details$Rlabel))
  
  ##Function to create new axis grob
  .render <- function(name,ix,items,hshift=0,vshift=0){
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      colour    <- e$colour
      size      <- e$size;
      lineheight<- e$lineheight
      family    <- ifthenelse(is.character(e$family),e$family,"sans")
      face      <- e$face
      hjust     <- e$hjust
      vjust     <- e$vjust
      angle     <- e$angle
      grob      <- textGrob( label = d$L[ix], 
                             x = unit(d$x[ix] + hshift,"npc"), 
                             y = unit(d$y[ix] + vshift,"npc"),
                             hjust=hjust, 
                             vjust=vjust, 
                             rot  =angle,
                             gp   = gpar(col        = colour, 
                                         fontsize   = size,
                                         fontfamily = family, 
                                         fontface   = face, 
                                         lineheight = lineheight))
      #print(convertWidth(widthDetails(grob), 'npc', TRUE))
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){ warning(e)})
    return(items)
  }
  
  #process the axes
  SHIFT <- 0.01
  items <- .render("axis.tern.title.T",1,items,vshift=  SHIFT)
  items <- .render("axis.tern.title.L",2,items,vshift= -SHIFT*tan(pi/6),hshift=-SHIFT)
  items <- .render("axis.tern.title.R",3,items,vshift= -SHIFT*tan(pi/6),hshift= SHIFT)
}






