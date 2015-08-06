#' Ternary Coordinate System
#' 
#' \code{coord_tern} is a function which creates a transformation mechanism between the ternary system, and, the cartesian system.
#' It inherits from the fixed coordinate system, employing fixed ratio between x and y axes once transformed.
#' 
#' @section Aesthetics (Required in Each Layer):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("coord", "tern")}
#' 
#' @section Additional Points to Note:
#' It is important to note that once the \code{coord_tern()} coordinate system has been applied, the base plot object is no longer strictly a ggplot object, 
#' rather, a ggtern object where several patches have been applied to facilitate correct plotting.
#' 
#' Abovementioned limitations include the types of geometries which can be used (ie approved geometries), 
#' or modifications to required aesthetic mappings. One such essential patch is, for approved geometries previously 
#' requiring \code{x} and \code{y} coordinates, now require an additional \code{z} coordinate, and, 
#' \code{\link[ggtern]{geom_segment}} goes one step further in that it requires both an additional 
#' \code{z} and \code{zend} coordinate mappings. 
#' 
#' In essence, the required aesthetics are the product between what
#' is required of the 'layer' and what is required of the 'coordinate system'.
#' @param T the Top Mapping (default ['x', 'y' or 'z'] stored in global option \code{'tern.default.T'})
#' @param L the Left Mapping (default ['x', 'y' or 'z'] stored in global option \code{'tern.default.L'})
#' @param R the Right Mapping (default ['x', 'y' or 'z'] stored in global option \code{'tern.default.R'})
#' @param xlim the range of x in the cartesian space
#' @param ylim the range of y in the cartesian space
#' @param Tlim the range of T in the ternary space
#' @param Llim the range of L in the ternary space
#' @param Rlim the range of R in the ternary space
#' @param clockwise DEPRECIATED, replaced by individual theme element, see \code{\link{axis.tern.clockwise}}.
#' @return \code{coord_tern} returns a ternary coordinate system object.
#' @export
coord_tern <- function(T      = getOption("tern.default.T"),
                       L      = getOption("tern.default.L"),
                       R      = getOption("tern.default.R"),
                       xlim   = c(0,1),
                       ylim   = c(0,1),
                       Tlim   = NULL,
                       Llim   = NULL,
                       Rlim   = NULL,
                       clockwise) {
  
  ##Validate x and y lims...
  validateLims <- function(p,s){
    if(length(p) >= 2 & is.numeric(p))return(sort(p[c(1:2)]))
    if(length(2) >= 2 & is.numeric(s))return(sort(s[c(1:2)]))
    c(0,1)
  }
  xlim <- validateLims(xlim,ylim)
  ylim <- validateLims(ylim,xlim)
  
  ##Put into correct aspect ratio.
  if(diff(xlim) != diff(ylim)){ ylim <- mean(ylim) + c(-1,1)*diff(xlim)/2 }
  ylim <- min(ylim) + c(0,1)*diff(xlim)*coord_aspect.ternary()
  
  #Fallback if invalid values provided.
  resolve <- function(t,d){ifthenelse(!is.character(t),d,t[1])}
  T = resolve(T,"x")
  L = resolve(L,"y")
  R = resolve(R,"z")
  
  #Run some checks to ensure valid assignment will transpire between T, L, R and x, y and z.
  all.coords <- c("x","y","z")
  all.axes   <- c("T","L","R")
  if(length(which(!c(T,L,R) %in% all.coords)) > 0)
    stop("Options for T, L and R are x,y and z",call.=FALSE)
  if(length(unique(c(T,L,R))) != 3)
    stop("x, y and z must be assigned to T, L and R in some order and NOT duplicated",call.=FALSE)
  
  #Progressively assign to T, L and R.
  T <- match.arg(T, all.coords)
  L <- match.arg(L, all.coords[which(!all.coords %in% c(T  ))]) #T      is picked, L and R to remain
  R <- match.arg(R, all.coords[which(!all.coords %in% c(T,L))]) #T & L are picked, R       to remain
  
  #return coordinate object of fixed ratio.
  coord(
    T = T, 
    L = L, 
    R = R,
    limits = list(x = xlim, 
                  y = ylim,
                  T = Tlim,
                  L = Llim,
                  R = Rlim),
    #required_aes is now a function of the coordinate system, as well as the geometries.
    required_aes = all.coords, 
    required_axes= all.axes,
    #the class, ternary fixed ratio.
    subclass = c("ternary","fixed")
  )
}

#' S3 Method Is Linear
#'
#' @param x data
#' @param y data
#' @rdname coord
#' @keywords internal
#' @method is.linear ternary
#' @S3method is.linear ternary
is.linear.ternary <- function(coord) TRUE

#' S3 Method Coordinate Transform
#'
#' @param coord coordinate system
#' @param data input data
#' @param details scales details
#' @param verbose verbose reporting
#' @param revertToCart fall back to cartesian data if error
#' @param passToCartesian after conducting the ternary transformation, then execute the standard cartesian transformation.
#' @param discard throw away data outside the plotting perimeter
#' @param dont_transform override the ternary transformation
#' @rdname coord
#' @method coord_transform ternary
#' @S3method coord_transform ternary
coord_transform.ternary <- function(coord, data, details, 
                                    verbose        = FALSE,
                                    revertToCart   = FALSE,
                                    passToCartesian= TRUE,
                                    discard        = getOption("tern.discard.external"),
                                    dont_transform = getOption("tern.dont_transform")){
  #If transformation is enabled
  if(!dont_transform){
    
    #Original Data Backup.
    bup    <- data 
    
    tryCatch({
      check_required_aesthetics(coord$required_aes, names(data),"coord_tern")
      data   <- .rename_data_ternary(coord, data)
      ix.tern <- c("T","L","R"); 
      ix.cart <- c("x","y")
      lim           <- ternLimitsForCoord(coord)
      
      #HACK
      for(ix in ix.tern)
        if(is.null(coord$limits[[ix]]))
          revertToCart = TRUE
      
      ##Execute the transformation to cartesian
      data[,ix.cart] <- transform_tern_to_cart(data = data[,ix.tern],Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,ix.cart]
      
      #Discard records outside the polygon region that defines the plot area.
      if(discard){
        
        #Get the extremes (PLUS TOLLERANCE) to determine if points are outside the plot area.
        xtrm <- get_tern_extremes(coord,expand=expandTern(coord))[,ix.tern]
        
        #Transform extremes to cartesian space
        data.extremes <-transform_tern_to_cart(data = xtrm,Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,ix.cart]
        
        in.poly <- sp::point.in.polygon(data$x,data$y,as.numeric(data.extremes$x),as.numeric(data.extremes$y))
        data   <- subset(data,in.poly > 0)
      }
      
    #Error Handling - Terminate or Revert to 'cartesian'
    },error=function(e){
      msg <- as.character(e)
      if(!revertToCart)
        stop(gsub("Error: ","",msg)) #Terminate
      if(verbose)
        message(msg) #Report Error if verbose
      data <- bup    #Revert
    })
  }
  
  ##Default is to execute the cartesian transformation (DEFAULT)
  if(passToCartesian & !missing(details)){
    ggint$coord_transform.cartesian(coord,data,details)
  }else{ ##however sometimes (say in an intermediate step), we may wish to suppress.
    data
  }
}

#' S3 Method Expand Deraults
#'
#' @param scales plot scales 
#' @param aesthetic mappings
#' @rdname coord
#' @method coord_expand_defaults ternary
#' @S3method coord_expand_defaults ternary
coord_expand_defaults.ternary <- function(coord, scales, aesthetic){
  ggint$expand_default(scales)
}

#' S3 Method Coordinate Train
#'
#' @rdname coord
#' @method coord_train ternary
#' @S3method coord_train ternary
coord_train.ternary <- function(coord, scales){
  #Current theme and last plot.
  theme <- theme_update()
  plot  <- last_plot()
  
  #Shift the plot left/right, up/down
  hshift   <- convertUnit(calc_element_plot("axis.tern.hshift", theme=theme,verbose=FALSE,plot=plot),"npc",valueOnly=TRUE)
  vshift   <- convertUnit(calc_element_plot("axis.tern.vshift", theme=theme,verbose=FALSE,plot=plot),"npc",valueOnly=TRUE)
  
  #build some trimmed down cartesian coords
  ret <- c(ggint$train_cartesian(scales$x,coord$limits$x - hshift, "x"),
           ggint$train_cartesian(scales$y,coord$limits$y - vshift, "y"))[c("x.range","y.range")]
  
  #detailed ternary coords
  IX <- c("T","L","R")
  for(ix in IX){
    scale <- scales[[ix]]
    if(!is.null(scale)){
      tmp <- ggint$train_cartesian(scale,coord$limits[ix],ix)
      ret <- c(ret,tmp) #breaks, ticks etc...
      ret[paste0(ix,"label")] <- scale$name #labels
    }
  }
  
  #Add the 
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
coord_distance.ternary <- function(coord,x,y,details) {
  .dist_euclidean(x, y) / .dist_euclidean(details$x.range, details$y.range)
}


#' S3 Method Render Vertical Axis
#'
#' @param theme net theme
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
  items <- list()
  
  #The limits.
  data.extreme <- .get.data.extreme(coord,details)
  
  #render.
  items <- .render.titles(data.extreme,items,theme,details) #MAIN TITLES
  items <- .render.arrows(data.extreme,items,theme,details) #ARROWS
  
  #render
  ggint$ggname("foreground",gTree(children = do.call("gList", items)))
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
  items <- .render.grids(data.extreme,     items,theme,details)  #GRIDS
  items <- .render.border(data.extreme,    items,theme)         #BORDER
  
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
#Functions to determine the rotation angles for the various components
#----------------------------------------------------------------------------------
.get.angles             <- function(clockwise){ifthenelse(clockwise,c(-180,-60,60),c(0,120,240))}
.get.angles.arrows      <- function(clockwise){.get.angles(clockwise) + ifthenelse(clockwise,-30,30)}
.get.angles.arrowmarker <- function(clockwise){ifthenelse(clockwise,c(60,0,-60),c(-60,60,0) )}
.get.angles.ticklabels  <- function(clockwise){ifthenelse(clockwise,c(0,-60,60),c(0,-60,60))}

#----------------------------------------------------------------------------------
#Internals >>>> Theme flags.
#----------------------------------------------------------------------------------
.theme.get.clockwise <- function(theme){
  clockwise     <- theme$axis.tern.clockwise  
  clockwise     <- ifthenelse(is.logical(clockwise),clockwise[1],getOption("tern.clockwise"))
  clockwise
}
.theme.get.showtitles <- function(theme){
  showtitles    <- calc_element_plot("axis.tern.showtitles",theme=theme)
  showtitles    <- ifthenelse(is.logical(showtitles),showtitles[1],getOption("tern.showtitles"))
  showtitles
}
.theme.get.showlabels <- function(theme){
  showlabels    <- calc_element_plot("axis.tern.showlabels",theme=theme)
  showlabels    <- ifthenelse(is.logical(showlabels),showlabels[1],getOption("tern.showlabels"))
  showlabels
}
.theme.get.showgrid.major <- function(theme){
  showgrid <- calc_element_plot("axis.tern.showgrid.major",theme=theme)
  showgrid <- ifthenelse(is.logical(showgrid),showgrid[1],getOption("tern.showgrid.major"))
  showgrid
}
.theme.get.showgrid.minor <- function(theme){
  showgrid <- calc_element_plot("axis.tern.showgrid.minor",theme=theme)
  showgrid <- ifthenelse(is.logical(showgrid),showgrid[1],getOption("tern.showgrid.minor"))
  showgrid
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

#For relative arrow positioning, this function allows global 'width' metric to be determined
.theme.get.maxlabwidth <- function(details,theme,ixseq){
  maxlabwidth <- unit(0,"npc")
  for(ix in ixseq){
    name      <- paste0("axis.tern.text.",ix)
    element   <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
    text      <- as.character(max(as.numeric(details[[paste0(ix,".labels")]])))
    grobwidth <- grobWidth(textGrob(text,gp=gpar(fontsize  =element$size,
                                                 fontfamily=element$family,
                                                 fontface  =element$face,
                                                 lineheight=element$lineheight)))
    maxlabwidth <- convertUnit(max(maxlabwidth,grobwidth),"npc")
  }
  maxlabwidth
}

#----------------------------------------------------------------------------------
#Internals >>>> Data Extremes.
#Function to determine apex points of ternary plot area in cartesian coordinates
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
# -Backgrounds
# -Grid lines
# -Ternary Border
# -Precession Arrows
# -Apex Titles
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
                                              lwd  = size *find_global_tern(".pt"),
                                              lty  = linetype
                                )
      )
      
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){
      
    })
    return(items)
  }
  
  #process the axes
  items <- .renderA("panel.background.tern",items)
  items
}
.render.grids      <- function(data.extreme,items,theme,details){
  #Process the flags.
  clockwise     <- .theme.get.clockwise(theme)
  outside       <- .theme.get.outside(theme)
  showprimary   <- .theme.get.showprimary(theme)
  showsecondary <- .theme.get.showsecondary(theme)
  showgrid.major<- .theme.get.showgrid.major(theme)
  showgrid.minor<- .theme.get.showgrid.minor(theme)
  showlabels    <- .theme.get.showlabels(theme)
  shift         <- ifthenelse(!outside,180,0)
  
  #major & minor ticklength
  tl.major <- tl.minor <- 0
  tryCatch({
    tl.major <- convertUnit(theme$axis.tern.ticklength.major,"npc",valueOnly=T)
    tl.minor <- convertUnit(theme$axis.tern.ticklength.minor,"npc",valueOnly=T)
  },error=function(x){
    #handle quietly
  })
  
  #Top, Left Right sequence.
  seq.tlr <- c("T","L","R")
  
  #ASSEMBLE THE GRID DATA.
  .getData <- function(X,ix,existing=NULL,major=TRUE,angle=0,angle.text=0){
    breaks.major <- details[[paste0(X,".major_source")]]
    breaks.minor <- details[[paste0(X,".minor_source")]]
    breaks <- if(major){ breaks.major }else{ breaks.minor }
    
    #BYPASS IF NECESSARY
    if(length(breaks) == 0){ return(existing) }
   
    labels <- if(major){details[[paste0(X,".labels")]]}else{""}
    labels <- as.character(ifthenelse(identical(labels,waiver()),100*breaks,labels))
    
    #Assign new id.
    id <- (max(existing$ID,0) + 1)
    limits <- c(0,1)
    tryCatch({
      limits <- as.numeric(details[[paste0(X,".range")]]);
    },error=function(e){
      #quietly
    })
    limits <- is.numericor(limits,c(0,1))
    ix     <- min(ix,ifthenelse(major,length(tl.major),length(tl.minor)))
    majmin <- ifthenelse(major,"major","minor")  #Major or Minor Element Name part.
    
    #The new dataframe
    new            <- data.frame(ID = id,Scale=X,Breaks=breaks,Labels=labels,Major=major)
    new            <- subset(new,Breaks >= min(limits) & Breaks <= max(limits))
    new$Prop       <- (new$Breaks - min(limits)) / abs(diff(limits))
    new$TickLength <- ifthenelse(major,tl.major[ix],tl.minor[ix])
    new$NameText   <- paste0("axis.tern.text.",X)
    new$NameTicks  <- paste0("axis.tern.ticks.",majmin,".",X)
    new$NameGrid   <- paste0("panel.grid.tern.",majmin,".",X)
    new$Major      <- major
    
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
  angles.text <- .get.angles.ticklabels(clockwise)
  
  ##get the base data.
  d <- NULL
  for(major in c(TRUE,FALSE))
    for(i in 1:length(seq.tlr))
        d <- .getData(X=seq.tlr[i],ix=i,existing=d, major = major,angle = angles[i],angle.text = angles.text[i]);
  
  if(empty(d)){ return(items) } 
  if(nrow(d) > 1){d <- d[nrow(d):1,]}  #REVERSE (minor under major)
  
  #FUNCTION TO RENDER TICKS AND LABELS
  .render.ticks <- function(name,items,d,primary=TRUE){
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      if(identical(e,element_blank()))
        return(items)
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
                  lwd     = size*find_global_tern(".pt"))
      )
      ##Add to the items.
      items[[length(items) + 1]] <- grob
    },error = function(e){ 
      warning(e)
    })
    return(items)
  }
  
  .render.labels <- function(name,items,d){ 
    tryCatch({  
      e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
      if(identical(e,element_blank()))
        return(items)
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
  .render.grid <- function(name,items,d,showgrid.major=TRUE,showgrid.minor=TRUE){
    if((unique(d$Major) & showgrid.major) | (!unique(d$Major) & showgrid.minor)){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme,verbose=F,plot=NULL)
        if(identical(e,element_blank()))
          return(items)
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
                      lwd     = size*find_global_tern(".pt"))
          )
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        }
      },error = function(e){ warning(e)})
    }
    return(items)
  }

  #PROCESS TICKS AND LABELS
  if(showgrid.major | showgrid.minor)
    for(n in unique(d$NameGrid)){ 
      items <- .render.grid(  name=n,items=items,d=d[which(d$NameGrid  == n),], showgrid.major=showgrid.major,showgrid.minor=showgrid.minor)
    } 
  if(showprimary)
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),],primary=TRUE)}
  if(showsecondary)
    for(n in unique(d$NameTicks)){items <- .render.ticks(name=n,items=items,d=d[which(d$NameTicks == n),],primary=FALSE)}
  if(showlabels)
    for(n in unique(d$NameText)){ items <- .render.labels(name=n,items=items,d=d[which(d$NameText  == n),])}
  items
}
.render.border     <- function(data.extreme,items,theme){
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
                    lwd = size*find_global_tern(".pt"))
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
.render.arrows     <- function(data.extreme,items,theme,details,maxgrob){
  axis.tern.showarrows <- theme$axis.tern.showarrows
  if(is.logical(axis.tern.showarrows) && (axis.tern.showarrows)){
    tryCatch({
      #clockwise or anticlockwise precession
      clockwise <- .theme.get.clockwise(theme)
      
      #The basic data.
      d.s <- data.extreme[ifthenelse(clockwise,c(2,3,1),c(3,1,2)),]
      d.f <- data.extreme[c(1,2,3),]
      rownames(d.s) <- rownames(d.f) #Correct rownames
      d.diff        <- d.f - d.s
      
      #arrow start and finish proportions
      arrowstart = theme$axis.tern.arrowstart
      arrowfinish= theme$axis.tern.arrowfinish
      
      #Ensure arrow start and finish length is 3.
      if(length(arrowstart) != 3 && length(arrowstart) >= 1)
        arrowstart <- rep(arrowstart[1],3)
      if(length(arrowfinish) != 3 && length(arrowfinish) >= 1)
        arrowfinish <- rep(arrowfinish[1],3)
      
      #Itterate over indexes 1:3
      for(i in c(1:3)){
        #Put in correct order.
        if(arrowfinish[i] < arrowstart[i]){
          warning(paste("Arrow size theme 'element axis.tern.arrowfinish[",i,"]' (",arrowfinish[i],") is < 'axis.tern.arrowstart[",i,"]' (",arrowstart[i],"), values will be swapped.",sep=""),call.=FALSE)
          tmp  = arrowstart[i] #hold in memory
          #Swap values
          arrowstart[i]  = arrowfinish[i]
          arrowfinish[i] = tmp
        }
        #Check finish
        if(arrowfinish[i] > 1.0){
          warning(paste("Arrow size theme 'element axis.tern.arrowfinish[",i,"]' (",arrowfinish[i],") is > 1.0 and will be truncated",sep=""),call.=FALSE)
          arrowfinish[i] = 1.0
        }
        #Check start
        if(arrowstart[i] < 0.0){
          warning(paste("Arrow size theme 'element axis.tern.arrowstart[",i,"]' (",arrowstart[i],") is < 0.0 and will be truncated",sep=""),call.=FALSE)
          arrowstart[i] = 0.0
        }
      }
      
      #Cut down to relative proportion.
      d.f <- d.f -   (1-arrowfinish)*d.diff
      d.s <- d.s +   arrowstart*d.diff
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
      arrowbaseline <- calc_element_plot("axis.tern.arrowbaseline",theme=theme)
      ticksoutside <- .theme.get.outside(theme)
      ticklength <- max(calc_element_plot("axis.tern.ticklength.major",theme=theme,verbose=F,plot=NULL),
                        calc_element_plot("axis.tern.ticklength.minor",theme=theme,verbose=F,plot=NULL))
      
      #Ensure there are EXACTLY 3 values for each metric
      if(length(arrowsep) != 3 && length(arrowsep) >= 1)
        arrowsep <- rep(arrowsep[1],3)
      if(length(arrowbaseline) != 3 && length(arrowbaseline) >= 1)
        arrowbaseline <- rep(arrowbaseline[1],3)
      ticklength = rep(ticklength,3)
      
      #get set of 3 arrowsep positions
      arrowsep <- sapply(c(1:3),function(x){
        newunit <- arrowsep[x] + 
          ifthenelse(arrowbaseline[x] >= 1 & ticksoutside,ticklength[x], unit(0,"npc")) + 
          ifthenelse(arrowbaseline[x] >= 2,.theme.get.maxlabwidth(details,theme,ixseq),unit(0,"npc"))
        
        convertWidth(newunit,"npc",valueOnly=TRUE)
      })
      
      #MOVE the Arrows Off the Axes.
      d[ixrow,"angle"]    <- .get.angles.arrows(clockwise)
      d[ixrow,"arrowsep"] <- arrowsep
      #xcoordinates
      d[,ixcol[c(1,3)]]   <- d[,ixcol[c(1,3)]] + cos(pi*d$angle/180)*arrowsep
      #ycoorinates
      d[,ixcol[c(2,4)]]   <- d[,ixcol[c(2,4)]] + sin(pi*d$angle/180)*arrowsep/coord_aspect.ternary()
      
      #Centerpoints, labels, arrowsuffix
      d$xmn   <- rowMeans(d[,ixcol[c(1,3)]])
      d$ymn   <- rowMeans(d[,ixcol[c(2,4)]])
      
      d$L     <- c(details$Tlabel,details$Llabel,details$Rlabel)
      d$W     <- c(details$Wlabel)
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
                      lwd    = size*find_global_tern(".pt"))
          )
          ##Add to the items.
          items[[length(items) + 1]] <- grob
        },error = function(error){ 
          print(error)
          warning(error)
        })
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
.render.titles     <- function(data.extreme,items,theme,details){
  showtitles <- .theme.get.showtitles(theme)
  if(!showtitles)
    return(items)
  clockwise  <- .theme.get.clockwise(theme)
  
  d    <- data.extreme
  d$L  <- as.expression(c(details$Tlabel,details$Llabel,details$Rlabel))
  
  ##Function to create new axis grob
  .render <- function(name,ix,items,hshift=0,vshift=0){
    tryCatch({
      e <- calc_element(name,theme=theme,verbose=F)
      colour    <- e$colour
      size      <- e$size;
      lineheight<- e$lineheight
      family    <- ifthenelse(is.character(e$family),e$family,"sans")
      face      <- e$face
      hjust     <- e$hjust
      vjust     <- e$vjust
      angle     <- e$angle
      
      if(!identical(e,element_blank())){
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
        items[[length(items) + 1]] <- grob
      }
    },error = function(e){ warning(e)})
    return(items)
  }
  
  #process the axes
  SHIFT <- 0.01
  items <- .render("axis.tern.title.T",1,items,vshift=  SHIFT)
  items <- .render("axis.tern.title.L",2,items,vshift= -SHIFT*tan(pi/6),hshift=-SHIFT)
  items <- .render("axis.tern.title.R",3,items,vshift= -SHIFT*tan(pi/6),hshift= SHIFT)
}






