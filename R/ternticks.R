ternticks <- function (plot) {
  .ternticks$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)
}
.ternticks <- proto(ggplot2:::Geom, {
  objname <- "ticks_tern"
  draw <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #THE SCALES.
    scale_T = function(){ret = plot$scales$get_scales("T"); if(!identical(ret,NULL)){ret}else{scale_T_continuous()}}
    scale_L = function(){ret = plot$scales$get_scales("L"); if(!identical(ret,NULL)){ret}else{scale_L_continuous()}}
    scale_R = function(){ret = plot$scales$get_scales("R"); if(!identical(ret,NULL)){ret}else{scale_R_continuous()}}
    
    ##COORDINATES OF MAJOR SEGMENTS
    d.major.T <- scale_T()$breaks; 
    d.major.L <- scale_L()$breaks; 
    d.major.R <- scale_R()$breaks;
        
    .vett.labels <- function(scale){x = scale$labels; y = scale$breaks;if(identical(x,waiver())){100*y}else{x}}
    d.major.T.labels <- .vett.labels(scale_T())
      d.major.L.labels <- .vett.labels(scale_L())
        d.major.R.labels <- .vett.labels(scale_R())
    
    ##COORDINATES OF MINOR SEGMENTS.
    d.minor.L <- scale_L()$minor_breaks; 
    d.minor.T <- scale_T()$minor_breaks; 
    d.minor.R <- scale_R()$minor_breaks
    
    #Strip minors which are occupied by majors.
    d.minor.T <- d.minor.T[which(!d.minor.T %in% d.major.T)]
    d.minor.L <- d.minor.L[which(!d.minor.L %in% d.major.L)]
    d.minor.R <- d.minor.R[which(!d.minor.R %in% d.major.R)]
    
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
    .process.ticks <- function(name,set,index,items,tl=0){
      if(tl > 0){
          set <- set[which(set > 0 & set <= 1.0)]
          if(length(set) > 0){
            V <- set
            tryCatch({
                if(index == 1){
                  d <- data.frame(coord_transform(coordinates,transformTernToCart(T=V,L=0,R=1-V,scale=F),scales),      #START
                                  coord_transform(coordinates,transformTernToCart(T=V,L=-tl,R=1+tl-V,scale=F),scales)) #FINISH
                }else if(index == 2){
                  d <- data.frame(coord_transform(coordinates,transformTernToCart(T=1-V,L=V,R=0,scale=F),scales),      #START
                                  coord_transform(coordinates,transformTernToCart(T=1+tl-V,L=V,R=-tl,scale=F),scales)) #FINISH
                }else if(index == 3){
                  d <- data.frame(coord_transform(coordinates,transformTernToCart(T=0,L=1-V,R=V,scale=F),scales),      #START
                                  coord_transform(coordinates,transformTernToCart(T=-tl,L=1+tl-V,R=V,scale=F),scales)) #FINISH
                }else{stop("index out of range")}
                colnames(d) <- c("x","y","xend","yend")
                items <- .render.ticks(name,items=items,d=d)
            },error=function(e){
              #NOTHING.
            })
          }
      }
      return(items)
    }
    
    #MAJOR GRID
    items <- .process.ticks("axis.tern.ticks.major.T",d.major.T,1,items=items,tl=tl.major)
    items <- .process.ticks("axis.tern.ticks.major.L",d.major.L,2,items=items,tl=tl.major)
    items <- .process.ticks("axis.tern.ticks.major.R",d.major.R,3,items=items,tl=tl.major)
    
    items <- .process.ticks("axis.tern.ticks.minor.T",d.minor.T,1,items=items,tl=tl.minor)
    items <- .process.ticks("axis.tern.ticks.minor.L",d.minor.L,2,items=items,tl=tl.minor)
    items <- .process.ticks("axis.tern.ticks.minor.R",d.minor.R,3,items=items,tl=tl.minor)
    
    
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
    
    .process.labels <- function(name,set,index,items,tl=0,labels){
      ix <- which(set > 0 & set <= 1.0)
      set    <- set[ix]
      labels <- labels[ix]
      if(length(set) > 0){
        V <- set;
        tryCatch({
          if(index == 1){
            d <- data.frame(coord_transform(coordinates,transformTernToCart(T=V,L=0,R=1-V,scale=F),scales),      #START
                            coord_transform(coordinates,transformTernToCart(T=V,L=-tl,R=1+tl-V,scale=F),scales), #FINISH
                            labels=labels) #FINISH
          }else if(index == 2){
            d <- data.frame(coord_transform(coordinates,transformTernToCart(T=1-V,L=V,R=0,scale=F),scales),      #START
                            coord_transform(coordinates,transformTernToCart(T=1+tl-V,L=V,R=-tl,scale=F),scales), #FINISH
                            labels=labels)
          }else if(index == 3){
            d <- data.frame(coord_transform(coordinates,transformTernToCart(T=0,L=1-V,R=V,scale=F),scales),      #START
                            coord_transform(coordinates,transformTernToCart(T=-tl,L=1+tl-V,R=V,scale=F),scales), #FINISH
                            labels=labels) 
          }else{stop("index out of range")}
          colnames(d) <- c("x","y","xend","yend","labels")
          items <- .render.labels(name,items=items,d=d)
        },error=function(e){
          message(e)
          #NOTHING.
        })
      }
      return(items)
    }
    
    items <- .process.labels("axis.tern.text.T",d.major.T,1,items=items,tl=tl.major,labels=d.major.T.labels)
    items <- .process.labels("axis.tern.text.L",d.major.L,2,items=items,tl=tl.major,labels=d.major.L.labels)
    items <- .process.labels("axis.tern.text.R",d.major.R,3,items=items,tl=tl.major,labels=d.major.R.labels)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})