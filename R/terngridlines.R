terngridlines <- function (plot) {
  .terngridlines$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)
}
.terngridlines <- proto(ggplot2:::Geom, {
  objname <- "grid_tern"
  draw <- function(.,data, scales,coordinates,plot,...){
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #THE SCALES.
    scale_T = function(){ret = plot$scales$get_scales("T"); if(!identical(ret,NULL)){ret}else{scale_T_continuous()}}
    scale_L = function(){ret = plot$scales$get_scales("L"); if(!identical(ret,NULL)){ret}else{scale_L_continuous()}}
    scale_R = function(){ret = plot$scales$get_scales("R"); if(!identical(ret,NULL)){ret}else{scale_R_continuous()}}
    
    ##COORDINATES OF MAJOR SEGMENTS
    d.major.T <- scale_T()$breaks; d.major.L <- scale_L()$breaks; d.major.R <- scale_R()$breaks
    
    ##COORDINATES OF MINOR SEGMENTS.
    d.minor.L <- scale_L()$minor_breaks; d.minor.T <- scale_T()$minor_breaks; d.minor.R <- scale_R()$minor_breaks
    
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
    .process.set <- function(name,set,index,items){
        set <- set[which(set > 0 & set < 1.0)]
        if(length(set) > 0){
          V <- set
          tryCatch({
              if(index == 1){
                d <- data.frame(coord_transform(coordinates,transformTernToCart(T=V,L=0,R=1-V),scales),
                                coord_transform(coordinates,transformTernToCart(T=V,L=1-V,R=0),scales))
              }else if(index == 2){
                d <- data.frame(coord_transform(coordinates,transformTernToCart(T=1-V,L=V,R=0),scales),
                                coord_transform(coordinates,transformTernToCart(T=0,L=V,R=1-V),scales))
              }else if(index == 3){
                d <- data.frame(coord_transform(coordinates,transformTernToCart(T=0,L=1-V,R=V),scales),
                                coord_transform(coordinates,transformTernToCart(T=1-V,L=0,R=V),scales))
              }else{stop("index out of range")}
              colnames(d) <- c("x","y","xend","yend")
              items <- .render.grid(name,items=items,d=d)  
          },error=function(e){
          })
        }
      #})
      return(items)
    }
    #MAJOR GRID
    items <- .process.set("panel.grid.tern.major.T",d.major.T,1,items=items)
    items <- .process.set("panel.grid.tern.major.L",d.major.L,2,items=items)
    items <- .process.set("panel.grid.tern.major.R",d.major.R,3,items=items)
    #MINOR GRID
    items <- .process.set("panel.grid.tern.minor.T",d.minor.T,1,items=items)
    items <- .process.set("panel.grid.tern.minor.L",d.minor.L,2,items=items)
    items <- .process.set("panel.grid.tern.minor.R",d.minor.R,3,items=items)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})