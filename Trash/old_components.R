#THE BORDERS AROUND BACKGROUND
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
    data.border <- get_tern_extremes(coordinates)
    data.border <- coord_transform(coordinates, data.border, scales,discard=FALSE)
    
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
    data.background <- get_tern_extremes(coordinates)
    data.background <- coord_transform(coordinates, data.background, scales,discard=FALSE)
    data.background$id =  1
    
    ##Function to create new axis grob
    .render <- function(name,items){
      tryCatch({  
        e <- calc_element_plot(name,theme=theme_update(),verbose=F,plot=plot)
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
    items <- .render("panel.background.tern",items)
    
    #render.
    gTree(children = do.call("gList", items))
  }
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(fill="grey90",size=0,colour="transparent",linetype=1)
})