ternbackground <- function (plot) {.ternbackground$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)}
.ternbackground <- proto(ggplot2:::Geom, {
  objname <- "background_tern"
  draw <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #get the data.
    data.background <- data.frame(T=c(1,0,0),
                                  L=c(0,1,0),
                                  R=c(0,0,1))
    data.background    <- transformTernToCart(data=data.background)
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
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})