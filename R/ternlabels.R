ternlabels <- function (plot) {.ternlabels$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)}
.ternlabels <- proto(ggplot2:::Geom, {
  objname <- "background_tern"
  draw <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #get the data.
    d <- data.frame(T=c(1,0,0),
                    L=c(0,1,0),
                    R=c(0,0,1))
    d    <- transformTernToCart(data=d)
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
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})