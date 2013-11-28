ternborder <- function (plot) {.ternborder$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)}
.ternborder <- proto(ggplot2:::Geom, {
  objname <- "border_tern"
  draw <- function(., data, scales, coordinates,plot,...) {
    items <- list()
    theme.plot    <- plot$theme
    theme.current <- theme_update()
    
    #get the data.
    data.border <- data.frame(T=c(1,0,0),
                              L=c(0,1,0),
                              R=c(0,0,1))
    data.border <- transformTernToCart(data=data.border)
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
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})