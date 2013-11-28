ternarrows <- function (plot) {.ternarrows$new(mapping=aes(x=x,y=y,z=NULL),data=data.frame(x=0,y=0,z=0),plot=plot)}

.ternarrows <- proto(ggplot2:::Geom, {
  objname <- "border_tern"
  draw <- function(.,data,scales,coordinates,plot,...) {
    items <- list()
    if(!is.logical(plot$theme$ternary.options$showarrows)){
      ##BYPASS IF NULL
    }else if(plot$theme$ternary.options$showarrows){
      .ifthenelse <- function(i,y,n){if(i){y}else{n}}
      e <- calc_element_plot("ternary.options",theme=theme_update(),verbose=F,plot=plot)
      
      ##GET THE DATA.
      b <-.ifthenelse(is.numeric(e$arrowsep),e$arrowsep[1],0)
      s <- min(max(e$arrowstart, 0.0),1.0)
      f <- max(min(e$arrowfinish,1.0),0.0)
      
      b = -b
      #START & FINISH
      d <- data.frame(TS=c(s,1-s-b,+b),LS=c(b,s,1-s-b),RS=c(1-s-b,+b,s), 
                      TE=c(f,1-f-b,b), LE=c(b,f,1-f-b),RE=c(1-f-b,b,f))
      d <- rbind(transformTernToCart(data=d[,1:3],scale=F), #SPLIT
                 transformTernToCart(data=d[,4:6],scale=F))
      d <- coord_transform(coordinates,d,scales)
      d <- cbind(d[1:3,],d[4:6,]);                          #JOIN
      colnames(d) <- c("x","y","xend","yend")
      d$xmn   <- rowMeans(d[,c("x","xend")])
      d$ymn   <- rowMeans(d[,c("y","yend")])
      d$L     <- c(plot$labels$T,
                   plot$labels$L,
                   plot$labels$R)
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
  default_aes <- function(.) aes()
  guide_geom <- function(.) "none"
})