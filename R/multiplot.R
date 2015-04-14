#' Arrange Multiple Plot Objects
#'
#' \code{ggtern.multi} is a function which permits the arrangement of muliple \code{ggtern} or \code{ggplot2} objects,
#' plots can be provided to the elipsis argument, or, as a list and at the simplest case, the number of columns can be
#' specified. For more advanced usage, consider the layout argument.
#'
#' By default, 1 column is specified, which means that the plots will be stacked on top of each other in a single column,
#' however, if say 4 plots are provided to the ellipsis or \code{plotlist}, with \code{cols} equal to 2, 
#' then this will produce a 2 x 2 arrangement.
#' 
#' In regards to the \code{layout} argument (which overrides the \code{cols} argument), if it is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot number 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the 
#' bottom - see the last example below.
#' @param ... multiple plot objects
#' @param plotlist alternative to the ... argument, provide a list of ggplot or grob objects, objects which do not inherit 
#' the ggplot or grob classes will be stripped.
#' @param cols number of columns if the layout parameter is not provided.
#' @param layout override number of cols, and provide a matrix specifying the layout
#' @source http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @aliases multi multiplot
#' @examples
#'  data(Feldspar)
#'  p <- ggtern(data=Feldspar,aes(Ab,An,Or)) + geom_point() + labs(title="Multiple Plot Example")
#'  
#'  #two cols
#'  ggtern.multi(p,p,cols=2)
#' 
#'  ##Below are some alternatives, uncomment to run
#'  #ggtern.multi(plotlist=list(p,p,p))
#'  #ggtern.multi(p,p,p,layout=matrix(c(1,1,2,3), nrow=2,byrow=TRUE))
#' @export
ggtern.multi <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)
  cnt <- 0
  for(i in length(plots):1){
    if(!inherits(plots[[i]],"ggplot") & !inherits(plots[[i]],"grob")){
      plots[[i]] <- NULL
      cnt <- cnt + 1
    }
  }
  if(cnt > 0)
    message("Plots have been removed, since they are not valid ggplot or ggtern objects.")
  
  #Execute.
  tryCatch({
    #Count the number of plots...
    numPlots = length(plots)
    
    #Check the layout.
    if(!is.null(layout)){
      if(is.matrix(layout) & is.numeric(layout)){
        if(numPlots < max(layout)){
          message("Plot ID's in Layout Exceed the Number of Plots provided by ... and plotlist, reverting.")
          layout = NULL
        }else{
          ##OK.
        }
      }else{
        message("Provided layout is not matrix or is not numeric.")
        layout = NULL
      }
    }
    
    #Build the layout if it is null.
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
    }
  
    #Render the plots or grobs.
    if (numPlots == 0){
      stop("No valid plots to render, aborting.")
    }else if (numPlots == 1) {
      print(plots[[1]]) 
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        if(inherits(plots[[i]],"grob")){
          grid.draw(grobTree(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col)))
        }else{
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
      }
    }
  },error=function(e){
    #message(e)
  })
}

if(0){
  p1 <- ggtern() + Tline(.5)
  p2 <- ggtern() + Lline(.5)
  p3 <- ggtern() + Rline(.5)
  test <- rep(p1,2)
  ggtern.multi(plotlist=list(p1,p2,p3,textGrob(label="The Cat Sat on the Mat",gp=gpar(fontsize=10,color="black"))),layout=matrix(c(1,2,3,4), nrow=2,byrow=FALSE))
}


