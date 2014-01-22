
#' Ternary Diagrams in R
#'
#' @description
#' Ternary diagrams are used frequently in a number of disciplines to graph compositional features for mixtures of three different elements or compounds. 
#' It is possible to represent a coordinate system having three (3) degrees of freedom, in 2D space, since the third dimention is linear and depends only 
#' on the other two. 
#' 
#' The \code{ggtern} package is based on (extends) the very popular \code{\link{ggplot}} package, which is an implementation of Wilkinsons 
#' "The Grammar of Graphics", and, makes provision for a highly methodical construction process for the development 
#' of meaningful (graphical) data representations. Of course, the above book by Wilkinson outlines the \emph{theory}, 
#' whilst Hadley Wickhams \code{\link{ggplot2}} implementation is where much of the magic happens, 
#' and, an ideal base-platform for the \code{ggtern} package.
#' 
#' In this document, some of the main features are highlighted, however, current examples (and corresponding outputs) 
#' can be viewed at http://ggtern.com
#'
#' @section \code{ggtern} Constructor:
#' Plots in \code{ggtern} are instigated via the default constructor: \code{ggtern(...)}, 
#' for additional information, click \link[=ggtern]{HERE}:
#' 
#' @section \code{ggtern} Ternary Coordinate System:
#' The foundation of this package, is the ternary coordinate system, which can be produced with the \code{coord_tern(...)} command and added to an existing 
#' ggplot object. The \code{ggtern(...)} constructor adds the \code{coord_tern(...)} coordinate system by default. 
#' For further information on the \code{coord_tern(...)} coordinate system, click \link[=coord_tern]{HERE}.
#'
#' @section Valid Geometries for \code{ggtern}:
#' ggplot2, using the \code{\link{grid}} and \code{\link{proto}} architectures, makes provision for a many number of geometries to be added 
#' progressively in \emph{'layers'} to a given base plot. Due to the nature of the ternary coordinate system, some of the 
#' geometries which are available in ggplot2, are \strong{not relevant} (or won't function) with ternary plots and as such, a limited number of 
#' 'approved' geometries can be used. Click \link[=approved_geometries]{HERE} for the full list of approved geometries.
#' 
#' Notably, \code{ggtern} includes novel geometries not available to \code{ggplot2} which include:
#' \enumerate{
#'   \item \code{\link[=geom_confidence]{Confidence Intervals via the Mahalnobis Distance}}
#'   \item \code{\link[=geom_errorbarT]{Ternary Errorbars}}
#'   \item \code{\link[=geom_Tline]{Ternary Constant-Lines}}
#' }
#' 
#' @section Handling Non-Approved Geometries:
#' If a geometric layer is added that is \strong{NOT} contained in the approved \link[=approved_geometries]{list}, \strong{IT WILL BE STRIPPED / IGNORED} from the ternary diagram 
#' when rendering takes place (notifying the user to such effect). The reason for this is that subtle 'patches' have been applied, which are mainly to do with 
#' the transformation procedures when incorporating a 'third' dimention. \strong{NB:} In the future, others may be made available once patched.
#'
#' @section New Theme Elements and Heirarchies:
#' \code{ggtern} implements many new theme elements and heirarchies which can be tailored on a case-by-case basis. 
#' The full list of new elements can is provided \link[=newelements]{HERE}.
#'
#' @section Theme Element Convenience Functions:
#' \code{ggtern} has made available a number of convenience functions, for rapid tweaking of common theme elements, for a comprehensive list, 
#' see \link[=convenience]{HERE}.
#'
#' @section Modification to Required Aesthetics:
#' Each geometry has a pre-determined set of \strong{required} aesthetics. These have been modifid such that where \code{x} and \code{y} were previously 
#' required, now an additional \code{z} aesthetic is required (\code{geom_segment} now requires \code{z} and \code{zend}). 
#' This is made possible without affecting the standard ggplot2 behaviour because \code{ggtern} distinuishes between \code{ggplot} and 
#' \code{ggtern} objects, distinguished by the presence of the \code{coord_tern(...)} coordinate system.
#' 
#' @section Provided Datasets:
#' \code{ggtern} ships with a number of datasets, including:
#' \enumerate{
#'   \item \code{\link[=data_Feldspar]{Elkin and Groves Feldspar Data}}
#'   \item \code{\link[=data_USDA]{USDA Textural Classification Data}}
#' }
#' @aliases introduction intro overview
#' @examples
#' \donttest{
#'##-----------------------------------------------
#'## DUMMY DATA
#'##-----------------------------------------------
#'set.seed(1)
#'DATA.SIMPLE <- data.frame(x=1,
#'                          y=1,
#'                          z=1)
#'DATA.POLY   <- data.frame(x=c(.8,.1,.1),
#'                          y=c(.1,.8,.1),
#'                          z=c(.1,.1,.8))
#'DATA.POLY2  <- data.frame(x=c(.7,.1,.3),
#'                          y=c(.1,.8,.2),
#'                          z=c(.2,.1,.5))
#'DATA.RANDOM <- data.frame(x     = runif(50),
#'                          y     = runif(50),
#'                          z     = runif(50),
#'                          Value = runif(50,1,10),
#'                          GroupA= as.factor(round(runif(50,1,2))),
#'                          GroupB= as.factor(round(runif(50,1,2))),
#'                          Group = as.factor(as.integer(runif(50,1,4))))
#'DATA.RANDOM2 <- data.frame(x=runif(50),
#'                           y=runif(50),
#'                           z=runif(50),
#'                           colour=runif(50),
#'                           groupA=rep(paste(c("A1","A2"),""),25)[sample(25)],
#'                           groupB=rep(paste(c("B1","B2"),""),25)[sample(25)])
#'##-----------------------------------------------
#'## CONSTRUCTION -- NOTE x, y and z aesthetics.
#'##-----------------------------------------------
#'#Method 1
#'plot <- ggtern(data=DATA.SIMPLE,aes(x,y,z)) + geom_point()
#'plot
#'
#'#Method 2
#'plot <- ggplot(data=DATA.SIMPLE,aes(x,y,z)) + geom_point() + coord_tern()
#'plot
#'
#'
#'##-----------------------------------------------
#'## MODIFY LABELS
#'##-----------------------------------------------
#'plot + labs(x="XA",y="YA",z="ZA") #Ternary Axis Method 1
#'
#'#additional commands (uncomment to execute)
#'#plot + labs(T="XB",L="YB",R="ZB") #Ternary Axis Method 2
#'
#'
#'##-----------------------------------------------
#'## Arrow Label Suffix
#'##-----------------------------------------------
#'plot + labs(W="SUFFIX")           #Suffix 
#'plot + atomic_percent()
#'
#'#additional commands (uncomment to execute)
#'#plot + weight_percent()
#'#plot + custom_percent("CUSTOM")
#'
#'
#'##-----------------------------------------------
#'## Clockwise / Anticlockwise
#'##-----------------------------------------------
#'plot + tern_clockwise()        # -- (1)
#'plot + tern_anticlockwise()    # -- (2)
#'
#'#additional commands (uncomment to execute)
#'#plot + tern_counterclockwise() #alias to #2
#'
#'
#'##-----------------------------------------------
#'## Axis Limitations / Restrictions
#'##-----------------------------------------------
#'plot + coord_tern(Tlim=c(.3,1),
#'                  Llim=c(0,.7),
#'                  Rlim=c(0,.7))
#'plot + tern_limits(1,.7,.7)    #More Convenient
#'
#'#additional commands (uncomment to execute)
#'#plot + tern_limit(1,.7,.7)     #Alias 1
#'#plot + limit_tern(1,.7,.7)     #Alias 2
#'#plot + limits_tern(1,.7,.7)    #Alias 3
#'
#'
#'##-----------------------------------------------
#'## Demonstration of Focussing on Region
#'##-----------------------------------------------
#'ggtern(data=data.frame(x=c(.8,.3,.3),
#'                       y=c(.1,.6,.1),
#'                       z=c(.1,.1,.6)),
#'       aes(x=x,y=y,z=z)) + 
#'  geom_polygon(color="red",alpha=0.25,size=2,linetype=2) + 
#'  geom_point(size=3,color="magenta",fill="yellow",shape=21) + 
#'  labs(title="Region to Focus")
#focus
#'last_plot() + coord_tern(Tlim=c(.3,.8),Llim=c(.1,0.6),Rlim=c(.1,0.6)) + 
#'  labs(title="Region After Focus")
#'
#'
#'##-----------------------------------------------
#'## Custom Themes
#'##-----------------------------------------------
#'plot + theme_tern_rgbw()
#'
#'#Additional themes (uncomment to execute)
#'#plot + theme_bw()
#'#plot + theme_gray()
#'#plot + theme_rgbg()
#'
#'
#'##-----------------------------------------------
#'## Use of Color Aesthetics -- Just like Normal!
#'##-----------------------------------------------
#'plot <- ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
#'  geom_point(aes(fill=Group),shape=21)
#'plot
#'
#'
#'##-----------------------------------------------
#'## Use of geom_polygon
#'##-----------------------------------------------
#'plot <- ggtern(data=DATA.POLY,aes(x,y,z)) + 
#'  geom_polygon(fill="yellow",
#'               alpha=0.5,
#'               color="red",
#'               size=2,
#'               linetype=2)
#'plot
#'
#'
#'##-----------------------------------------------
#'## Use of geom_point and geom_path
#'##-----------------------------------------------
#'plot <- ggtern(data=DATA.RANDOM2,aes(x,y,z)) + 
#'  geom_point() + 
#'  geom_path()
#'plot
#'
#'
#'##-----------------------------------------------
#'## Use of geom_rug
#'##-----------------------------------------------
#'last_plot() + geom_rug()
#'
#'
#'##-----------------------------------------------
#'## Use of Combined Geometries
#'##-----------------------------------------------
#'ggplot(data=DATA.POLY2,
#'       aes(x=x,y=y,z=z)) + 
#'  coord_tern() + 
#'  geom_polygon(alpha=0.5,color="red",size=2) +
#'  geom_path(color="blue",linetype=2,size=1) +
#'  geom_point(size=3,fill="yellow",
#'             color="red",shape=21) +
#'  geom_smooth(method="lm",
#'              limitarea=FALSE,
#'              se=FALSE,
#'              fullrange=TRUE,
#'              color="magenta",
#'              size=1,
#'              n=25,
#'              linetype=3)
#'
#'
#'##-----------------------------------------------
#'## Use of Segment (req. z and zend aesthetics)
#'##-----------------------------------------------
#'ggtern(data=data.frame(A=0.6,B=0.2,C=0.2,
#'                       xend=0.2,yend=0.6,zend=0.2),
#'       aes(x=A,y=B,z=C,xend=xend,yend=yend,zend=zend)) + 
#'  geom_segment(size=2,arrow=arrow(),color="red")
#'
#'
#'##-----------------------------------------------
#'## Use of Linear Regression (trivial example)
#'##-----------------------------------------------
#'ggtern(data=DATA.RANDOM,aes(x,y,z,color=Group)) + 
#'  geom_point() + 
#'  geom_smooth(aes(Group=Group),size=2,n=25) + 
#'  theme_bw()
#'
#'
#'##-----------------------------------------------
#'## Use of geom_density2d
#'##-----------------------------------------------
#'#density2d with polygon and contours,n=25 for speed
#'ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
#'  geom_point() +
#'  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..),n=25) +
#'  geom_density2d(color="red",linetype=2,n=25)
#'
#'
#'##-----------------------------------------------
#'## Use of Faceting
#'##-----------------------------------------------
#'ggtern(data=DATA.RANDOM,aes(x,y,z)) + facet_grid(GroupA~GroupB) + 
#'  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..),n=25) +
#'  geom_point() + 
#'  scale_fill_gradient(low="blue",high="red") + 
#'  labs(title="Example of Faceting",fill="Fill Metric",alpha="Alpha Metric")
#'
#'
#'##-----------------------------------------------
#'## Zoom in on particular region.
#'##-----------------------------------------------
#' plot + labs(title="Example of Region Focus") + 
#'  coord_tern(xlim=c(0.5,1.0)) + 
#'  theme(legend.position=c(1,1),
#'        legend.justification=c(1,1),
#'        legend.direction="vertical",legend.box="horizontal",
#'        legend.box.just="top")
#'
#'
#'##-----------------------------------------------
#'## Example NO ARROWS
#'##-----------------------------------------------
#'plot + theme_noarrows() + labs(title="Example of No Arrows")
#'
#'##-----------------------------------------------
#'## Control of Ticks
#'##-----------------------------------------------
#'plot <- plot + theme_bw()
#'plot + theme_ticksinside()  #Ticks Inside
#'plot + theme_showsecondary()#Secondary ticks
#'
#'##-----------------------------------------------
#'## Example RECESSED Arrows
#'##-----------------------------------------------
#'ggtern(data=DATA.RANDOM,mapping=aes(x,y,z)) + 
#'  geom_point() + 
#'  theme_rgbw() + 
#'  tern_limits(labels=c(0,10,20,"","","","","",80,90,100)) + #AFFECT ALL SCALES 
#'  theme(ternary.options=element_ternary(arrowsep=0.03,arrowstart=0.25,arrowfinish=0.75))
#' }
#' @aliases ggtern-package
#' @name ggtern-package
#' @rdname ggtern-package
NULL