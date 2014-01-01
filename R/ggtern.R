
#'Ternary Diagrams in R
#'
#'Ternary diagrams are used frequently in materials science to graph compositional features for mixtures of three different elements or compounds.
#'This package is based (extends) the very popular \code{\link{ggplot}} package, which is an implementation of Wilkinsons "The Grammar of Graphics".
#'
#'Plots in \code{ggtern} are instigated via the default constructor: \code{ggtern(...)}, which is essentially a convenience wrapper for the following: 
#'\code{ggplot{...} + coord_tern()}, indeed, if one wishes to use \code{ggplot{...} + coord_tern()} then this is quite satisfactory, however, it is important
#'to note that once the \code{coord_tern()} coordinate system has been applied, the object is no longer strictly a ggplot object, rather, a ggtern object and 
#'several patches have been applied to facilitate correct plotting, including some limitations on the types of geometries which can be used. One such essential 
#'patch is, for approved geometries previously requiring \code{x} and \code{y} coordinates, now require an additional \code{z} coordinate. 
#'\code{\link[ggtern]{geom_segment}} goes one step further in that it requires both an additional \code{z} and \code{zend} coordinate mappings.
#'
#' @section Valid Geometries for ggtern:
#'ggplot2, using the \code{\link{grid}} and \code{\link{proto}} architectures, makes provision for a many number of geometries to be added 
#'progressively in \emph{'layers'} to a given base plot. In this version 1.0 (the first release of this package), some of the 
#'geometries which are available in ggplot2, are \strong{not relevant} (or wont function) with ternary plots. As such, a limited number of 
#''approved' geometries can be used, includes the following in the first instance:
#'\enumerate{
#'  \item \code{\link[=geom_point]{Point}}
#'  \item \code{\link[=geom_path]{Path}}
#'  \item \code{\link[=geom_segment]{Segment}}
#'  \item \code{\link[=geom_polygon]{Polygon}}
#'  \item \code{\link[=geom_smooth]{Smooth}}
#'  \item \code{\link[=geom_text]{Text}}
#'  \item \code{\link[=geom_density2d]{Density2d}}
#'  \item \code{\link[=geom_rug]{Rug}}
#'}
#'Additionally, ggterin incldues novel geometries, including:
#'\enumerate{
#'  \item \code{\link[=geom_confidence]{Confidence}}
#'}
#'@section Handling Non-Approved Geometries:
#'If a geometric layer is added that is \strong{NOT} contained in the above list, \strong{IT WILL BE STRIPPED / IGNORED} from the ternary diagram 
#'when rendering takes place (notifying the user to such effect). The reason for this is that subtle 'patches' have been applied, which are mainly to do with 
#'the transformation procedures when incorporating a 'third' dimention. \strong{NB:} In the future, others may be made available once patched.
#'
#'@section New Theme Elements and Heirarchies:
#'\code{ggtern} implements many new theme elements and heirarchies which can be tailored on a case-by-case basis. 
#'The full list of new elements can is provided \link[=newelements]{HERE}.
#'
#'@section Theme Element Convenience Functions:
#'\code{ggtern} has made available a number of convenience functions, for rapid tweaking of common theme elements, for a comprehensive list, 
#'see \link[=convenience]{HERE}.
#'
#'@section Modification to Required Aesthetics:
#'Each geometry has a pre-determined set of \strong{required} aesthetics. These have been modifid such that where \code{x} and \code{y} were previously 
#'required, now an additional \code{z} aesthetic is required (\code{geom_segment} now requires \code{z} and \code{zend}). 
#'This is made possible without affecting the standard ggplot2 behaviour because \code{ggtern} distinuishes between ggplot and 
#'ggtern objects.
#'
#'\code{ggtern} is a method which is a convenience wrapper for \code{ggplot(...) + coord_tern()}, and, analogous to \code{ggplot}, 
#'which creates an object of class \code{ggtern} (which inherits \code{ggplot}).
#'@examples
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
#'#plot + theme_tern_bw()
#'#plot + theme_tern_gray()
#'#plot + theme_tern_rgbg()
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
#'  theme_tern_bw()
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
#'
#'##-----------------------------------------------
#'## Example RECESSED Arrows
#'##-----------------------------------------------
#'ggtern(data=DATA.RANDOM,mapping=aes(x,y,z)) + 
#'  geom_point() + 
#'  theme_tern_rgbw() + 
#'  tern_limits(labels=c(0,10,20,"","","","","",80,90,100)) + #AFFECT ALL SCALES 
#'  theme(ternary.options=element_ternary(arrowsep=0.03,arrowstart=0.25,arrowfinish=0.75))
#' }
#'@inheritParams ggplot2::ggplot
#'@export
ggtern <- function(data=NULL,...){ggplot(data=data,...) + coord_tern()}







