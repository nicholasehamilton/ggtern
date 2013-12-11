##-----------------------------------------------
## DUMMY DATA
##-----------------------------------------------
set.seed(1)
DATA.SIMPLE <- data.frame(x=1,
                          y=1,
                          z=1)
DATA.POLY   <- data.frame(x=c(.8,.1,.1),
                          y=c(.1,.8,.1),
                          z=c(.1,.1,.8))
DATA.POLY2  <- data.frame(x=c(.7,.1,.3),
                          y=c(.1,.8,.2),
                          z=c(.2,.1,.5))
DATA.RANDOM <- data.frame(x     = runif(100),
                          y     = runif(100),
                          z     = runif(100),
                          Value = runif(100,1,10),
                          GroupA= as.factor(round(runif(100,1,2))),
                          GroupB= as.factor(round(runif(100,1,2))),
                          Group = as.factor(as.integer(runif(100,1,4))))
DATA.RANDOM2 <- data.frame(x=runif(100),
                           y=runif(100),
                           z=runif(100),
                           colour=runif(100),
                           groupA=rep(paste(c("A1","A2"),""),50)[sample(50)],
                           groupB=rep(paste(c("B1","B2"),""),50)[sample(50)])

##-----------------------------------------------
## CONSTRUCTION -- NOTE x, y and z aesthetics.
##-----------------------------------------------
#Method 1
plot <- ggtern(data=DATA.SIMPLE,aes(x,y,z)) + geom_point()
plot

#Method 2
plot <- ggplot(data=DATA.SIMPLE,aes(x,y,z)) + geom_point() + coord_tern()
plot

##-----------------------------------------------
## MODIFY LABELS
##-----------------------------------------------
plot + labs(x="XA",y="YA",z="ZA") #Ternary Axis Method 1
plot + labs(T="XB",L="YB",R="ZB") #Ternary Axis Method 2

##-----------------------------------------------
## Arrow Label Suffix
##-----------------------------------------------
plot + labs(W="SUFFIX")           #Suffix 
plot + atomic_percent()
plot + weight_percent()
plot + custom_percent("CUSTOM")

##-----------------------------------------------
## Clockwise / Anticlockwise
##-----------------------------------------------
plot + tern_clockwise()        # -- (1)
plot + tern_anticlockwise()    # -- (2)
plot + tern_counterclockwise() #alias to #2

##-----------------------------------------------
## Axis Limitations / Restrictions
##-----------------------------------------------
plot + coord_tern(Tlim=c(.3,1),
                  Llim=c(0,.7),
                  Rlim=c(0,.7))
plot + tern_limits(1,.7,.7)    #More Convenient
plot + tern_limit(1,.7,.7)     #Alias 1
plot + limit_tern(1,.7,.7)     #Alias 2
plot + limits_tern(1,.7,.7)    #Alias 3

##-----------------------------------------------
## Demonstration of Focussing on Region
##-----------------------------------------------
ggtern(data=data.frame(x=c(.8,.3,.3),
                               y=c(.1,.6,.1),
                               z=c(.1,.1,.6)),aes(x=x,y=y,z=z)) + 
  geom_polygon(color="red",alpha=0.25,size=2,linetype=2) + 
  geom_point(size=3,color="magenta",fill="yellow",shape=21) + 
  labs(title="Region to Focus")
last_plot() + coord_tern(Tlim=c(.3,.8),Llim=c(.1,0.6),Rlim=c(.1,0.6)) + 
              labs(title="Region After Focus")

##-----------------------------------------------
## Custom Themes
##-----------------------------------------------
plot + theme_tern_gray()
plot + theme_tern_bw()
plot + theme_tern_rgbw()
plot + theme_tern_rgbg()

##-----------------------------------------------
## Use of Color Aesthetics -- Just like Normal!
##-----------------------------------------------
plot <- ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
        geom_point(aes(fill=Group),shape=21)
plot

##-----------------------------------------------
## Use of geom_polygon
##-----------------------------------------------
plot <- ggtern(data=DATA.POLY,aes(x,y,z)) + 
        geom_polygon(fill="yellow",
                     alpha=0.5,
                     color="red",
                     size=2,
                     linetype=2)
plot

##-----------------------------------------------
## Use of geom_point and geom_path
##-----------------------------------------------
plot <- ggtern(data=DATA.RANDOM2,aes(x,y,z)) + geom_point() + geom_path()
plot

##-----------------------------------------------
## Use of geom_rug
##-----------------------------------------------
last_plot() + geom_rug()

##-----------------------------------------------
## Use of Combined Geometries
##-----------------------------------------------
ggplot(data=DATA.POLY2,
            aes(x=x,y=y,z=z)) + 
        coord_tern() + 
        geom_polygon(alpha=0.5,color="red",size=2) +
        geom_path(color="blue",linetype=2,size=1) +
        geom_point(size=3,fill="yellow",
                   color="red",shape=21) +
        geom_smooth(method="lm",
                    limitarea=FALSE,
                    se=FALSE,
                    fullrange=TRUE,
              color="magenta",size=1,linetype=3)

##-----------------------------------------------
## Use of Segment (req. z and zend aesthetics)
##-----------------------------------------------
ggtern(data=data.frame(A=0.6,B=0.2,C=0.2,
                       xend=0.2,yend=0.6,zend=0.2),
               aes(x=A,y=B,z=C,xend=xend,yend=yend,zend=zend)) + 
       geom_segment(size=2,arrow=arrow(),color="red")

##-----------------------------------------------
## Use of Linear Regression (trivial example)
##-----------------------------------------------
ggtern(data=DATA.RANDOM,aes(x,y,z,color=Group)) + 
               geom_point() + 
               geom_smooth(aes(Group=Group),size=2) + 
               theme_tern_bw()

##-----------------------------------------------
## Use of geom_density2d
##-----------------------------------------------
#density2d with contours
ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
  geom_point() +
  geom_density2d()

#density2d with polygon
ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
  geom_point() +
  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..))

##-----------------------------------------------
## Use of Faceting
##-----------------------------------------------
ggtern(data=DATA.RANDOM,aes(x,y,z)) + facet_grid(GroupA~GroupB) + 
  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..)) +
  geom_point() + 
  labs(title="Example of Faceting",fill="Fill Metric",alpha="Alpha Metric")

#Different colours
last_plot() + scale_fill_gradient(low="blue",high="red")

##-----------------------------------------------
## More Facetting
##-----------------------------------------------
plot <- ggtern(data=DATA.RANDOM2,mapping=aes(x=x,y=y,z=z)) + 
  labs(title ="Example Ternary Plots w/ Faceting",
       fill  ="Fill Metric",
       color ="Color Metric",
       alpha ="Alpha Metric",
       x     ="T",
       y     ="L",
       z     ="R") + 
  stat_density2d(fullrange=TRUE,n=200,geom="polygon",
                 aes(fill=..level..,alpha=..level..)) + 
  theme_tern_rgbw() + 
  atomic_percent() + 
  geom_point(size=3,shape=16,aes(colour=factor(paste0(groupA,"/",groupB))))
plot + facet_grid(groupA~groupB)
plot + facet_wrap(groupA~groupB)

##-----------------------------------------------
## Zoom in on particular region.
##-----------------------------------------------
plot + labs(title="Example of Region Focus") + 
  coord_tern(xlim=c(0.5,1.0)) + 
  theme(legend.position=c(1,1),
        legend.justification=c(1,1),
        legend.direction="vertical",legend.box="horizontal",
        legend.box.just="top")

##-----------------------------------------------
## Example NO ARROWS
##-----------------------------------------------
plot + theme_noarrows() + labs(title="Example of No Arrows")

##-----------------------------------------------
## Example RECESSED Arrows
##-----------------------------------------------
ggtern(data=DATA.RANDOM,mapping=aes(x,y,z)) + 
  geom_point() + 
  theme_tern_rgbw() + 
  tern_limits(labels=c(0,10,20,"","","","","",80,90,100)) + #AFFECT ALL SCALES 
  theme(ternary.options=element_ternary(arrowsep=0.03,arrowstart=0.25,arrowfinish=0.75))




