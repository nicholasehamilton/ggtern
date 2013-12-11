##-----------------------------------------------
## DUMMY DATA
##-----------------------------------------------
DATA.SIMPLE <- data.frame(x=1,
                          y=1,
                          z=1)
DATA.POLY   <- data.frame(x=c(.8,.1,.1),
                          y=c(.1,.8,.1),
                          z=c(.1,.1,.8))
DATA.RANDOM <- data.frame(x     = runif(100),
                          y     = runif(100),
                          z     = runif(100),
                          Value = runif(100,1,10),
                          GroupA= as.factor(round(runif(100,1,2))),
                          GroupB= as.factor(round(runif(100,1,2))),
                          Group = as.factor(as.integer(runif(100,1,4))))

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
## Use of Linear Regression (trivial example)
##-----------------------------------------------
plot <- ggtern(data=DATA.RANDOM,aes(x,y,z,color=Group)) + 
               geom_point() + 
               geom_smooth(aes(Group=Group),size=2) + 
               theme_tern_bw()
plot

##-----------------------------------------------
## Use of geom_density2d
##-----------------------------------------------
#density2d with contours
plot <- ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
  geom_point() +
  geom_density2d()
plot

#density2d with polygon
plot <- ggtern(data=DATA.RANDOM,aes(x,y,z)) + 
  geom_point() +
  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..))
plot

##-----------------------------------------------
## Use of Faceting
##-----------------------------------------------
plot <- ggtern(data=DATA.RANDOM,aes(x,y,z)) + facet_grid(GroupA~GroupB) + 
  stat_density2d(geom="polygon",aes(fill=..level..,alpha=..level..)) +
  geom_point() + 
  labs(title="Example of Faceting",fill="Fill Metric",alpha="Alpha Metric")
plot

#Different colours
last_plot() + scale_fill_gradient(low="blue",high="red")




