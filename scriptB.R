MYDATA <- data.frame(x=c(1,0,0,runif(100)),y=c(0,1,0,runif(100)),z=c(0,0,1,runif(100)))

plot <- ggplot(data=MYDATA,aes(x=x,y=y,z=z)) + geom_path()
#plot <- plot + scale_z_continuous()  
plot <- plot + coord_tern() #+ stat_density2d(aes(x,y))
plot

#scale_z_continuous() + 
#geom_point()
#plot 