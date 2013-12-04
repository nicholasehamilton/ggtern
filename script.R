



MYDATA <- data.frame(Mg=c(1,0,0),Zn=c(0,1,0),Ca=c(0,0,1))
plot <- ggtern(data=MYDATA,aes(x=Mg,y=Zn,z=Ca)) + geom_point() #+ geom_smooth(method=lm)
plot 


MYDATA <- data.frame(Mg=runif(100),Zn=runif(100),Ca=runif(100),colour=runif(100),
                     groupA=rep(paste(c("T1","T2"),""),50)[sample(50)],
                     groupB=rep(paste(c("P1","P2"),""),50)[sample(50)]) 

plot <- ggtern(data=MYDATA,mapping=aes(x=Mg,y=Zn,z=Ca)) + #facet_grid(groupA~groupB) + 
  geom_point(size=5,shape=21,aes(fill=factor(paste0(groupA,"/",groupB)))) + #+ coord_tern() + 
  labs(title="Example Ternary Plots w/ Facetting",fill="Temperature / Pressure",color="Series")
#plot #+ coord_tern(xlim=c(0.5,1.1),ylim=c(0,0.5))

plot <- plot + scale_T_continuous(limits=c(0.00,0.6)) + scale_L_continuous(limits=c(0.0,0.6)) + 
  scale_R_continuous(limits=c(0.4,1.0))
plot <- plot + theme(legend.position=c(0,1),legend.justification=c(0,1))

#plot <- plot + coord_tern()
plot

plot + geom_smooth(data=MYDATA,method=lm,aes(color=groupA),inherit.aes=F,aes(x=x,y=y)) 


plot <- ggtern(data=MYDATA,mapping=aes(x=Mg,y=Zn,z=Ca)) + facet_grid(groupA~groupB) + 
  geom_point(size=5,shape=21,aes(fill=factor(paste0(groupA,"/",groupB)))) + 
  labs(title="Example Ternary Plots w/ Facetting",fill="Temperature / Pressure",color="Series") + 
  theme(legend.position=c(0,1),legend.justification=c(0,1),
        #ternary.options=element_ternary(padding=0,arrowsep=0),
        plot.margin=unit(c(0,0,0,0),"cm"),
        legend.margin=unit(-0*3.5,"lines"))
plot
plot + geom_point(data=data.frame(x=1,y=-0.1),aes(x=x,y=y),inherit.aes=F,size=10,color="red")
plot + geom_smooth(data=MYDATA,method=lm)

#plot <- plot + coord_tern() #+ coord_fixed(ylim=c(0,1),xlim=c(0,1))
#plot

plot <- ggtern(data=MYDATA,aes(x=Mg,y=Zn,z=Ca))  + facet_grid(groupA~groupB) + 
  geom_point(size=5,shape=21,aes(fill=factor(paste0(groupA,"/",groupB)))) + #+ 
  labs(title="Example Ternary Plots w/ Facetting",fill="Temperature / Pressure") +  
  theme(legend.position=c(0,1),legend.justification=c(0,1))
plot + scale_T_continuous(breaks=seq(0.1,1,by=0.1),labels=c(10,20,"","","","","",80,90,100)) + 
       scale_L_continuous(breaks=seq(0.1,1,by=0.1),labels=c(10,20,"","","","","",80,90,100)) +
       scale_R_continuous(breaks=seq(0.1,1,by=0.1),labels=c(10,20,"","","","","",80,90,100)) +
       theme(ternary.options=element_ternary(arrowsep=0.04),axis.tern.arrow=element_line(size=1)) + 
geom_smooth(method="lm")

MYDATA.NEW <- data.frame(Mg=runif(100),Zn=runif(100),Ca=runif(100)) 
plot <- ggtern(data=MYDATA.NEW,mapping=aes(x=Mg,y=Zn,z=Ca))  + #facet_grid(groupA~groupB) + 
  geom_point_tern() + 
  labs(title="Example Ternary Plots w/ Facetting",fill="Temperature / Pressure") + 
  theme(legend.position=c(0,1),legend.justification=c(0,1))
plot


ggtern(data=data.frame(x=1,y=1,z=1,groupA=c(1,2,1,2),groupB=c(1,1,2,2)),aes(x=x,y=y))  + facet_grid(groupA~groupB) +
  geom_point() + coord_polar()







