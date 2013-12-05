.element_tree <- ggplot2:::.element_tree #DUPLICATE

#TERNARY OPTIONS.
.element_tree$ternary.options = ggplot2:::el_def("element_ternary","")

##AXIS ARROWS
.element_tree$axis.tern         = ggplot2:::el_def("element_line", "line")

.element_tree$axis.tern.arrow   = ggplot2:::el_def("element_line", "axis.tern")
.element_tree$axis.tern.arrow.T = ggplot2:::el_def("element_line", "axis.tern.arrow")
.element_tree$axis.tern.arrow.L = ggplot2:::el_def("element_line", "axis.tern.arrow")
.element_tree$axis.tern.arrow.R = ggplot2:::el_def("element_line", "axis.tern.arrow")

.element_tree$axis.tern.line    = ggplot2:::el_def("element_line", "axis.tern")
.element_tree$axis.tern.line.T  = ggplot2:::el_def("element_line", "axis.tern.line")
.element_tree$axis.tern.line.L  = ggplot2:::el_def("element_line", "axis.tern.line")
.element_tree$axis.tern.line.R  = ggplot2:::el_def("element_line", "axis.tern.line")

.element_tree$axis.tern.text    = ggplot2:::el_def("element_text", "text")
.element_tree$axis.tern.text.T  = ggplot2:::el_def("element_text", "axis.tern.text")
.element_tree$axis.tern.text.L  = ggplot2:::el_def("element_text", "axis.tern.text")
.element_tree$axis.tern.text.R  = ggplot2:::el_def("element_text", "axis.tern.text")

.element_tree$axis.tern.arrow.text    = ggplot2:::el_def("element_text", "text")
.element_tree$axis.tern.arrow.text.T  = ggplot2:::el_def("element_text", "axis.tern.arrow.text")
.element_tree$axis.tern.arrow.text.L  = ggplot2:::el_def("element_text", "axis.tern.arrow.text")
.element_tree$axis.tern.arrow.text.R  = ggplot2:::el_def("element_text", "axis.tern.arrow.text")

.element_tree$axis.tern.title   = ggplot2:::el_def("element_text", "text")
.element_tree$axis.tern.title.T = ggplot2:::el_def("element_text", "axis.tern.title")
.element_tree$axis.tern.title.L = ggplot2:::el_def("element_text", "axis.tern.title")
.element_tree$axis.tern.title.R = ggplot2:::el_def("element_text", "axis.tern.title")

.element_tree$axis.tern.ticks          = ggplot2:::el_def("element_line", "line")

.element_tree$axis.tern.ticks.major    = ggplot2:::el_def("element_line", "axis.tern.ticks")
.element_tree$axis.tern.ticks.major.T  = ggplot2:::el_def("element_line", "axis.tern.ticks.major")
.element_tree$axis.tern.ticks.major.L  = ggplot2:::el_def("element_line", "axis.tern.ticks.major")
.element_tree$axis.tern.ticks.major.R  = ggplot2:::el_def("element_line", "axis.tern.ticks.major")

.element_tree$axis.tern.ticks.minor    = ggplot2:::el_def("element_line", "axis.tern.ticks")
.element_tree$axis.tern.ticks.minor.T  = ggplot2:::el_def("element_line", "axis.tern.ticks.minor")
.element_tree$axis.tern.ticks.minor.L  = ggplot2:::el_def("element_line", "axis.tern.ticks.minor")
.element_tree$axis.tern.ticks.minor.R  = ggplot2:::el_def("element_line", "axis.tern.ticks.minor")

.element_tree$panel.grid.tern          = ggplot2:::el_def("element_line", "line")
.element_tree$panel.grid.tern.major    = ggplot2:::el_def("element_line", "panel.grid.tern")
.element_tree$panel.grid.tern.minor    = ggplot2:::el_def("element_line", "panel.grid.tern")

.element_tree$panel.grid.tern.major.T = ggplot2:::el_def("element_line", "panel.grid.tern.major")
.element_tree$panel.grid.tern.minor.T = ggplot2:::el_def("element_line", "panel.grid.tern.minor")
.element_tree$panel.grid.tern.major.L = ggplot2:::el_def("element_line", "panel.grid.tern.major")

.element_tree$panel.grid.tern.minor.L = ggplot2:::el_def("element_line", "panel.grid.tern.minor")
.element_tree$panel.grid.tern.major.R = ggplot2:::el_def("element_line", "panel.grid.tern.major")
.element_tree$panel.grid.tern.minor.R = ggplot2:::el_def("element_line", "panel.grid.tern.minor")

##TERNARY PANEL
.element_tree$panel.background.tern <- ggplot2:::el_def("element_rect", "rect")


#FUNCTION TO CLEAR THE EXISTING CARTESIAN THEME ELEMENTS
.theme_wipecartesian <- function(){
  theme(
    panel.background     = element_blank(),
    panel.border         = element_blank(),
    panel.grid.major     = element_blank(), 
    panel.grid.minor     = element_blank(), 
    axis.ticks           = element_blank(), 
    axis.text.x          = element_blank(), 
    axis.text.y          = element_blank(),
    axis.title.x         = element_blank(), 
    axis.title.y         = element_blank()
  )
}


.theme_tern      <- function(col.BG="grey90",col.T="darkred",col.L="darkgreen",col.R="darkblue"){
  
  ##CUSTOMIZE
  default    <- theme_update()
  size.title <- max(default$axis.title.x$size,5)
  size.text  <- max(default$axis.text.x$size, 3)
  
  theme(
    legend.background       = element_blank(),
    
    panel.background.tern   = element_rect(fill=col.BG,color=NA),
    axis.tern               = element_line(size=0.5,linetype="solid"),
    axis.tern.line          = element_line(),
    axis.tern.line.T        = element_line(colour=col.T),
    axis.tern.line.L        = element_line(colour=col.L),
    axis.tern.line.R        = element_line(colour=col.R),
    
    axis.tern.arrow         = element_line(lineend=arrow(length=unit(2.5,"mm"))),
    axis.tern.arrow.T       = element_line(colour=col.T),
    axis.tern.arrow.L       = element_line(colour=col.L),
    axis.tern.arrow.R       = element_line(colour=col.R),
    
    axis.tern.text          = element_text(size=size.text,face="plain"),
    axis.tern.text.T        = element_text(colour=col.T,vjust=0.5,hjust=-0.2,angle=0),
    axis.tern.text.L        = element_text(colour=col.L,vjust=0.5,hjust=1.2,angle=-60),
    axis.tern.text.R        = element_text(colour=col.R,vjust=0.5,hjust=1.2,angle=60),
    
    axis.tern.arrow.text    = element_text(size=size.text,hjust=0.5),
    axis.tern.arrow.text.T  = element_text(colour=col.T,  vjust=-0.2,angle=-60),
    axis.tern.arrow.text.L  = element_text(colour=col.L,vjust=-0.2,angle=60),
    axis.tern.arrow.text.R  = element_text(colour=col.R, vjust=1.2, angle=0),
    
    axis.tern.title   = element_text(size=5, angle=0,face="bold",hjust=0.5 ,vjust=0.5),
    axis.tern.title.T = element_text(colour=col.T,vjust=-0.5),
    axis.tern.title.L = element_text(colour=col.L,hjust=1.4),
    axis.tern.title.R = element_text(colour=col.R,hjust=-0.4),
    
    panel.grid.tern         = element_line(colour="black"),
    panel.grid.tern.major   = element_line(size=0.25, linetype="longdash"),
    panel.grid.tern.major.T = element_line(colour=col.T),
    panel.grid.tern.major.L = element_line(colour=col.L),
      panel.grid.tern.major.R = element_line(colour=col.R),
      panel.grid.tern.minor   = element_line(size=0.10, linetype="dotted",colour="black"),
      
      axis.tern.ticks.major   = element_line(size=0.25),
      axis.tern.ticks.major.T = element_line(colour=col.T),
      axis.tern.ticks.major.L = element_line(colour=col.L),
      axis.tern.ticks.major.R = element_line(colour=col.R),
      axis.tern.ticks.minor   = element_line(size=0.10,colour="black")
    )
}
theme_tern       <- function(){.theme_tern()}
theme_tern_bw    <- function(){.theme_tern("white","black","black","black")}
theme_tern_gray  <- function(){.theme_tern(col.BG="grey90",col.T="black",col.L="black",col.R="black")}

