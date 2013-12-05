# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"
  #var[var %in% c("T", "Tmin", "Tmax", "Tend", "Tintercept")] <- "T"
  #var[var %in% c("L", "Lmin", "Lmax", "Lend", "Lintercept")] <- "L"
  #var[var %in% c("R", "Rmin", "Rmax", "Rend", "Rintercept")] <- "R"
  var
}
unlockBinding("aes_to_scale", asNamespace("ggplot2"))
assign("aes_to_scale", aes_to_scale, asNamespace("ggplot2"))
lockBinding("aes_to_scale", asNamespace("ggplot2"))

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y","T","L","R")
}
unlockBinding("is_position_aes", asNamespace("ggplot2"))
assign("is_position_aes", is_position_aes, asNamespace("ggplot2"))
lockBinding("is_position_aes", asNamespace("ggplot2"))


pushback <- function(target,destination=target,namespace="ggplot2",check=T){
  if(check){if(!exists(target))stop("target does not exist")}
  if(class(target) != "character")stop("target must be specified as a character")
  writeLines(paste("patching:",target))
  unlockBinding(destination, asNamespace(namespace))
  assign(target, destination, asNamespace(namespace))
  lockBinding(destination, asNamespace(namespace))
}



.ggtern_build_axislines <- function(plot){
  ##Axis Lines
  theme.current <- theme_update()
  colour.line.L <- "transparent"
  size.line.L <- 0
  if(!identical(theme.current$axis.line.L,element_blank()) & !identical(theme.current$axis.line.L,NULL)){
    colour.line.L =theme.current$axis.line.L$colour
    size.line.L   =max(theme.current$axis.line.L$size,0)
    
    data.axis <- data.frame(x=c(1,0),y=c(0,1),z=c(0,0))
    data.axis <- .ggtern.fix.aes(data.axis,plot)
    plot <- plot + geom_path_tern(data=data.axis,aes_tern(x=x,y=y,z=z),colour=colour.line.L,size=size.line.L)
  }
  colour.line.R <- "transparent"
  size.line.R <- 0
  if(!identical(theme.current$axis.line.R,element_blank()) & !identical(theme.current$axis.line.R,NULL)){
    colour.line.R =theme.current$axis.line.R$colour
    size.line.R   =max(theme.current$axis.line.R$size,0)
    
    data.axis <- data.frame(x=c(0,0),y=c(1,0),z=c(0,1))
    data.axis <- .ggtern.fix.aes(data.axis,plot)
    plot <- plot + geom_path_tern(data=data.axis,aes_tern(x=x,y=y,z=z),colour=colour.line.R,size=size.line.R)
  }
  colour.line.T <- "transparent"
  size.line.T <- 0
  if(!identical(theme.current$axis.line.T,element_blank()) & !identical(theme.current$axis.line.T,NULL)){
    colour.line.T =theme.current$axis.line.T$colour
    size.line.T   =max(theme.current$axis.line.T$size,0)
    
    data.axis = data.frame(x=c(0,1),y=c(0,0),z=c(1,0))
    data.axis <- .ggtern.fix.aes(data.axis,plot)
    plot <- plot + geom_path_tern(data=data.axis,aes_tern(x=x,y=y,z=z),colour=colour.line.T,size=size.line.T)
  }
}

.ggtern_build_background <- function(plot){
  ##CREATE THE TERNARY BACKGROUND...
  data.border <- data.frame(A=c(1,0,0),B=c(0,1,0),C=c(0,0,1))
  data.border <- .ggtern.fix.aes(data.border,plot)
  
  rect <- plot$theme$panel.background.tern
  theme.current <- theme_update()
  if(identical(rect,NULL)){
    rect <- theme.current$panel.background.tern
  }
  fill <- rect$fill
  colour <- rect$colour
  size  <- rect$size
  linetype <- rect$linetype
  if(identical(fill,NULL))fill="transparent"
  if(identical(colour,NA))colour="transparent"
  if(identical(size,NULL))size=0
  if(identical(linetype,NULL))linetype=1
  
  ##BACKGROUND
  plot <- plot + geom_polygon_tern(data=data.border,mapping=aes(x=A,y=B,z=C,group=NULL),fill=fill,colour=colour,size=size,linetype=linetype)
  plot
}

.ggtern_build_labels <- function(plot){
  theme.current <- theme_update()
  
  ##BORDER
  data.border <- data.frame(A=c(1,0,0),B=c(0,1,0),C=c(0,0,1))
  data.border <- .ggtern.fix.aes(data.border,plot)
  
  ##LABELS
  data.labels <- data.frame(data.border,label = as.character(c(plot$labels$T,plot$labels$L,plot$labels$R)))
  data.labels <- .ggtern.fix.aes(data.labels,plot)
  
  plot <- plot + geom_text_tern(x=1,y=0,z=0,group=NULL,label=plot$labels$T,colour=theme.current$axis.title.T$colour,size=theme.current$axis.title.T$size,hjust= 0.5,vjust=-0.5)
  plot <- plot + geom_text_tern(x=0,y=1,z=0,group=NULL,label=plot$labels$L,colour=theme.current$axis.title.L$colour,size=theme.current$axis.title.L$size,hjust= 1.2,vjust= 1.0)
  plot <- plot + geom_text_tern(x=0,y=0,z=1,group=NULL,label=plot$labels$R,colour=theme.current$axis.title.R$colour,size=theme.current$axis.title.R$size,hjust=-0.2,vjust= 1.0)
  
  #plot <- plot + geom_text_tern(data=data.labels[1,],aes(x=A,y=B,z=C,label=label), colour=theme.current$axis.title.T$colour,size=theme.current$axis.title.T$size,hjust=0.5,vjust=-0.5)
  #plot <- plot + geom_text_tern(data=data.labels[2,],aes(x=A,y=B,z=C,label=label), colour=theme.current$axis.title.L$colour,size=theme.current$axis.title.L$size,hjust=1.2,vjust=1)
  #plot <- plot + geom_text_tern(data=data.labels[3,],aes(x=A,y=B,z=C,label=label), colour=theme.current$axis.title.R$colour,size=theme.current$axis.title.R$size,hjust=-0.2,vjust=1)
  plot
}


