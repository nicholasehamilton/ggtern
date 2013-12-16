
#internal kills the cartesian elements.
.theme_nocart <- function(){
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

#helper function
.theme_tern      <- function(base_size = 12, base_family = "",col.BG="grey90",col.T="darkred",col.L="darkgreen",col.R="darkblue"){  
  #TEXT SIZES
  size.base      <- max(base_size-4,2)
  size.text      <- max(base_size-2,4)
  size.title     <- max(base_size-0,6)
  
  theme_gray(base_size=base_size,base_family=base_family) %+replace%
    .theme_nocart()                                       %+replace%
    theme(      
      legend.background       = element_blank(),
      ternary.options         = element_ternary(),
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
      
      axis.tern.text          = element_text(size=size.base,face="plain"),
      axis.tern.text.T        = element_text(colour=col.T,vjust=0.5,hjust=-0.2,angle =0),
      axis.tern.text.L        = element_text(colour=col.L,vjust=0.5,hjust= 1.2,angle =0),
      axis.tern.text.R        = element_text(colour=col.R,vjust=0.5,hjust= 1.2,angle =0),
      
      axis.tern.arrow.text    = element_text(size=size.text,hjust=0.5),
      axis.tern.arrow.text.T  = element_text(colour=col.T, vjust=-0.2, angle =0),
      axis.tern.arrow.text.L  = element_text(colour=col.L, vjust=-0.2, angle =0),
      axis.tern.arrow.text.R  = element_text(colour=col.R, vjust= 1.2, angle =0),
      
      axis.tern.title         = element_text(size  =size.title, angle=0,face="bold",hjust=0.5 ,vjust=0.5),
      axis.tern.title.T       = element_text(colour=col.T,vjust= 0.0),
      axis.tern.title.L       = element_text(colour=col.L,hjust= 1.0),
      axis.tern.title.R       = element_text(colour=col.R,hjust= 0.0),
      
      panel.grid.tern         = element_line(size=0.25,colour="black"),
      panel.grid.tern.major   = element_line(linetype="longdash"),
      panel.grid.tern.major.T = element_line(colour=col.T),
      panel.grid.tern.major.L = element_line(colour=col.L),
      panel.grid.tern.major.R = element_line(colour=col.R),
      panel.grid.tern.minor   = element_line(size=0.10, linetype="dotted",colour="black"),
      
      axis.tern.ticks         = element_line(size=0.25),
      axis.tern.ticks.major   = element_line(),
      axis.tern.ticks.major.T = element_line(colour=col.T),
      axis.tern.ticks.major.L = element_line(colour=col.L),
      axis.tern.ticks.major.R = element_line(colour=col.R),
      axis.tern.ticks.minor   = element_line(size=0.10,colour="black")
    )
}

#' \code{theme_tern_gray} ternary theme, gray theme
#' @param base_size base font size
#' @param base_family base font family
#' @rdname modifyterntheme
#' @export
theme_tern_gray  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="grey90",col.T="black",col.L="black",col.R="black")
}

#' \code{theme_tern_rgbg} ternary theme, red green blue with gray background
#' @rdname modifyterntheme
#' @export
theme_tern_rgbg  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="gray90")
}

#' \code{theme_tern_rgbw} ternary theme, red green blue with white background
#' @rdname modifyterntheme
#' @export
theme_tern_rgbw  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="white")
}

#' \code{theme_tern_bw} ternary theme black and white
#' @rdname modifyterntheme
#' @export
theme_tern_bw    <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, "white","black","black","black")
}

