
#internal kills the cartesian elements.
.theme_nocart <- function(){
    theme(
      #panel.background     = element_blank(),
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
.theme_tern      <- function(base_size           = 12, 
                             base_family         = "",
                             base_ggplot2_theme  = "theme_gray",
                             col.BG              = "grey90",
                             col.T               = "black",
                             col.L               = "black",
                             col.R               = "black",
                             col.grid.T          = "white",
                             col.grid.L          = "white",
                             col.grid.R          = "white",
                             col.grid.minor      = "grey95",
                             col.axis.T          = col.T,
                             col.axis.L          = col.L,
                             col.axis.R          = col.R,
                             col.arrow.T         = col.T,
                             col.arrow.L         = col.L,
                             col.arrow.R         = col.R,
                             col.title.T         = col.T,
                             col.title.L         = col.L,
                             col.title.R         = col.R,
                             col.arrow.text.T    = col.T,
                             col.arrow.text.L    = col.L,
                             col.arrow.text.R    = col.R,
                             col.ticks.major     = "black",
                             axis.tern.size      = 0.5,
                             showarrow           = getOption("tern.showarrows"),
                             ticks.outside       = getOption("tern.ticks.outside"),
                             ticks.showsecondary = getOption("tern.ticks.showsecondary"),
                             ticks.showprimary   = getOption("tern.ticks.showprimary"),
                             grid.linetype       = 1,
                             grid.linetype.minor = grid.linetype,
                             grid.major.size     = NULL,
                             ticklength.major    = unit(0.010,"npc"),
                             ticklength.minor    = unit(0.005,"npc"),
                             margin.tern         = unit(1,"lines")){  
  #TEXT SIZES
  size.base      <- max(base_size-4,2)
  size.text      <- max(base_size-2,4)
  size.title     <- max(base_size-0,6)
  
  get(base_ggplot2_theme,asNamespace("ggplot2"))(
    base_size=base_size,base_family=base_family)            %+replace%
    #.theme_nocart()                                         %+replace%
    theme(      
      legend.background       = element_blank(),
      ternary.options         = element_ternary(),
      
      panel.background.tern      = element_rect(fill=col.BG,color=NA),
      axis.tern.clockwise        = getOption("tern.clockwise"),
      axis.tern.showarrows       = showarrow,
      axis.tern.showtitles       = getOption("tern.showtitles"),
      axis.tern.showlabels       = getOption("tern.showlabels"),
      axis.tern.arrowstart       = getOption("tern.arrowstart"),
      axis.tern.arrowfinish      = getOption("tern.arrowfinish"),
      axis.tern.arrowbaseline    = getOption("tern.arrowbaseline"),
      
      plot.margin                = unit(c(1,1,1,1),"lines"),
      panel.margin               = unit(0.25,"lines"),
      axis.tern.padding          = unit(c(2,0,0,0),"lines"),
      axis.tern.hshift           = unit(0.0,       "npc"),
      axis.tern.vshift           = unit(0.0,       "npc"),
      axis.tern.arrowsep         = unit(1.0,       "lines"),
      
      axis.tern               = element_line(size=axis.tern.size,linetype="solid"),
      axis.tern.line          = element_line(),
      axis.tern.line.T        = element_line(colour=col.axis.T),
      axis.tern.line.L        = element_line(colour=col.axis.L),
      axis.tern.line.R        = element_line(colour=col.axis.R),
      
      axis.tern.arrow         = element_line(lineend=arrow(length=unit(2.5,"mm"))),
      axis.tern.arrow.T       = element_line(colour=col.arrow.T),
      axis.tern.arrow.L       = element_line(colour=col.arrow.L),
      axis.tern.arrow.R       = element_line(colour=col.arrow.R),
      
      axis.tern.text          = element_text(size=size.base,face="plain",hjust=0.5,vjust=0.5),
      axis.tern.text.T        = element_text(colour=col.T,hjust=-0.2),
      axis.tern.text.L        = element_text(colour=col.L,hjust=+1.2),
      axis.tern.text.R        = element_text(colour=col.R,hjust=+1.2),
      
      axis.tern.arrow.text    = element_text(size=size.text,face="plain",hjust=0.5,vjust=0.5),
      axis.tern.arrow.text.T  = element_text(colour=col.arrow.text.T, vjust=-0.2),
      axis.tern.arrow.text.L  = element_text(colour=col.arrow.text.L, vjust=-0.2),
      axis.tern.arrow.text.R  = element_text(colour=col.arrow.text.R, vjust= 1.2),
      
      axis.tern.title         = element_text(size=size.title,face="bold",hjust=0.5 ,vjust=0.5),
      axis.tern.title.T       = element_text(colour=col.title.T,vjust= -0.1),
      axis.tern.title.L       = element_text(colour=col.title.L,hjust= +1.1),
      axis.tern.title.R       = element_text(colour=col.title.R,hjust= -0.1),
      
      panel.grid.tern         = element_line(linetype=grid.linetype),
      panel.grid.tern.major   = element_line(color="black",size=grid.major.size),
      panel.grid.tern.major.T = element_line(colour=col.grid.T),
      panel.grid.tern.major.L = element_line(colour=col.grid.L),
      panel.grid.tern.major.R = element_line(colour=col.grid.R),
      panel.grid.tern.minor   = element_line(size=0.25,colour=col.grid.minor,linetype=grid.linetype.minor),
      
      axis.tern.ticks.outside       = ticks.outside,
      axis.tern.ticks.showsecondary = ticks.showsecondary,
      axis.tern.ticks.showprimary   = ticks.showprimary,
      
      axis.tern.ticklength.major    = ticklength.major,
      axis.tern.ticklength.minor    = ticklength.minor,
      
      axis.tern.ticks         = element_line(),
      axis.tern.ticks.major   = element_line(color=col.ticks.major),
      axis.tern.ticks.major.T = element_line(colour=col.T),
      axis.tern.ticks.major.L = element_line(colour=col.L),
      axis.tern.ticks.major.R = element_line(colour=col.R),
      
      axis.tern.ticks.minor   = element_line(size=0.20),
      axis.tern.ticks.minor.T = element_line(colour=col.T),
      axis.tern.ticks.minor.L = element_line(colour=col.L),
      axis.tern.ticks.minor.R = element_line(colour=col.R),
      
      panel.margin.tern       = margin.tern
    )
}

#' A theme with grey background and white gridlines (ggtern version)
#' 
#' \code{theme_gray} is a theme with grey background and white gridlines.
#' @aliases theme_tern_gray theme_grey
#' @param base_size base font size
#' @param base_family base font family
#' @rdname theme_gray
#' @export
theme_gray  <- function(base_size = 12, base_family = ""){
  #if(!inherits(get_last_coord(),"ternary")){return(ggplot2::theme_gray(base_size=base_size,base_family=base_family))}
  .theme_tern(base_size=base_size, base_family=base_family, 
              col.BG="grey90",
              col.T ="gray50",col.L="gray50",col.R="gray50",
              col.axis.T="grey90",col.axis.L="grey90",col.axis.R="grey90",
              col.title.T="black",col.title.L="black",col.title.R="black",
              axis.tern.size = 0.25,ticklength.minor = unit(0,"npc"),
              showarrow=FALSE,
              col.arrow.text.T="black",col.arrow.text.L="black",col.arrow.text.R="black")
}
theme_tern_gray <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_gray has been superceded by the ggplot2 standard theme_gray")
  theme_gray(base_size,base_family)
}
theme_tern_grey <- theme_tern_gray
theme_grey <- theme_gray

#' A theme with grey background, red, green and blue axes and white gridlines
#' 
#' \code{theme_rgbg} is a theme with grey background, red, green and blue axes and white gridlines
#' @aliases theme_tern_rgbg theme_rgb
#' @inheritParams theme_gray
#' @rdname theme_rgbg
#' @seealso \code{\link{theme_rgbw}}
#' @export
theme_rgbg  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, 
              col.BG="gray90",
              col.T="darkred",col.L="darkblue",col.R="darkgreen")
}
theme_tern_rgbg <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbg has been superceded by theme_rgbg")
  theme_rgbg(base_size,base_family)
}
theme_rgb <- theme_rgbg

#' Theme with custom basic colours
#' 
#' @param base_size base font size
#' @param base_family base font family
#' @param col.T colour of top axis, ticks labels and major gridlines
#' @param col.L colour of left axis, ticks, labels and major gridlines
#' @param col.R colour of right axis, ticks, labels and major gridlines
#' @param col.BG colour of background colour to plot area
#' \code{theme_custom} is a convenience function to allow the user to control the basic theme colours very easily.
#' @export
theme_custom  <- function(base_size = 12,
                          base_family = "",
                          col.T='gray95',
                          col.L='gray95',
                          col.R='gray95',
                          col.BG="transparent",
                          col.grid.minor="gray90"){
  .theme_tern(base_size=base_size, base_family=base_family, 
                        col.BG     = col.BG,
                        col.T      = col.T,
                        col.L      = col.L,
                        col.R      = col.R,
                        col.grid.T = col.T,
                        col.grid.L = col.L,
                        col.grid.R = col.R,
                        col.grid.minor = col.grid.minor,
                        grid.linetype=6,
                        grid.linetype.minor=1,
                        grid.major.size=0.25)
}

#' A theme with white background, red, green and blue axes and gidlines
#' 
#' \code{theme_rgbw} is a theme with white background, red, green and blue axes and gridlines
#' @rdname theme_rgbw
#' @inheritParams theme_gray
#' @aliases theme_tern_rgbw
#' @seealso \code{\link{theme_rgbg}}
#' @export
theme_rgbw  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="white",
              col.T="darkred",
              col.L="darkblue",
              col.R="darkgreen",
              col.grid.T ="darkred",
              col.grid.L="darkblue",
              col.grid.R="darkgreen",
              col.grid.minor ="gray90",grid.linetype=6,grid.linetype.minor=1,grid.major.size=0.25)
}
theme_tern_rgbw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbw has been superceded by theme_rgbw")
  theme_rgbw(base_size,base_family)
}

#' A theme with white background and black gridlines (ggtern version)
#' 
#' \code{theme_bw} is a theme with white background and black gridlines
#' @rdname theme_bw
#' @inheritParams theme_gray
#' @aliases theme_tern_bw
#' @export
theme_bw    <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family,base_ggplot2_theme="theme_bw",
              col.BG=NA,
              col.T ="black",
              col.L ="black",
              col.R ="black",
              col.grid.T="gray90",
              col.grid.L="gray90",
              col.grid.R="gray90",
              col.axis.T="gray50",
              col.axis.L="gray50",
              col.axis.R="gray50",
              col.arrow.T="gray50",
              col.arrow.L="gray50",
              col.arrow.R="gray50",
              #col.grid.minor="gray98",
              grid.linetype.minor=1,
              grid.linetype=1,
              grid.major.size=0.25)
}
theme_tern_bw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_bw has been superceded by the ggplot2 standard theme_bw")
  theme_bw(base_size,base_family)
}



#' A minimalistic theme with no background annotations (ggtern version).
#'
#' @param base_size base font size
#' @param base_family base font family
#' @aliases theme_tern_minimal
#' @export
theme_minimal    <- function(base_size = 12, base_family = ""){
  #if(!inherits(get_last_coord(),"ternary")){
  #  return(ggplot2::theme_bw(base_size=base_size,base_family=base_family))
  #}
  .theme_tern(base_size=base_size, base_family=base_family,base_ggplot2_theme="theme_minimal",
              col.BG=NA,
              col.T ="black",
              col.L ="black",
              col.R ="black",
              col.grid.T="gray90",
              col.grid.L="gray90",
              col.grid.R="gray90",
              col.axis.T="gray90",
              col.axis.L="gray90",
              col.axis.R="gray90",
              col.arrow.T="gray50",
              col.arrow.L="gray50",
              col.arrow.R="gray50",
              col.grid.minor="gray98",
              grid.linetype.minor=1,
              grid.linetype=1,
              grid.major.size=0.25,
              ticklength.minor=unit(0,"npc"),
              showarrow=FALSE)
}
theme_tern_minimal <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_minimal has been superceded by the ggplot2 standard theme_minimal")
  theme_minimal(base_size,base_family)
}

#' A classic-looking theme, with x and y axis lines and no gridlines (ggtern version).
#'
#' @param base_size base font size
#' @param base_family base font family
#' @aliases theme_tern_classic
#' @export
theme_classic <- function(base_size=12,base_family=""){
  .theme_tern(base_size=base_size, base_family=base_family,base_ggplot2_theme="theme_classic",
              col.BG=NA,
              col.T ="black",
              col.L ="black",
              col.R ="black",
              col.grid.T="black",
              col.grid.L="black",
              col.grid.R="black", 
              col.ticks.major="magenta",
              col.grid.minor="gray90") %+replace% theme(axis.tern.showgrid.major=FALSE,axis.tern.showgrid.minor=FALSE)
}
theme_tern_classic <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_classic has been superceded by the ggplot2 standard theme_classic")
  theme_classic(base_size,base_family)
}





