
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
.theme_tern      <- function(base_size  = 12, 
                             base_family= "",
                             base_ggplot2_theme="theme_gray",
                             col.BG        ="grey90",
                             col.T         ="black",
                             col.L         ="black",
                             col.R         ="black",
                             grid.T        ="white",
                             grid.L        ="white",
                             grid.R        ="white",
                             grid.minor    ="grey95",
                             grid.linetype =1,
                             grid.linetype.minor = grid.linetype,
                             grid.major.size=NULL){  
  #TEXT SIZES
  size.base      <- max(base_size-4,2)
  size.text      <- max(base_size-2,4)
  size.title     <- max(base_size-0,6)
  
  #ggplot2::theme_gray(base_size=base_size,base_family=base_family)  %+replace%
  get(base_ggplot2_theme,asNamespace("ggplot2"))(
    base_size=base_size,base_family=base_family)            %+replace%
    .theme_nocart()                                         %+replace%
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
      
      panel.grid.tern         = element_line(linetype=grid.linetype),
      panel.grid.tern.major   = element_line(color="black",size=grid.major.size),
      panel.grid.tern.major.T = element_line(colour=grid.T),
      panel.grid.tern.major.L = element_line(colour=grid.L),
      panel.grid.tern.major.R = element_line(colour=grid.R),
      panel.grid.tern.minor   = element_line(size=0.25,colour=grid.minor,linetype=grid.linetype.minor),
      
      axis.tern.ticks.outside = element_logical(TRUE),
      axis.tern.ticks         = element_line(),
      axis.tern.ticks.major   = element_line(color="black"),
      axis.tern.ticks.major.T = element_line(colour=col.T),
      axis.tern.ticks.major.L = element_line(colour=col.L),
      axis.tern.ticks.major.R = element_line(colour=col.R),
      
      axis.tern.ticks.minor   = element_line(size=0.25),
      axis.tern.ticks.minor.T = element_line(colour=col.T),
      axis.tern.ticks.minor.L = element_line(colour=col.L),
      axis.tern.ticks.minor.R = element_line(colour=col.R)
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
  if(!inherits(get_last_coord(),"ternary")){return(ggplot2::theme_gray(base_size=base_size,base_family=base_family))}
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="grey90",col.T="black",col.L="black",col.R="black")
}
theme_tern_gray <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_gray has been superceded by the ggplot2 standard theme_gray")
  theme_gray(base_size,base_family)
}
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
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="gray90",
              col.T="darkred",col.L="darkblue",col.R="darkgreen")
}
theme_tern_rgbg <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_rgbg has been superceded by theme_rgbg")
  theme_rgbg(base_size,base_family)
}
theme_rgb <- theme_rgbg

#' #' A theme with white background, red, green and blue axes and gidlines
#' 
#' \code{theme_rgbw} is a theme with white background, red, green and blue axes and gidlines
#' @rdname theme_rgbw
#' @inheritParams theme_gray
#' @aliases theme_tern_rgbw
#' @seealso \code{\link{theme_rgbg}}
#' @export
theme_rgbw  <- function(base_size = 12, base_family = ""){
  .theme_tern(base_size=base_size, base_family=base_family, col.BG="white",
              col.T="darkred",col.L="darkblue",col.R="darkgreen",
              grid.T ="darkred",grid.L="darkblue",grid.R="darkgreen",
              grid.minor ="gray90",grid.linetype=6,grid.linetype.minor=1,grid.major.size=0.25)
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
  if(!inherits(get_last_coord(),"ternary")){return(ggplot2::theme_bw(base_size=base_size,base_family=base_family))}
  .theme_tern(base_size=base_size, base_family=base_family,base_ggplot2_theme="theme_bw",
              col.BG=NA,
              col.T ="black",
              col.L ="black",
              col.R ="black",
              grid.T="black",
              grid.L="black",
              grid.R="black",
              grid.minor="gray90",
              grid.linetype.minor=1,
              grid.linetype=6,
              grid.major.size=0.25)
}
theme_tern_bw <- function(base_size = 12, base_family = ""){
  tern_dep("1.0.1.3","theme_tern_bw has been superceded by the ggplot2 standard theme_bw")
  theme_bw(base_size,base_family)
}

