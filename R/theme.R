#' New Theme Elements
#' 
#' 
#' Use this function to modify theme settings.
#'
#' Theme elements can inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title}, 
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' For more examples of modifying properties using inheritance, see
#' \code{\link{+.gg}} and \code{\link{\%+replace\%}}.
#'
#' To see a graphical representation of the inheritance tree, see the
#' last example below.
#'
#' @aliases theme theme-tern
#' @name theme
#' @section New/Additional Inheritance Structures:
#' Based on the \code{ggplot2} existing structure (\code{\link[ggplot2]{theme}}), 
#' The new individual theme elements for the ternary plot are as follows:
#' 
#' \tabular{lll}{
#'   \strong{NAME}   \tab \strong{DESCRIPTION}       \tab \strong{(INHERITS)}      \cr
#'   ternary.options \tab ternary specific options   \tab (\code{element_ternary}) \cr
#'   axis.tern       \tab base line for ternary plot \tab (\code{element_line})    \cr
#' }
#' @rdname terntheme
NULL


.element_tree <- ggplot2:::.element_tree #DUPLICATE

#TERNARY OPTIONS.
.element_tree$ternary.options = ggplot2:::el_def("element_ternary","")

#.element_tree$ternary.options.clockwise  = ggplot2:::el_def("logical","")

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


#' Ternary Theme Elements
#' 
#' @section Clearing the Usual Cartesian Elements:
#' \code{theme_nocart} is a function that returns empty theme elemens for the standard cartesian memebers, including
#' panel.background, panel.border, panel.grid.major, panel.grid.minor, axis.ticks, axis.text.x, axis.text.y, axis.title.x, 
#' axis.title.y are all set to blank
#' @rdname terntheme
#' @export
theme_nocart <- function(){
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



.theme_arrows <- function(show){
  if(!is.logical(show)){show=TRUE}
  show=show[1]
  current <- theme_get()
  e <- current$ternary.options
  if(inherits(e,"element_ternary")){
    e$showarrows <- show
    return(theme(ternary.options=e))
  }else{
    theme(ternary.options=element_ternary(showarrows=show))
  }
}

#' Ternary Theme Elements
#' 
#' \code{theme_noarrows} is a function that apepnds to the current theme a flag to switch OFF the ternary arrows
#' @rdname terntheme
#' @export
theme_noarrows   <- function(){.theme_arrows(FALSE)}

#' Ternary Theme Elements
#' 
#' \code{theme_showarrows} is a function that apepnds to the current theme a flag to switch ON the ternary arrows
#' @rdname terntheme
#' @export
theme_showarrows <- function(){.theme_arrows(TRUE)}

#helper function
.theme_tern      <- function(col.BG="grey90",col.T="darkred",col.L="darkgreen",col.R="darkblue"){
  
  #THE BASE THEME
  base <- theme_gray()
  
  #TEXT SIZES
  size.title     <- 12
  size.text      <- 10
  size.ticklabels<- 8
  
  #NO CARTESIAN
  base$panel.background        = element_blank()
  base$panel.border            = element_blank()
  base$panel.grid.major        = element_blank() 
  base$panel.grid.minor        = element_blank() 
  base$axis.ticks              = element_blank() 
  base$axis.text.x             = element_blank() 
  base$axis.text.y             = element_blank()
  base$axis.title.x            = element_blank() 
  base$axis.title.y            = element_blank()
  
  base$legend.background       = element_blank()
  base$ternary.options         = element_ternary()
  base$panel.background.tern   = element_rect(fill=col.BG,color=NA)
  base$axis.tern               = element_line(size=0.5,linetype="solid")
  base$axis.tern.line          = element_line()
  base$axis.tern.line.T        = element_line(colour=col.T)
  base$axis.tern.line.L        = element_line(colour=col.L)
  base$axis.tern.line.R        = element_line(colour=col.R)
    
  base$axis.tern.arrow         = element_line(lineend=arrow(length=unit(2.5,"mm")))
  base$axis.tern.arrow.T       = element_line(colour=col.T)
  base$axis.tern.arrow.L       = element_line(colour=col.L)
  base$axis.tern.arrow.R       = element_line(colour=col.R)
    
  base$axis.tern.text          = element_text(size=size.ticklabels,face="plain")
  base$axis.tern.text.T        = element_text(colour=col.T,vjust=0.5,hjust=-0.2,angle =0)
  base$axis.tern.text.L        = element_text(colour=col.L,vjust=0.5,hjust=1.2, angle =0)
  base$axis.tern.text.R        = element_text(colour=col.R,vjust=0.5,hjust=1.2, angle =0)
    
  base$axis.tern.arrow.text    = element_text(size=size.text,hjust=0.5)
  base$axis.tern.arrow.text.T  = element_text(colour=col.T, vjust=-0.2,angle =0)
  base$axis.tern.arrow.text.L  = element_text(colour=col.L, vjust=-0.2,  angle =0)
  base$axis.tern.arrow.text.R  = element_text(colour=col.R, vjust=1.2,  angle =0)
    
  base$axis.tern.title         = element_text(size  =size.title, angle=0,face="bold",hjust=0.5 ,vjust=0.5)
  base$axis.tern.title.T       = element_text(colour=col.T,           vjust=-0.5)
  base$axis.tern.title.L       = element_text(colour=col.L,hjust= 1.4           )
  base$axis.tern.title.R       = element_text(colour=col.R,hjust=-0.4           )
    
  base$panel.grid.tern         = element_line(colour="black")
  base$panel.grid.tern.major   = element_line(size=0.25, linetype="longdash")
  base$panel.grid.tern.major.T = element_line(colour=col.T)
  base$panel.grid.tern.major.L = element_line(colour=col.L)
  base$panel.grid.tern.major.R = element_line(colour=col.R)
  base$panel.grid.tern.minor   = element_line(size=0.10, linetype="dotted",colour="black")
      
  base$axis.tern.ticks.major   = element_line(size=0.25)
  base$axis.tern.ticks.major.T = element_line(colour=col.T)
  base$axis.tern.ticks.major.L = element_line(colour=col.L)
  base$axis.tern.ticks.major.R = element_line(colour=col.R)
  base$axis.tern.ticks.minor   = element_line(size=0.10,colour="black")
  
  base
}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_rgbg} ternary theme, red green blue with gray background
#' @rdname terntheme
#' @export
theme_tern_rgbg  <- function(){.theme_tern(col.BG="gray90")}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_rgbw} ternary theme, red green blue with white background
#' @rdname terntheme
#' @export
theme_tern_rgbw  <- function(){.theme_tern(col.BG="white")}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_bw} ternary theme black and white
#' @rdname terntheme
#' @export
theme_tern_bw    <- function(){.theme_tern("white","black","black","black")}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_gray} ternary theme, gray theme
#' @rdname terntheme
#' @export
theme_tern_gray  <- function(){.theme_tern(col.BG="grey90",col.T="black",col.L="black",col.R="black")}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_nogrid} ternary theme, no minor grids.
#' @rdname terntheme
#' @export
theme_tern_nogrid_minor <- function(){
  theme(panel.grid.tern.minor=element_blank(),
        panel.grid.tern.minor.T=element_blank(),
        panel.grid.tern.minor.L=element_blank(),
        panel.grid.tern.minor.R=element_blank()
  ) 
}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_nogrid} ternary theme, no major grids.
#' @rdname terntheme
#' @export
theme_tern_nogrid_major <- function(){
    theme(panel.grid.tern.major=element_blank(),
          panel.grid.tern.major.T=element_blank(),
          panel.grid.tern.major.L=element_blank(),
          panel.grid.tern.major.R=element_blank()
    ) 
}

#' Ternary Theme Elements
#' 
#' \code{theme_tern_nogrid} ternary theme, no major or minor grids.
#' @rdname terntheme
#' @export
theme_tern_nogrid <- function(){
  list(
    theme_tern_nogrid_minor(),
    theme_tern_nogrid_major()
  )
}

.theme_new <- (function() {
  theme.tern <- theme_tern_gray()
  theme      <- theme_gray()
  list(
    get = function(){
      ifthenelse(inherits(last_plot(),"ggtern"),theme.tern,theme)
    },
    set = function(new) {
      ifthenelse(inherits(last_plot(),"ggtern"),{
        missing <- setdiff(names(theme_tern_gray()), names(new))
        if (length(missing) > 0) {
          warning("New theme missing the following elements: ",paste(missing, collapse = ", "), call. = FALSE)
        }
        old <- theme.tern
        theme.tern <<- new
        invisible(old)
      },{
        missing <- setdiff(names(theme_gray()), names(new))
        if (length(missing) > 0) {
          warning("New theme missing the following elements: ",
                  paste(missing, collapse = ", "), call. = FALSE)
        }
        old <- theme
        theme <<- new
        invisible(old)
      })
    }
  )
})()
.theme_get <- .theme_new$get
.theme_set <- .theme_new$set







