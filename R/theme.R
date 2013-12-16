#' New Theme Elements
#' 
#' \code{ggtern} creates many new theme elements and inheritances, the following is an outline:
#'
#' Theme elements can inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title}, 
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' Modifying the newly created items requires the same procedures as introduced in the ggplot2 \code{\link[ggplot2]{theme}} documentation.
#' Some convenience functions have been also newly created, proceed to \code{\link{convenience}} for additional information.
#'
#' @aliases themeelements elements newelements theme-tern newthemes newtheme
#' @aliases theme theme-tern newthemes newtheme
#' @aliases ternary.options axis.tern axis.tern.arrow axis.tern.arrow.T axis.tern.arrow.L axis.tern.arrow.R panel.background.tern axis.tern.line axis.tern.line.T 
#' axis.tern.line.L axis.tern.line.R axis.tern.text axis.tern.text.T axis.tern.text.L axis.tern.text.R axis.tern.arrow.text axis.tern.arrow.text.T axis.tern.arrow.text.L
#' axis.tern.arrow.text.R axis.tern.title axis.tern.title.T axis.tern.title.L axis.tern.title.R axis.tern.ticks axis.tern.ticks.major axis.tern.ticks.major.T axis.tern.ticks.major.L
#' axis.tern.ticks.major.R axis.tern.ticks.minor axis.tern.ticks.minor.T axis.tern.ticks.minor.L axis.tern.ticks.minor.R panel.grid.tern panel.grid.tern.major
#' panel.grid.tern.major.T panel.grid.tern.major.L panel.grid.tern.major.R panel.grid.tern.minor panel.grid.tern.minor.T panel.grid.tern.minor.L panel.grid.tern.minor.R
#' @name themeelements
#' @name theme
#' @section New/Additional Inheritance Structures:
#' Based on the \code{ggplot2} existing structure (\code{\link[ggplot2]{theme}}), the new individual theme elements for the ternary plot are as follows:
#' 
#' \tabular{llll}{
#'   \strong{NAME}            \tab \strong{DESCRIPTION}                          \tab \strong{OBJECT}               \tab \strong{INHERITS}      \cr
#'   \code{ternary.options}   \tab Ternary specific options                      \tab \code{\link{element_ternary}} \tab                        \cr
#'   \code{panel.background.tern**}    \tab Background of Ternary Plot Area        \tab \code{\link{element_rect}}    \tab \code{rect}  \cr
#'   \code{axis.tern}         \tab Base line for ggtern object                   \tab \code{\link{element_line}}    \tab \code{line}            \cr
#'   \code{axis.tern.arrow}   \tab Base line for ternary arrows                  \tab \code{\link{element_line}}    \tab \code{axis.tern}       \cr
#'   \code{axis.tern.arrow.T} \tab Specific line for TOP ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.arrow.L} \tab Specific line for LHS ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.arrow.R} \tab Specific line for RHS ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.line}    \tab Base line for ternary axes                    \tab \code{\link{element_line}}    \tab \code{axis.tern}       \cr
#'   \code{axis.tern.line.T}  \tab Specific line for TOP ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.line.L}  \tab Specific line for LHS ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.line.R}  \tab Specific line for RHS ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.text}    \tab Base text for ggtern object                   \tab \code{\link{element_text}}    \tab \code{text}            \cr
#'   \code{axis.tern.text.T}  \tab Specific text for TOP species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.text.L}  \tab Specific text for LHS species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.text.R}  \tab Specific text for RHS species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.arrow.text}    \tab Base text for arrow labels              \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.arrow.text.T}  \tab Specific text for TOP arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.arrow.text.L}  \tab Specific text for LHS arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.arrow.text.R}  \tab Specific text for RHS arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.title}    \tab Base text for Apex Labels                    \tab \code{\link{element_text}}    \tab \code{axis.tern.text}   \cr
#'   \code{axis.tern.title.T}  \tab Specific text for TOP Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   \code{axis.tern.title.L}  \tab Specific text for LHS Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   \code{axis.tern.title.R}  \tab Specific text for RHS Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   
#'   \code{axis.tern.ticks}          \tab Base ticks for ggtern object           \tab \code{\link{element_line}}    \tab \code{axis.tern}   \cr
#'   \code{axis.tern.ticks.major}    \tab Base Major ticks for ggtern object     \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks}   \cr
#'   \code{axis.tern.ticks.major.T}  \tab Major ticks for TOP Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.title.major.L}  \tab Major ticks for LHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.title.major.R}  \tab Major ticks for RHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.ticks.minor}    \tab Base Minor ticks for ggtern object     \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks}   \cr
#'   \code{axis.tern.ticks.minor.T}  \tab Minor ticks for TOP Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr
#'   \code{axis.tern.title.minor.L}  \tab Minor ticks for LHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr
#'   \code{axis.tern.title.minor.R}  \tab Minor ticks for RHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr

#'   \code{panel.grid.tern}          \tab Base gridline for ggtern object        \tab \code{\link{element_line}}    \tab \code{axis.tern}   \cr
#'   \code{panel.grid.tern.major}    \tab Base major gridline                    \tab \code{\link{element_line}}    \tab \code{panel.grid.tern}   \cr
#'   \code{panel.grid.tern.major.T}  \tab Major gridline for TOP Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.major.L}  \tab Major gridline for LHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.major.R}  \tab Major gridline for RHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.minor}    \tab Base major gridline                    \tab \code{\link{element_line}}    \tab \code{panel.grid.tern}   \cr
#'   \code{panel.grid.tern.minor.T}  \tab Minor gridline for TOP Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#'   \code{panel.grid.tern.minor.L}  \tab Minor gridline for LHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#'   \code{panel.grid.tern.minor.R}  \tab Minor gridline for RHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#' }
#' ** \strong{NB:} For \code{panel.background.tern}, whilst the ternary area is 'triangular' per-se, \code{\link{element_rect}} has been used, 
#' as it actually holds NO information regarding the geometry (width, height), only fill, color, 
#' size and linetype border (ie the style of how it will be rendered).
#' @rdname terntheme
NULL

#SEARCH FOR THE ORIGINAL FUNCTIONS
ggint$.element_tree <- find_global(".element_tree")
.el_def       <- find_global("el_def")

#TERNARY OPTIONS.
ggint$.element_tree$ternary.options = .el_def("element_ternary","element_ternary") #

##TERNARY PANEL
ggint$.element_tree$panel.background.tern <- .el_def("element_rect", "rect")

##AXIS ARROWS
ggint$.element_tree$axis.tern         = .el_def("element_line", "line") #
ggint$.element_tree$axis.tern.arrow   = .el_def("element_line", "axis.tern") #
ggint$.element_tree$axis.tern.arrow.T = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.arrow.L = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.arrow.R = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.line    = .el_def("element_line", "axis.tern") #
ggint$.element_tree$axis.tern.line.T  = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.line.L  = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.line.R  = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.text    = .el_def("element_text", "text") #
ggint$.element_tree$axis.tern.text.T  = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.text.L  = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.text.R  = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.arrow.text     = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.arrow.text.T   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.arrow.text.L   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.arrow.text.R   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.title          = .el_def("element_text", "axis.tern.text")#
ggint$.element_tree$axis.tern.title.T        = .el_def("element_text", "axis.tern.title")#
ggint$.element_tree$axis.tern.title.L        = .el_def("element_text", "axis.tern.title")#
ggint$.element_tree$axis.tern.title.R        = .el_def("element_text", "axis.tern.title")#

ggint$.element_tree$axis.tern.ticks          = .el_def("element_line", "axis.tern")#
ggint$.element_tree$axis.tern.ticks.major    = .el_def("element_line", "axis.tern.ticks")#
ggint$.element_tree$axis.tern.ticks.major.T  = .el_def("element_line", "axis.tern.ticks.major")#
ggint$.element_tree$axis.tern.ticks.major.L  = .el_def("element_line", "axis.tern.ticks.major")#
ggint$.element_tree$axis.tern.ticks.major.R  = .el_def("element_line", "axis.tern.ticks.major")#

ggint$.element_tree$axis.tern.ticks.minor    = .el_def("element_line", "axis.tern.ticks")#
ggint$.element_tree$axis.tern.ticks.minor.T  = .el_def("element_line", "axis.tern.ticks.minor")#
ggint$.element_tree$axis.tern.ticks.minor.L  = .el_def("element_line", "axis.tern.ticks.minor")#
ggint$.element_tree$axis.tern.ticks.minor.R  = .el_def("element_line", "axis.tern.ticks.minor")#

ggint$.element_tree$panel.grid.tern          = .el_def("element_line", "axis.tern") #
ggint$.element_tree$panel.grid.tern.major    = .el_def("element_line", "panel.grid.tern") #
ggint$.element_tree$panel.grid.tern.major.T  = .el_def("element_line", "panel.grid.tern.major") #
ggint$.element_tree$panel.grid.tern.major.L  = .el_def("element_line", "panel.grid.tern.major") #
ggint$.element_tree$panel.grid.tern.major.R  = .el_def("element_line", "panel.grid.tern.major") #

ggint$.element_tree$panel.grid.tern.minor    = .el_def("element_line", "panel.grid.tern") #
ggint$.element_tree$panel.grid.tern.minor.T  = .el_def("element_line", "panel.grid.tern.minor") #
ggint$.element_tree$panel.grid.tern.minor.L  = .el_def("element_line", "panel.grid.tern.minor") #
ggint$.element_tree$panel.grid.tern.minor.R  = .el_def("element_line", "panel.grid.tern.minor") #


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
.theme_tern      <- function(col.BG="grey90",col.T="darkred",col.L="darkgreen",col.R="darkblue"){
  
  #THE BASE THEME
  base <- theme_gray() #like ggplot2, starts from gray theme.
  
  #TEXT SIZES
  size.base      <- 8
  size.text      <- 10
  size.title     <- 12
  
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
  
  base$axis.tern.text          = element_text(size=size.base,face="plain")
  base$axis.tern.text.T        = element_text(colour=col.T,vjust=0.5,hjust=-0.2,angle =0)
  base$axis.tern.text.L        = element_text(colour=col.L,vjust=0.5,hjust= 1.2,angle =0)
  base$axis.tern.text.R        = element_text(colour=col.R,vjust=0.5,hjust= 1.2,angle =0)
  
  base$axis.tern.arrow.text    = element_text(size=size.text,hjust=0.5)
  base$axis.tern.arrow.text.T  = element_text(colour=col.T, vjust=-0.2, angle =0)
  base$axis.tern.arrow.text.L  = element_text(colour=col.L, vjust=-0.2, angle =0)
  base$axis.tern.arrow.text.R  = element_text(colour=col.R, vjust= 1.2, angle =0)
  
  base$axis.tern.title         = element_text(size  =size.title, angle=0,face="bold",hjust=0.5 ,vjust=0.5)
  base$axis.tern.title.T       = element_text(colour=col.T,vjust= 0.0)
  base$axis.tern.title.L       = element_text(colour=col.L,hjust= 1.0)
  base$axis.tern.title.R       = element_text(colour=col.R,hjust= 0.0)
  
  base$panel.grid.tern         = element_line(size=0.25,colour="black")
  base$panel.grid.tern.major   = element_line(linetype="longdash")
  base$panel.grid.tern.major.T = element_line(colour=col.T)
  base$panel.grid.tern.major.L = element_line(colour=col.L)
  base$panel.grid.tern.major.R = element_line(colour=col.R)
  base$panel.grid.tern.minor   = element_line(size=0.10, linetype="dotted",colour="black")
  
  base$axis.tern.ticks         = element_line(size=0.25)
  base$axis.tern.ticks.major   = element_line()
  base$axis.tern.ticks.major.T = element_line(colour=col.T)
  base$axis.tern.ticks.major.L = element_line(colour=col.L)
  base$axis.tern.ticks.major.R = element_line(colour=col.R)
  base$axis.tern.ticks.minor   = element_line(size=0.10,colour="black")
  
  base
}


