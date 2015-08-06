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
#' @section New/Additional Inheritance Structures:
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_theme()}
#' **  \strong{NB:} \code{panel.background.tern}, whilst the ternary area is 'triangular' per-se, \code{\link{element_rect}} has been used, 
#' as it actually holds NO information regarding the geometry (width, height), only fill, color, 
#' size and linetype border (ie the style of how it will be rendered).
#' *** \strong{NB:} \code{ternary.options} has been fully Depreciated since \code{ggtern} version 1.0.1.3
#' @aliases themeelements elements newelements theme-tern newthemes newtheme theme 
#' panel.background.tern axis.tern.clockwise axis.tern.padding axis.tern 
#' axis.tern.hshift axis.tern.vshift axis.tern.line axis.tern.line.T axis.tern.line.L 
#' axis.tern.line.R axis.tern.text axis.tern.text.T axis.tern.text.L axis.tern.text.R 
#' axis.tern.showarrows axis.tern.arrowsep axis.tern.arrowstart axis.tern.arrowfinish 
#' axis.tern.arrow axis.tern.arrow.T axis.tern.arrow.L axis.tern.arrow.R axis.tern.arrow.text 
#' axis.tern.arrow.text.T axis.tern.arrow.text.L axis.tern.arrow.text.R axis.tern.showtitles
#' axis.tern.title axis.tern.title.T axis.tern.title.L axis.tern.title.R axis.tern.ticklength.major 
#' axis.tern.ticklength.minor axis.tern.ticks.outside axis.tern.ticks.showprimary axis.tern.ticks.showsecondary 
#' axis.tern.ticks axis.tern.ticks.major axis.tern.ticks.major.T axis.tern.ticks.major.L axis.tern.ticks.major.R 
#' axis.tern.ticks.minor axis.tern.ticks.minor.T axis.tern.ticks.minor.L axis.tern.ticks.minor.R axis.tern.showgrid.major 
#' panel.grid.tern panel.grid.tern.major panel.grid.tern.major.T panel.grid.tern.major.L panel.grid.tern.major.R 
#' axis.tern.showgrid.minor panel.grid.tern.minor 
#' panel.grid.tern.minor.T panel.grid.tern.minor.L panel.grid.tern.minor.R ternary.options
#' panel.margin.tern
#' axis.tern.showlabels
#' @name theme_elements
#' @rdname theme_elements
NULL


#SEARCH FOR THE ORIGINAL FUNCTIONS
ggint$.element_tree      <- find_global_tern(".element_tree")
ggint$.element_tree.orig <- ggint$.element_tree #To determine the new set relative to the existing.
.el_def                  <- ggint$el_def

##TERNARY PANEL
ggint$.element_tree$panel.background.tern    = .el_def("element_rect", "rect",                 description="Background of Ternary Plot Area**")

##Clockwise Precession.
ggint$.element_tree$axis.tern.clockwise      = .el_def("logical",                              description="Clockwise or Anticlockwise Precession")
ggint$.element_tree$axis.tern.padding        = .el_def("unit",                                 description="Padding between ternary plot and perimeter")

##AXIS ARROWS
ggint$.element_tree$axis.tern                = .el_def("element_line", "line",                 description="Base line for ggtern object") #
ggint$.element_tree$axis.tern.hshift         = .el_def("unit",                                 description="Amount to nudge the plot horizontally") #
ggint$.element_tree$axis.tern.vshift         = .el_def("unit",                                 description="Amount to nudge the plot vertically") #

ggint$.element_tree$axis.tern.line           = .el_def("element_line", "axis.tern",            description="Base Line") #
ggint$.element_tree$axis.tern.line.T         = .el_def("element_line", "axis.tern.line",       description="Line for TOP Axis") #
ggint$.element_tree$axis.tern.line.L         = .el_def("element_line", "axis.tern.line",       description="Line for LHS Axis") #
ggint$.element_tree$axis.tern.line.R         = .el_def("element_line", "axis.tern.line",       description="LIne for RHS Axis") #

ggint$.element_tree$axis.tern.showlabels     = .el_def("logical",                              description="Axis Labels Show or Hide")
ggint$.element_tree$axis.tern.text           = .el_def("element_text", "text",                 description="Base Text") #
ggint$.element_tree$axis.tern.text.T         = .el_def("element_text", "axis.tern.text",       description="Text for TOP Axis") #
ggint$.element_tree$axis.tern.text.L         = .el_def("element_text", "axis.tern.text",       description="Text for LHS Axis") #
ggint$.element_tree$axis.tern.text.R         = .el_def("element_text", "axis.tern.text",       description="Text for RHS Axis") #

ggint$.element_tree$axis.tern.showarrows     = .el_def("logical",                              description="Arrows Show or Hide")
ggint$.element_tree$axis.tern.arrowbaseline  = .el_def("numeric",                              description="Arrow Origin 0=Axis,1=Ticks,2=Labels")
ggint$.element_tree$axis.tern.arrowsep       = .el_def("unit",                                 description="Arrows Seperation from Axis")
ggint$.element_tree$axis.tern.arrowstart    = .el_def("numeric",                               description="Proportion of Axis when Arrow Starts")
ggint$.element_tree$axis.tern.arrowfinish   = .el_def("numeric",                               description="Proportion of Axis when Arrow Finishes")
ggint$.element_tree$axis.tern.arrow          = .el_def("element_line", "axis.tern",            description="Base Arrow Line") #
ggint$.element_tree$axis.tern.arrow.T        = .el_def("element_line", "axis.tern.arrow",      description="Arrow Line for TOP Axis") #
ggint$.element_tree$axis.tern.arrow.L        = .el_def("element_line", "axis.tern.arrow",      description="Arrow Line for LHS Axis") #
ggint$.element_tree$axis.tern.arrow.R        = .el_def("element_line", "axis.tern.arrow",      description="Arrow Line for RHS Axis") #
ggint$.element_tree$axis.tern.arrow.text     = .el_def("element_text", "axis.tern.text",       description="Base Arrow Label") #
ggint$.element_tree$axis.tern.arrow.text.T   = .el_def("element_text", "axis.tern.arrow.text", description="Arrow Label on TOP Axis") #
ggint$.element_tree$axis.tern.arrow.text.L   = .el_def("element_text", "axis.tern.arrow.text", description="Arrow Label on LHS Axis") #
ggint$.element_tree$axis.tern.arrow.text.R   = .el_def("element_text", "axis.tern.arrow.text", description="Arrow Label on RHS Axis") #

ggint$.element_tree$axis.tern.showtitles     = .el_def("logical",                              description="Apex Titles Show or Hide")
ggint$.element_tree$axis.tern.title          = .el_def("element_text", "axis.tern.text",       description="Base Apex Title") #
ggint$.element_tree$axis.tern.title.T        = .el_def("element_text", "axis.tern.title",      description="Apex Title for TOP Axis") #
ggint$.element_tree$axis.tern.title.L        = .el_def("element_text", "axis.tern.title",      description="Apex Title for LHS Axis") #
ggint$.element_tree$axis.tern.title.R        = .el_def("element_text", "axis.tern.title",      description="Apex Title for RHS Axis") #

ggint$.element_tree$axis.tern.ticklength.major    = .el_def("unit",                            description="Ticks Major Ticklength")
ggint$.element_tree$axis.tern.ticklength.minor    = .el_def("unit",                            description="Ticks Minor Ticklength")
ggint$.element_tree$axis.tern.ticks.outside  = .el_def("logical",                              description="Ticks Outside or Inside") #
ggint$.element_tree$axis.tern.ticks.showprimary   = .el_def("logical",                         description="Ticks Show Primary")
ggint$.element_tree$axis.tern.ticks.showsecondary = .el_def("logical",                         description="Ticks Show Secondary")
ggint$.element_tree$axis.tern.ticks          = .el_def("element_line", "axis.tern",            description="Base Ticks") #
ggint$.element_tree$axis.tern.ticks.major    = .el_def("element_line", "axis.tern.ticks",      description="Base Major Ticks") #
ggint$.element_tree$axis.tern.ticks.major.T  = .el_def("element_line", "axis.tern.ticks.major",description="Base Major Ticks for TOP Axis") #
ggint$.element_tree$axis.tern.ticks.major.L  = .el_def("element_line", "axis.tern.ticks.major",description="Base Major Ticks for LHS Axis") #
ggint$.element_tree$axis.tern.ticks.major.R  = .el_def("element_line", "axis.tern.ticks.major",description="Base Major Ticks for RHS Axis") #

ggint$.element_tree$axis.tern.ticks.minor    = .el_def("element_line", "axis.tern.ticks",      description="Base Minor Ticks") #
ggint$.element_tree$axis.tern.ticks.minor.T  = .el_def("element_line", "axis.tern.ticks.minor",description="Base Minor Ticks for TOP Axis") #
ggint$.element_tree$axis.tern.ticks.minor.L  = .el_def("element_line", "axis.tern.ticks.minor",description="Base Minor Ticks for LHS Axis") #
ggint$.element_tree$axis.tern.ticks.minor.R  = .el_def("element_line", "axis.tern.ticks.minor",description="Base Minor Ticks for RHS Axis") #

ggint$.element_tree$axis.tern.showgrid.major = .el_def("logical",                              description="Show or Hide Major Gridline")
ggint$.element_tree$panel.grid.tern          = .el_def("element_line", "axis.tern",            description="Base Gridline") #
ggint$.element_tree$panel.grid.tern.major    = .el_def("element_line", "panel.grid.tern",      description="Base Major Gridline") #
ggint$.element_tree$panel.grid.tern.major.T  = .el_def("element_line", "panel.grid.tern.major",description="Major Gridline for TOP Axis") #
ggint$.element_tree$panel.grid.tern.major.L  = .el_def("element_line", "panel.grid.tern.major",description="Major Gridline for LHS Axis") #
ggint$.element_tree$panel.grid.tern.major.R  = .el_def("element_line", "panel.grid.tern.major",description="Major Gridline for RHS Axis") #

ggint$.element_tree$axis.tern.showgrid.minor = .el_def("logical",                              description="Show or Hide Minor Gridline")
ggint$.element_tree$panel.grid.tern.minor    = .el_def("element_line", "panel.grid.tern",      description="Base Minor Gridline") #
ggint$.element_tree$panel.grid.tern.minor.T  = .el_def("element_line", "panel.grid.tern.minor",description="Minor Gridline for TOP Axis") #
ggint$.element_tree$panel.grid.tern.minor.L  = .el_def("element_line", "panel.grid.tern.minor",description="Minor Gridline for LHS Axis") #
ggint$.element_tree$panel.grid.tern.minor.R  = .el_def("element_line", "panel.grid.tern.minor",description="Minor Gridline for RHS Axis") #

ggint$.element_tree$panel.margin.tern        = .el_def("unit",'unit'                          ,description="Panel Margin of the Ternary Plot Region")

#TERNARY OPTIONS.
ggint$.element_tree$ternary.options          = .el_def("element_ternary","element_ternary"    ,description="Ternary Specific Options (Depreciated)***") #


