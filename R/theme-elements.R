#SEARCH FOR THE ORIGINAL FUNCTIONS
ggint$.element_tree <- find_global(".element_tree")
.el_def             <- ggint$el_def

#TERNARY OPTIONS.
ggint$.element_tree$ternary.options          = .el_def("element_ternary","element_ternary") #

##TERNARY PANEL
ggint$.element_tree$panel.background.tern    = .el_def("element_rect", "rect")

##Show the arrows
ggint$.element_tree$axis.tern.showarrows     = .el_def("logical")
ggint$.element_tree$axis.tern.arrowsep       = .el_def("unit")

#Show the titles
ggint$.element_tree$axis.tern.showtitles     = .el_def("logical")

#Show the grids
ggint$.element_tree$axis.tern.showgrid.major = .el_def("logical")
ggint$.element_tree$axis.tern.showgrid.minor = .el_def("logical")

##Secondary ticks
ggint$.element_tree$axis.tern.ticks.showprimary   = .el_def("logical")
ggint$.element_tree$axis.tern.ticks.showsecondary = .el_def("logical")

##Clockwise Precession.
ggint$.element_tree$axis.tern.clockwise      = .el_def("logical")
ggint$.element_tree$axis.tern.padding        = .el_def("unit")

ggint$.element_tree$axis.tern.ticklength.major    = .el_def("unit")
ggint$.element_tree$axis.tern.ticklength.minor    = .el_def("unit")
ggint$.element_tree$axis.tern.arrowstart    = .el_def("numeric")
ggint$.element_tree$axis.tern.arrowfinish   = .el_def("numeric")

##AXIS ARROWS
ggint$.element_tree$axis.tern                = .el_def("element_line", "line") #
ggint$.element_tree$axis.tern.hshift         = .el_def("unit") #
ggint$.element_tree$axis.tern.vshift         = .el_def("unit") #
ggint$.element_tree$axis.tern.arrow          = .el_def("element_line", "axis.tern") #
ggint$.element_tree$axis.tern.arrow.T        = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.arrow.L        = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.arrow.R        = .el_def("element_line", "axis.tern.arrow") #
ggint$.element_tree$axis.tern.line           = .el_def("element_line", "axis.tern") #
ggint$.element_tree$axis.tern.line.T         = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.line.L         = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.line.R         = .el_def("element_line", "axis.tern.line") #
ggint$.element_tree$axis.tern.text           = .el_def("element_text", "text") #
ggint$.element_tree$axis.tern.text.T         = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.text.L         = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.text.R         = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.arrow.text     = .el_def("element_text", "axis.tern.text") #
ggint$.element_tree$axis.tern.arrow.text.T   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.arrow.text.L   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.arrow.text.R   = .el_def("element_text", "axis.tern.arrow.text") #
ggint$.element_tree$axis.tern.title          = .el_def("element_text", "axis.tern.text")#
ggint$.element_tree$axis.tern.title.T        = .el_def("element_text", "axis.tern.title")#
ggint$.element_tree$axis.tern.title.L        = .el_def("element_text", "axis.tern.title")#
ggint$.element_tree$axis.tern.title.R        = .el_def("element_text", "axis.tern.title")#

ggint$.element_tree$axis.tern.ticks.outside  = .el_def("logical")
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


