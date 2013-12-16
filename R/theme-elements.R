#SEARCH FOR THE ORIGINAL FUNCTIONS
ggint$.element_tree <- find_global(".element_tree")
.el_def       <- ggint$el_def

#TERNARY OPTIONS.
ggint$.element_tree$ternary.options          = .el_def("element_ternary","element_ternary") #

##TERNARY PANEL
ggint$.element_tree$panel.background.tern    = .el_def("element_rect", "rect")

##AXIS ARROWS
ggint$.element_tree$axis.tern                = .el_def("element_line", "line") #
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


#' @details \code{validate_element} is a function which checks the validity of a given theme element, against the elements table.
#' Since the \code{.elements_tree} is an internal function, which is not exported, and modifications could not be made, 
#' a new (and equivalent) \code{.element_tree} is created within ggtern.
#' @param el the element
#' @param elname the element name
#' @rdname overloaded
validate_element <- function(el, elname) {
  eldef <- ggint$.element_tree[[elname]]
  
  if (is.null(eldef)) {
    writeLines("testing")
    stop('"', elname, '" is not a valid theme element name...')
  }
  
  # NULL values for elements are OK
  if (is.null(el)) return()
  
  if (eldef$class == "character") {
    # Need to be a bit looser here since sometimes it's a string like "top"
    # but sometimes its a vector like c(0,0)
    if (!is.character(el) && !is.numeric(el))
      stop("Element ", elname, " must be a string or numeric vector.")
    
  } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
    stop("Element ", elname, " must be a ", eldef$class, " object.")
  }
  invisible()
}


