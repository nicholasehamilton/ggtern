
#'Ternary Diagrams in R
#'
#'Ternary diagrams are used frequently in materials science to graph compositional features for mixtures of three different elements.
#'It is based (extends) the very popular \code{\link{ggplot}} package, which is an implementation of Wilkinsons "The Grammar of Graphics".
#'
#'\tabular{ll}{
#' Package: \tab ggtern\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2013-11-15\cr
#' License: \tab GPL-2\cr
#' }
#' 
#'ggplot2, using the \code{\link{grid}} and \code{\link{proto}} architectures, makes provision for a many number of geometry types to be added 
#'progressively in \emph{'layers'} to a given base plot.
#'
#'In this version 1.0 (the first release of this package), some of the geometries which are available in ggplot2, are not relevant to 
#'ternary plots, and a limited number of 'approved' geometries can be used, including:
#'\enumerate{
#'  \item point
#'  \item path
#'  \item segment
#'  \item polygon
#'  \item smooth
#'  \item text
#'  \item density2d
#'  \item rug
#'}
#'
#'If a layer is added which is not in the above list, \strong{it will be stripped from the ternary diagram}, when rendered, notifying the user 
#'to such effect. This is because, for the approved list, subtle 'patches' were required, mainly to do with 
#'the transformation procedures when incorporating a third dimention. In the future, others may be made available once patched.
#'
#'Required aesthetics have been changed, where \code{x} and \code{y} were previously required, now \code{x}, 
#'\code{y} and \code{z} are required, and, this is made possible without affecting the standard ggplot2 behaviour because this package distinuishes between ggplot and 
#'ggtern objects (ggter inherits ggplot)
#'
#'\code{ggtern} is a method which is a convenience wrapper for \code{ggplot(...) + coord_tern()}
#'@author Written and Maintained by Nicholas Hamilton <n.hamilton@@student.unsw.edu.au>
#'
#'@section Main Constructor Options:
#'\code{ggtern} is a function, analogous to \code{ggplot}, which creates an object of class \code{ggtern} (which inherits \code{ggplot}).
#'@param ... same arguments as passed through to ggplot
#'@inheritParams ggplot2::ggplot
#'@usage ggtern(...)
#'@example /example/mainexamples.R
#'@export
ggtern <- function(...){ggplot(...) + coord_tern()}







