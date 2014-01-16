\docType{data}
\name{internal}
\alias{.all_aesthetics}
\alias{.internals}
\alias{Geom}
\alias{Statnew}
\alias{arrow}
\alias{arrow_label_formatter}
\alias{calc_element_plot}
\alias{cullAndConstrain}
\alias{find_global}
\alias{get_last_coord}
\alias{get_tern_extremes}
\alias{ggint}
\alias{ifthenelse}
\alias{internal}
\alias{is.numericor}
\alias{remove_outside}
\alias{set_last_coord}
\alias{sink_density}
\alias{undocumented}
\alias{unit}
\title{Internal Functions}
\format{functions and objects}
\usage{
  ggint

  Statnew

  Geom

  ifthenelse(x, a, b)

  is.numericor(a, b)

  get_tern_extremes(coordinates, verbose = F, expand = 0)

  arrow_label_formatter(label, suffix = "", ..., sep = "/")

  calc_element_plot(element, theme = theme_update(), ...,
    plot = NULL, verbose = F)

  find_global(name, env = environment())

  remove_outside(data)

  sink_density(df, remove = TRUE,
    coord = stop("coord is required"))

  get_last_coord()

  set_last_coord(new)

  cullAndConstrain(data)
}
\arguments{
  \item{x}{logical input to check}

  \item{a}{value to return if \code{x} is TRUE}

  \item{b}{value to return if \code{x} is FALSE}

  \item{a}{value to return if \code{a} is numeric}

  \item{b}{value to return if \code{a} is not numeric}

  \item{coordinates}{ggtern coordinate system, inheriting
  "ternary" and "coord" classes.}

  \item{verbose}{logical indicating verbose reporting to
  console}

  \item{expand}{numeric value to}

  \item{label}{character label}

  \item{suffix}{chacater suffix behind each label}

  \item{sep}{the seperator between label and suffix}

  \item{element}{the element name to calculate}

  \item{theme}{the theme to inherit from}

  \item{plot}{the plot to check locally for theme element,
  NULL is ok.}

  \item{...}{not used}

  \item{name}{character name of object to search for}

  \item{env}{environment to search within as first
  priority}

  \item{data}{data.frame}

  \item{df}{data.frame}

  \item{remove}{boolean remove or make zero}

  \item{coord}{coordinates of the type 'ternar', ie
  coord_tern()}

  \item{new}{the coordinate system to set}
}
\value{
  \code{get_tern_extremes} returns data.frame representing
  the T, L and R amounts (Columns) at each of the tips
  (extremes) of the ternary plot area (Rows)

  \code{find_global} returns an instance of the named
  object (if it exists), or \code{NULL} (if it does not).

  \code{get_last_coord} returns the current coordinate
  system
}
\description{
  INTERNAL FUNCTIONS (native to \code{ggtern}):
  \code{ggtern} makes use of several non-exported internal
  functions, a list of the internal functions is as
  follows:

  \code{ggint} is a structure which holds all the internal
  functions from the ggplot2 namespace, which can be used
  conveniently via \code{ggint$ABC}.

  \code{Statnew} is a local copy of the ggplot2 Stat
  top-level proto

  \code{Geom} is a local copy of the ggplot2 Gom top-level
  proto

  \code{ifthenelse} function takes input arguments
  \code{x}, \code{a} and \code{b} and returns \code{a} if
  \code{x} is \code{TRUE}, else, returns \code{b}

  \code{is.numericor} function takes input arguments
  \code{a} and \code{b} and returns \code{a} if \code{a} is
  numeric, else, returns \code{b}

  \code{get_tern_extremes} determines the limiting ternary
  coordinates given input coordinates.

  \code{arrow_label_formatter} is a function that formats
  the labels directly adjacent to the ternary arrows.

  \code{calc_element_plot} Calculates the element
  properties, by inheriting properties from its parents,
  and compares to whether the local plot overrides this
  value. Based largely off the
  \code{\link[ggplot2]{calc_element}} as provided in
  \code{\link{ggplot2}}

  \code{find_global} is a function that conducts a named
  search for the \code{name} object instance, within the
  \code{env} environment. If an instance doesn't exist
  within the \code{env} environment, a search is then
  conducted within the \code{ggtern} and \code{ggplot2}
  namespaces \emph{(in that order)}. This is a modified
  version of the original source as provided in
  \code{ggplot2}, which has the same functionality,
  however, the modification is such that the function now
  additionally searches within the \code{ggtern} namespace
  prior to the \code{ggplot2} namespace.

  \code{remove_outside} is a function that removes, from an
  input datases, all the rows wich are outside the ternary
  plot area

  \code{sink_density} is a function which permits contours
  on the ternary surface, without running over the ternary
  borders.

  \code{get_last_coord} is a function that returns the last
  coordinate system used.

  \code{set_last_coord} set the last coordinate system.

  \code{cullAndConstrain} is a function that is not
  implemented at this point in time, ignore it.

  \code{arrow} is an exported instance of the imported grid
  arrow function. For further information, refer to:
  \link[grid]{arrow}.

  \code{unit} is an exported instance of the imported grid
  unit function. For further information, refer to:
  \link[grid]{unit}.
}
\examples{
get_tern_extremes(coordinates = coord_tern())
arrow_label_formatter("TOP","Wt.\%",sep="/")
find_global('scale_x_continuous')
}
\seealso{
  \code{\link[ggplot2]{calc_element}}
}
\keyword{datasets}

