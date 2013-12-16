\docType{data}
\name{ifthenelse}
\alias{Geom}
\alias{GeomDensity2dtern}
\alias{GeomLline}
\alias{GeomRline}
\alias{GeomSmoothtern}
\alias{GeomTline}
\alias{StatDensity2dtern}
\alias{StatLline}
\alias{StatRline}
\alias{StatSmoothtern}
\alias{StatTline}
\alias{Statnew}
\alias{arrow}
\alias{arrow_label_formatter}
\alias{calc_element_plot}
\alias{calculate_stats}
\alias{check_required_aesthetics}
\alias{find_global}
\alias{get_last_coord}
\alias{get_tern_extremes}
\alias{ggint}
\alias{ifthenelse}
\alias{panel_scales}
\alias{remove_outside}
\alias{set_last_coord}
\alias{sink_density}
\alias{train_ranges}
\alias{trytransform}
\alias{undocumented}
\title{Internal Function}
\format{proto object 
 $ draw        :function (., data, scales, coordinates, limitarea = F, ...)   
 $ default_stat:function (.)   
 $ objname     : chr "smoothtern" 
 $ default_aes :function (.)   
 $ guide_geom  :function (.)   
 $ required_aes: chr [1:2] "x" "y" 
 $ draw_legend :function (., data, params, ...)   
 parent: proto object 
 .. parent: proto object}
\usage{
  ifthenelse(x, a, b)

  get_tern_extremes(coordinates, verbose = F, expand = 0)

  arrow_label_formatter(label, suffix = "", ..., sep = "/")

  calc_element_plot(element, theme = theme_update(), ...,
    plot = NULL, verbose = F)

  find_global(name, env = environment())

  trytransform(data, coord)

  remove_outside(data)

  sink_density(df, remove = TRUE,
    coord = stop("coord is required"))

  check_required_aesthetics(required, present, name)

  panel_scales(panel, i)

  calculate_stats(panel, data, layers)

  train_ranges(panel, coord)

  GeomSmoothtern

  GeomTline

  GeomLline

  GeomRline

  GeomDensity2dtern

  StatSmoothtern

  StatDensity2dtern

  StatTline

  StatLline

  StatRline

  get_last_coord()

  set_last_coord(new)
}
\arguments{
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

  \item{data}{the dataset}

  \item{coord}{the coordinates}

  \item{data}{data.frame}

  \item{df}{data.frame}

  \item{remove}{boolean remove or make zero}

  \item{coord}{coordinates}

  \item{character}{vector of required aesthetics}

  \item{character}{vector of present aesthetics}

  \item{name}{of object for error message}

  \item{new}{the coordinate system to set}

  \item{a}{argument a}

  \item{b}{argument b}

  \item{x}{argument x}
}
\value{
  data.frame representing the T, L and R amounts (Columns)
  at each of the tips (extremes) of the ternary plot area
  (Rows)

  Instance of the named object (if it exists), or
  \code{NULL} (if it does not).

  transformed data

  data.frame

  data.frame

  coordinate system
}
\description{
  \code{ifthenelse} function takes input arguments
  \code{x}, \code{a} and \code{b} and returns \code{a}

  \code{get_tern_extremes} determines the limiting ternary
  coordinates given input coordinates.

  \code{arrow_label_formatter} is a function that formats
  the labels directly adjacent to the axes on a ternary
  plot.

  \code{calc_element_plot} Calculate the element
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
  namespaces \emph{(in that order)}.

  \code{trytransform} is an internal function which
  attempts to make ternary transformation. If fails, the
  original data is returned

  Internal Function

  Internal Function

  Check required aesthetics are present This is used by
  geoms and stats to give a more helpful error message when
  required aesthetics are missing.

  Compute ranges and dimensions of each panel, using the
  coord.

  Modified Geom Smooth Proto

  Modified Stat Smooth Proto

  Internal functions to get and set the last coordinate
  system. An internal command for ggtern.

  \code{set_last_coord} set the last coordinate system.

  Internal Function

  Exported instance of the imported grid arrow function
}
\details{
  This is a modified version of the original source as
  provided in \code{ggplot2}, which has the same
  functionality, however, the modification is such that the
  function now additionally searches within the
  \code{ggtern} namespace prior to the \code{ggplot2}
  namespace.

  \code{get_last_coord} is a function that returns the last
  coordinate system used.
}
\examples{
get_tern_extremes(coordinates = coord_tern())
arrow_label_formatter("TOP","Wt.\%",sep="/")
find_global('scale_x_continuous')
}
\seealso{
  \code{\link[ggplot2]{calc_element}}

  \code{\link[grid]{arrow}}
}
\keyword{datasets}
\keyword{internal}

