\name{ifthenelse}
\alias{arrow.label.formatter}
\alias{calc_element_plot}
\alias{geomvaraes}
\alias{get_tern_extremes}
\alias{ifthenelse}
\title{ggtern Utilities}
\usage{
  ifthenelse(x, a, b)

  geomvaraes(ternreqaes, ggplotreqaes)

  get_tern_extremes(plot, coordinates = plot$coordinates,
    verbose = F)

  arrow.label.formatter(label, suffix = "", ..., sep = "/")

  calc_element_plot(element, theme = theme_update(), ...,
    plot = NULL, verbose = F)
}
\arguments{
  \item{x}{logical tern}

  \item{a}{value to return when true}

  \item{b}{value to return when false}

  \item{ternreqaes}{first argument}

  \item{ggplotreqaes}{second argument}

  \item{plot}{ggtern object}

  \item{coordinates}{ggtern coordinate system, inheriting
  "ternary" and "coord" classes.}

  \item{verbose}{logical indicating verbose reporting to
  console}

  \item{label}{character label}

  \item{suffix}{chacater suffix behind each label}

  \item{sep}{the seperator between label and suffix}

  \item{element}{the element name to calculate}

  \item{theme}{the theme to inherit from}

  \item{...}{not used}
}
\value{
  data.frame representing the T, L and R amounts (Columns)
  at each of the tips (extremes) of the ternary plot area
  (Rows)
}
\description{
  \code{ifthenelse} function takes input arguments
  \code{x}, \code{a} and \code{b} and returns \code{a} when
  \code{x} is \code{TRUE} otherwise \code{b} (when \code{x}
  is \code{FALSE})

  returns first argument if ternary and second if not. Used
  for creating variable required aes mappings when hacking
  ggplot2 for ternary plots.

  \code{get_tern_extremes} determines the limiting ternary
  coordinates given input plot or coordinates. In the event
  that the

  \code{arrow.label.formatter} is a function that formats
  the labels directly adjacent to the axes on a ternary
  plot.

  \code{calc_element_plot} Calculate the element
  properties, by inheriting properties from its parents,
  and compares to whether the local plot overrides this
  value. Based largely off the
  \code{\link[ggplot2]{calc_element}} as provided in
  \code{\link{ggplot2}}
}
\examples{
ifthenelse(TRUE,1,0)
get_tern_extremes(coordinates = coord_tern())
arrow.label.formatter("TOP","Wt.\%",sep="/")
}
\seealso{
  \code{\link[ggplot2]{calc_element}}
}

