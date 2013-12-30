\name{element_ternary}
\alias{element_ternary}
\title{Theme element: ternary structure}
\usage{
  element_ternary(showarrows, padding, arrowsep,
    arrowstart, arrowfinish, vshift, hshift,
    ticklength.major, ticklength.minor)
}
\arguments{
  \item{showarrows}{logical whether to show the axis
  directional arrows DEPRECIATED}

  \item{padding}{the padding around the plot area to make
  provision for axis labels, ticks and arrows, relative to
  the cartesian plane. DEPRECIATED}

  \item{arrowsep}{the distance between ternary axis and
  ternary arrows DEPRECIATED}

  \item{arrowstart}{the proportion along the ternary axis
  to start the directional arrow DEPRECIATED}

  \item{arrowfinish}{the proportion along the ternary axis
  to stop the directional arrow DEPRECIATED}

  \item{vshift}{shift the plot area vertically DEPRECIATED}

  \item{hshift}{shift the plot area horizontally
  DEPRECIATED}

  \item{ticklength.major}{the length of the major ternary
  ticks as an euclidean distance relative to the x and y
  limits of the cartesian plot area. DEPRECIATED}

  \item{ticklength.minor}{the length of the minor ternary
  ticks as an euclidean distance relative to the x and y
  limits of the cartesian plot area. DEPRECIATED}
}
\description{
  NOTE: This function has been entirely depreciated, and,
  has been replaced by individual theme elements:
  \code{\link{axis.tern.showarrows}}
  \code{\link{axis.tern.padding}}
  \code{\link{axis.tern.arrowsep}}
  \code{\link{axis.tern.arrowstart}}
  \code{\link{axis.tern.arrowfinish}}
  \code{\link{axis.tern.vshift}}
  \code{\link{axis.tern.hshift}}
  \code{\link{axis.tern.ticklength.major}}
  \code{\link{axis.tern.ticklength.minor}}
}
\details{
  Used to define the layout of some of the ggtern plot
  features which are unique to the ternary diagrams , and
  hence, this package.

  Note that some of these items, unlike
  \code{\link{ggplot2}} which permits absolute distances
  (ie say unit(1,"cm")) via the \code{\link{grid}} package,
  are instead defined as a relative length to the limits
  imposed by the underlying \code{x} and \code{y} axes.

  For example, the ternary axes are rendered directly onto
  the plot area, (rather than using an exclusive grid
  component) and since the \code{x} limits (unless changed)
  are between \code{[0,1]}, then a relative length of
  \code{0.02} represents a length that is approximately
  \code{2\%} of the plot edges from tip to tip. Elements
  which are subject to this include the \code{padding},
  \code{arrowsep}, \code{ticklength.major} and
  \code{ticklength.minor}. This is perhaps not ideal,
  however, seemed to be a necessary sacrifice in order to
  produce these plots.
}

