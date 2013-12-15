\name{stat_density2d}
\alias{stat_density2d}
\title{2d density estimation - (ggtern version)}
\usage{
  stat_density2d(mapping = NULL, data = NULL,
    geom = "Density2dTern", position = "identity",
    na.rm = FALSE, contour = TRUE, n = 100, ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geometric object to use display the data}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

  \item{contour}{If \code{TRUE}, contour the results of the
  2d density estimation}

  \item{n}{number of grid points in each direction}

  \item{...}{other arguments passed on to
  \code{\link{kde2d}}}
}
\description{
  Patched version of the 2d density estimation.
}
\seealso{
  \code{\link[ggtern]{stat_density2d}}
}

