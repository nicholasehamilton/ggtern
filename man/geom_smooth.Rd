\name{geom_smooth}
\alias{geom_smooth}
\title{Add a smoothed conditional mean (MODIFIED)}
\usage{
  geom_smooth(mapping = NULL, data = NULL, stat = "smooth",
    position = "identity", limitarea = T, fullrange = F,
    ...)
}
\arguments{
  \item{limitarea}{logical value to indicate whether
  smoothed data is discared outside of the ternary plot
  area, when this is set to \code{TRUE}, the value of
  \code{fullrange} has no meaning, since the full-range
  becomes essentially becomes limited by the ternary plot
  area. When set to \code{FALSE}, \code{fullarea} behaves
  as per normal.}
}
\description{
  \code{geom_smooth} is a function that provides additional
  functionality to the standard
  \code{\link[ggplot2]{geom_smooth}} function in the event
  that it is being used on a \code{ggtern} object.
  Parameters are the same, with the exception of the
  introduction of the \code{limitarea} parameter.
}

