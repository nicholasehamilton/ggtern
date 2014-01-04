\name{constructor}
\alias{constructor}
\title{ggtern Constructor}
\usage{
  ggtern(data = NULL, ...)
}
\arguments{
  \item{data}{default data set}

  \item{...}{other arguments passed to specific methods}
}
\description{
  Plots in \code{ggtern} are instigated via the default
  constructor: \code{ggtern(...)}, which is essentially a
  convenience wrapper for the following: \code{ggplot{...}
  + coord_tern()}, indeed, if one wishes to use
  \code{ggplot{...} + coord_tern()} then this is quite
  satisfactory. For further information on the
  \code{coord_tern(...)} coordinate system, click
  \link[=coord_tern]{HERE}.
}

