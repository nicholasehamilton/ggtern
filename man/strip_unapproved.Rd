\name{strip_unapproved}
\alias{strip_unapproved}
\title{Strip Unapproved Layers}
\usage{
  strip_unapproved(layers)
}
\arguments{
  \item{layers}{list of the layers to strip unnaproved
  layers from.}
}
\value{
  list of approved layers (may be empty if none are
  approved)
}
\description{
  \code{strip_unapproved} is a function which essentially
  'deletes' layers from the current ternary plot, if such
  layers are not one of the approved layers. The current
  list of approved layers are as follows: \itemize{
  \item{point} \item{path} \item{segment} \item{polygon}
  \item{smooth} \item{text} \item{density2d} \item{rug} }
  The balance of the available geometries to ggplot2 are
  either dissaproved, or, work in progress with regards to
  this package.
}

