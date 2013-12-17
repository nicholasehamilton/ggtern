\name{geom_confidence}
\alias{geom_confidence}
\title{Confidence Interval}
\usage{
  geom_confidence(mapping = NULL, data = NULL,
    breaks = c(0.5, 0.9, 0.95), n = 500,
    stat = "confidence", position = "identity",
    na.rm = FALSE, ...)
}
\arguments{
  \item{breaks}{the confidence intervals, default to 50, 90
  and 95 percent.}

  \item{n}{the numer of points in the approximation for
  each confidence interval.}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}
}
\description{
  Calculates the confidence intervals, via the Mahalnobis
  Distance and use of the \code{\link[=logratio]{Log-Ratio
  Transformation}}.
}
\examples{
\donttest{
  data(Feldspar)
  ggtern(data=Feldspar,aes(An,Ab,Or)) + geom_point() + geom_confidence()
}
}

