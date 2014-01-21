\name{geom_errorbarT}
\alias{geom_errorbarL}
\alias{geom_errorbarR}
\alias{geom_errorbarT}
\title{Ternary Error Bars}
\usage{
  geom_errorbarT(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    allow.outside = TRUE, ...)

  geom_errorbarL(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    allow.outside = TRUE, ...)

  geom_errorbarR(mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    allow.outside = TRUE, ...)
}
\arguments{
  \item{allow.outside}{logical value indicating whether the
  error bars can overflow the plot area, if FALSE, bars
  outiside will be stripped.}

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
}
\description{
  \code{geom_errorbarT}, \code{geom_errorbarL} and
  \code{geom_errorbarR} are geometries to render error bars
  for the top, left and right apex species respectively,
  analogous to \code{\link[ggplot2]{geom_errorbar}} and/or
  \code{\link[ggplot2]{geom_errorbarh}} as provided in the
  base ggplot2 package.
}
\section{Aesthetics}{
  Additional required aethetics are, for each respectively:

  \code{geom_errorbarT:} \itemize{ \item \code{Tmax} \item
  \code{Tmin} } \code{geom_errorbarL:} \itemize{ \item
  \code{Lmax} \item \code{Lmin} } \code{geom_errorbarR:}
  \itemize{ \item \code{Rmax} \item \code{Rmin} }
}
\examples{
\donttest{
#Example with Dummy Data.
tmp <- data.frame(x=1/3,
y=1/3,
z=1/3,
Min=1/3-1/6,
Max=1/3+1/6)
ggtern(data=tmp,aes(x,y,z)) +
  geom_point() +
  geom_errorbarT(aes(Tmin=Min,Tmax=Max))+
  geom_errorbarL(aes(Lmin=Min,Lmax=Max))+
  geom_errorbarR(aes(Rmin=Min,Rmax=Max))

#Example with Feldspar Data
data(Feldspar)
Feldspar$Min <- (Feldspar$An - 5)
Feldspar$Max <- (Feldspar$An + 5)
ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) +
  geom_errorbarT(aes(Tmin=Min,Tmax=Max,width=0.01),size=0.2,allow.outside=T) +
  geom_point(fill="white",shape=21)
}
}

