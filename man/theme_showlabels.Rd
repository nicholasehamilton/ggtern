\name{theme_showlabels}
\alias{theme_hidelabels}
\alias{theme_nolabels}
\alias{theme_showlabels}
\title{Show or Hide Axis Ticklabels}
\usage{
  theme_showlabels()

  theme_hidelabels()
}
\description{
  Convenience functions to enable or disable the axis
  ticklabels

  \code{theme_showlabels} is a function that apends to the
  current theme a flag to switch ON the axis ticklabels

  \code{theme_hidelabels} or \code{theme_nolabels} (Alias)
  are functions that apends to the current theme a flag to
  switch OFF the axis ticklabels
}
\examples{
\donttest{
data(Feldspar)
plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) + geom_point()
plot
plot + theme_hidelabels()
}
}

