\name{labels}
\alias{Llab}
\alias{Rlab}
\alias{Tlab}
\alias{Wlab}
\alias{labels}
\alias{zlab}
\title{Change Axis labels and legend titles}
\usage{
  zlab(label)

  Tlab(label)

  Llab(label)

  Rlab(label)

  Wlab(label)
}
\arguments{
  \item{label}{the label value to change}
}
\description{
  New label modification functions, equivalent to the
  original functions in ggplot2.
}
\details{
  NB: \code{Tlab} and \code{\link{xlab}} are equivalent, as
  is \code{Llab} and \code{\link{ylab}}, and \code{Rlab}
  and \code{zlab}. \code{Wlab} changes the ternary arrow
  suffix. (ie atomic percent, weight percent etc.)
}
\examples{
\donttest{
data(Feldspar)
plot <- ggtern(data=Feldspar,aes(Ab,An,Or)) +
        geom_point() +
        xlab("ABC")  +
        ylab("DEF")  +
        zlab("GHI")
plot
}
}
\seealso{
  \code{\link[ggplot2]{labs}}
}

