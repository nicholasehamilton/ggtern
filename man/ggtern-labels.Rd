\name{labels}
\alias{Llab}
\alias{Rlab}
\alias{Tlab}
\alias{Wlab}
\alias{labels}
\alias{llab}
\alias{rlab}
\alias{tlab}
\alias{wlab}
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
  \item{label}{the desired label, usually as a
  \code{\link{character}} or \code{\link{expression}},
  although other values can be inputed (such as, for
  example, scalar \code{\link{numeric}} or
  \code{\link{logical}}).}
}
\description{
  New label modification functions, equivalent to the
  original functions in ggplot2 (\code{\link{xlab} and
  \code{\link{ylab}}}) however for the new axes used in the
  \code{ggtern} package
}
\details{
  \code{Tlab} and \code{\link{xlab}} are equivalent, as is
  \code{Llab} and \code{\link{ylab}}, and \code{Rlab} and
  \code{zlab} (\code{zlab} is new to \code{ggtern}).
  \code{Wlab} changes the ternary arrow suffix (ie atomic
  percent, weight percent etc) when the ternary arrows are
  enabled (see \code{\link{theme_showarrows}} and
  \code{\link{percent_weight}})

  Expressions can be used in the labels, in the event that
  the user wishes to render formula, subscripts or
  superscripts, see the last example below.

  \strong{NB:} Aliasses exist for \code{Tlab}, \code{Llab},
  \code{Rlab} and \code{Wlab}, which are \code{tlab},
  \code{llab}, \code{rlab} and \code{wlab}. These aliasses
  produce an identical result, and are there for
  convenience in the event that the user forgets to use an
  upper-case letter.
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

#Top, Left and Right label (aliasses have been commented out)
plot + Tlab("TOP") #tlab("TOP")
plot + Llab("LHS") #llab("LHS")
plot + Rlab("RHS") #rlab("RHS")

#Weight Suffix (ensure showarrows enabled)
plot + theme_showarrows() + Wlab("WEIGHT")

#Use of expressions
plot + Tlab(expression(a^2~+b[2]~"/ Total \%"))
}
}
\seealso{
  ggplot2 \code{\link[ggplot2]{labs}}
}

