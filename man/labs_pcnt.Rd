\name{percent_weight}
\alias{atomic_percent}
\alias{custom_percent}
\alias{percent_atomic}
\alias{percent_custom}
\alias{percent_weight}
\alias{weight_percent}
\title{Atomic, Weight or Custom Percentage Suffix}
\usage{
  percent_weight()

  weight_percent()

  percent_atomic()

  atomic_percent()

  percent_custom(x)

  custom_percent(x)
}
\arguments{
  \item{x}{the custom suffix}
}
\description{
  \code{percent_weight} adds 'Wt. \%' to the arrow marker
  label as a suffix

  \code{weight_percent} is an alias for
  \code{percent_weight()}

  \code{percent_atomic} adds 'At. \%' to the arrow marker
  label as a suffix

  \code{atomic_percent} is an alias for
  \code{percent_atomic()}

  \code{percent_custom} adds a custom suffix to the arrow
  label marker.

  \code{custom_percent} is an alias for
  \code{percent_custom()}
}
\details{
  By default there are no suffixes behind the arrow label
  marker (the arrow up next to the ternary axes), and these
  functions appends to the set of arrow labels, a value to
  indicate the nature of the scale.

  These are convenience wrappers to \code{labs(W="XYZ")}.
  Atomic, Weight or Custom Percentage Suffix
}
\examples{
\donttest{
plot <- ggtern(data=data.frame(THS=1,LHS=0,RHS=0),aes(x=THS,y=LHS,z=RHS)) + geom_point()
plot + percent_weight()
plot + percent_atomic()
plot + percent_custom("Percentage")
}
}

