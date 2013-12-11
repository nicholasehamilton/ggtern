\name{trytransform}
\alias{trytransform}
\title{Try and Transform Ternary Data}
\usage{
  trytransform(data, ..., coord, scales)
}
\arguments{
  \item{data}{the dataset}

  \item{coord}{the coordinates}

  \item{scales}{the scales}

  \item{...}{not used}
}
\value{
  transformed data
}
\description{
  \code{trytransform} is an internal function which
  attempts to make ternary transformation. If fails, the
  original data is returned
}
\keyword{internal}

