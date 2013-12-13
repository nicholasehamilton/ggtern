\docType{data}
\name{Feldspar}
\alias{Feldspar}
\title{Elkin and Groves Feldspar Data}
\format{1 row per Feldspar composition}
\source{
  Elkin and Groves, 1990
}
\description{
  This dataset is some data on Feldspar and its phases, as
  a function of temperature and pressure.
}
\examples{
data(Feldspar)
summary(Feldspar)
ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
}
\author{
  Nick Hamilton
}
\keyword{datasets}

