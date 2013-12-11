\name{ggtern_constructor}
\alias{ggtern_constructor}
\title{Create ggtern object}
\usage{
  ggtern(...)
}
\arguments{
  \item{...}{same arguments as passed through to ggplot}
}
\description{
  \code{ggtern} is a function, analogous to \code{ggplot},
  which creates an object of class \code{ggtern} (which
  inherits \code{ggplot}).
}
\details{
  This method is a convenience wrapper for
  \code{ggplot(...) + coord_tern()}
}
\examples{
#Dummy Data.
DATA <- data.frame(x=1,y=1,z=1)

#Method 1
plot <- ggtern(data=DATA,aes(x,y,z)) + geom_point()
plot

#Method 2
plot <- ggplot(data=DATA,aes(x,y,z)) + geom_point() + coord_tern()
plot
}
\seealso{
  \code{\link{ggplot}}
}

