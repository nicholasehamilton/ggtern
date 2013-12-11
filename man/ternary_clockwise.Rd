\name{tern_clockwise}
\alias{tern_anticlockwise}
\alias{tern_clockwise}
\alias{tern_counterclockwise}
\title{Direction of Ternary Rotation}
\usage{
  tern_clockwise()

  tern_anticlockwise()

  tern_counterclockwise()
}
\description{
  \code{tern_clockwise} is a function that instructs the
  axes precession to be clockwise

  \code{tern_anticlockwise} is a function that instructs
  the axes precession to be anticlockwise

  \code{tern_counterclockwise} is an alias for
  \code{tern_anticlockwise}
}
\examples{
plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) +
                  geom_point() +
                  tern_clockwise()
plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) +
                  geom_point() +
                  tern_anticlockwise()
plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) +
                  geom_point() +
                  tern_counterclockwise()
}

