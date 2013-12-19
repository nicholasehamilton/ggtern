\name{tern_clockwise}
\alias{tern_anticlockwise}
\alias{tern_clockwise}
\alias{tern_counterclockwise}
\alias{theme_anticlockwise}
\alias{theme_clockwise}
\alias{theme_counterclockwise}
\title{Direction of Ternary Rotation}
\usage{
  tern_clockwise()

  tern_anticlockwise()

  tern_counterclockwise()

  theme_clockwise()

  theme_anticlockwise()

  theme_counterclockwise()
}
\description{
  \code{tern_clockwise} is a function that instructs the
  axes precession to be clockwise

  \code{tern_anticlockwise} is a function that instructs
  the axes precession to be anticlockwise

  \code{tern_counterclockwise} is an alias for
  \code{tern_anticlockwise}

  \code{theme_clockwise} is an alias for
  \code{tern_clockwise}

  \code{theme_anticlockwise} is an alias for
  \code{tern_anticlockwise}

  \code{theme_counterclockwise} is an alias for
  \code{tern_counterclockwise}
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

