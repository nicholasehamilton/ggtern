\name{theme_clockwise}
\alias{tern_anticlockwise}
\alias{tern_clockwise}
\alias{tern_counterclockwise}
\alias{theme_anticlockwise}
\alias{theme_clockwise}
\alias{theme_counterclockwise}
\title{Direction of Ternary Rotation}
\usage{
  theme_clockwise()

  theme_anticlockwise()

  theme_counterclockwise()

  tern_clockwise()

  tern_anticlockwise()

  tern_counterclockwise()
}
\description{
  \code{theme_clockwise, theme_anticlockwise} (or their
  aliasses) are function that instructs the axes precession
  to be clockwise or anticlockwise respectively.
}
\details{
  If the \code{\link{axis.tern.showarrows}} value is
  \code{FALSE}, these functions will set it to \code{TRUE}.
}
\examples{
\donttest{
  plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) +
                    geom_point() + theme_hidearrows()
  plot
  plot + theme_clockwise()
  plot + theme_anticlockwise()
  plot + theme_counterclockwise()
}
}

