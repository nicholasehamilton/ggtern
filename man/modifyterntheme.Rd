\name{modifyterntheme}
\alias{convenience}
\alias{modifynewthemes}
\alias{modifyterntheme}
\alias{theme_anticlockwise}
\alias{theme_clockwise}
\alias{theme_counterclockwise}
\alias{theme_noarrows}
\alias{theme_showarrows}
\alias{theme_tern_bw}
\alias{theme_tern_gray}
\alias{theme_tern_nogrid}
\alias{theme_tern_nogrid_major}
\alias{theme_tern_nogrid_minor}
\alias{theme_tern_rgbg}
\alias{theme_tern_rgbw}
\title{Modify Ternary Theme Elements}
\usage{
  theme_noarrows()

  theme_showarrows()

  theme_tern_rgbg()

  theme_tern_rgbw()

  theme_tern_bw()

  theme_tern_gray()

  theme_tern_nogrid_minor()

  theme_tern_nogrid_major()

  theme_tern_nogrid()

  theme_clockwise()

  theme_anticlockwise()

  theme_counterclockwise()
}
\description{
  Convenience functions to assist in the rapid modification
  of key theme elements.

  \code{theme_noarrows} is a function that apepnds to the
  current theme a flag to switch OFF the ternary arrows

  \code{theme_showarrows} is a function that apepnds to the
  current theme a flag to switch ON the ternary arrows

  \code{theme_tern_rgbg} ternary theme, red green blue with
  gray background

  \code{theme_tern_rgbw} ternary theme, red green blue with
  white background

  \code{theme_tern_bw} ternary theme black and white

  \code{theme_tern_gray} ternary theme, gray theme

  \code{theme_tern_nogrid} ternary theme, no minor grids.

  \code{theme_tern_nogrid} ternary theme, no major grids.

  \code{theme_tern_nogrid} ternary theme, no major or minor
  grids.

  \code{theme_clockwise} is an alias for
  \code{\link{tern_clockwise}}

  \code{theme_anticlockwise} is an alias for
  \code{\link{tern_anticlockwise}}

  \code{theme_counterclockwise} is an alias for
  \code{\link{tern_counterclockwise}}
}
\examples{
data(Feldspar)
\donttest{
plot <- ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
plot + theme_showarrows()
plot + theme_noarrows()
plot + theme_showarrows()
plot + theme_tern_rgbg()
plot + theme_tern_rgbw()
plot + theme_tern_bw()
plot + theme_tern_gray()
plot + theme_tern_nogrid_minor()
plot + theme_tern_nogrid_major()
plot + theme_tern_nogrid()
}
}

