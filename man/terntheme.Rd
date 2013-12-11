\name{theme}
\alias{theme}
\alias{theme-tern}
\alias{theme_noarrows}
\alias{theme_nocart}
\alias{theme_showarrows}
\alias{theme_tern_bw}
\alias{theme_tern_gray}
\alias{theme_tern_rgbg}
\alias{theme_tern_rgbw}
\title{New Theme Elements}
\usage{
  theme_nocart()

  theme_noarrows()

  theme_showarrows()

  theme_tern_rgbg()

  theme_tern_rgbw()

  theme_tern_bw()

  theme_tern_gray()
}
\description{
  Use this function to modify theme settings.

  Ternary Theme Elements

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
}
\details{
  Theme elements can inherit properties from other theme
  elements. For example, \code{axis.title.x} inherits from
  \code{axis.title}, which in turn inherits from
  \code{text}. All text elements inherit directly or
  indirectly from \code{text}; all lines inherit from
  \code{line}, and all rectangular objects inherit from
  \code{rect}.

  For more examples of modifying properties using
  inheritance, see \code{\link{+.gg}} and
  \code{\link{\%+replace\%}}.

  To see a graphical representation of the inheritance
  tree, see the last example below.
}
\section{New/Additional Inheritance Structures}{
  Based on the \code{ggplot2} existing structure
  (\code{\link[ggplot2]{theme}}), The new individual theme
  elements for the ternary plot are as follows:

  \tabular{lll}{ \strong{NAME} \tab \strong{DESCRIPTION}
  \tab \strong{(INHERITS)} \cr ternary.options \tab ternary
  specific options \tab (\code{element_ternary}) \cr
  axis.tern \tab base line for ternary plot \tab
  (\code{element_line}) \cr }
}

\section{Clearing the Usual Cartesian Elements}{
  \code{theme_nocart} is a function that returns empty
  theme elemens for the standard cartesian memebers,
  including panel.background, panel.border,
  panel.grid.major, panel.grid.minor, axis.ticks,
  axis.text.x, axis.text.y, axis.title.x, axis.title.y are
  all set to blank
}

