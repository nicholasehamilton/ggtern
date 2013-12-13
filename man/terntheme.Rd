\name{themeelements}
\alias{axis.tern}
\alias{axis.tern.arrow}
\alias{axis.tern.arrow.L}
\alias{axis.tern.arrow.R}
\alias{axis.tern.arrow.T}
\alias{axis.tern.arrow.text}
\alias{axis.tern.arrow.text.L}
\alias{axis.tern.arrow.text.R}
\alias{axis.tern.arrow.text.T}
\alias{axis.tern.line}
\alias{axis.tern.line.L}
\alias{axis.tern.line.R}
\alias{axis.tern.line.T}
\alias{axis.tern.text}
\alias{axis.tern.text.L}
\alias{axis.tern.text.R}
\alias{axis.tern.text.T}
\alias{axis.tern.ticks}
\alias{axis.tern.ticks.major}
\alias{axis.tern.ticks.major.L}
\alias{axis.tern.ticks.major.R}
\alias{axis.tern.ticks.major.T}
\alias{axis.tern.ticks.minor}
\alias{axis.tern.ticks.minor.L}
\alias{axis.tern.ticks.minor.R}
\alias{axis.tern.ticks.minor.T}
\alias{axis.tern.title}
\alias{axis.tern.title.L}
\alias{axis.tern.title.R}
\alias{axis.tern.title.T}
\alias{elements}
\alias{newelements}
\alias{newtheme}
\alias{newthemes}
\alias{panel.background.tern}
\alias{panel.grid.tern}
\alias{panel.grid.tern.major}
\alias{panel.grid.tern.major.L}
\alias{panel.grid.tern.major.R}
\alias{panel.grid.tern.major.T}
\alias{panel.grid.tern.minor}
\alias{panel.grid.tern.minor.L}
\alias{panel.grid.tern.minor.R}
\alias{panel.grid.tern.minor.T}
\alias{ternary.options}
\alias{theme-tern}
\alias{themeelements}
\title{New Theme Elements}
\description{
  \code{ggtern} creates many new theme elements and
  inheritances, the following is an outline:
}
\details{
  Theme elements can inherit properties from other theme
  elements. For example, \code{axis.title.x} inherits from
  \code{axis.title}, which in turn inherits from
  \code{text}. All text elements inherit directly or
  indirectly from \code{text}; all lines inherit from
  \code{line}, and all rectangular objects inherit from
  \code{rect}.

  Modifying the newly created items requires the same
  procedures as introduced in the ggplot2
  \code{\link[ggplot2]{theme}} documentation. Some
  convenience functions have been also newly created,
  proceed to \code{\link{convenience}} for additional
  information.
}
\section{New/Additional Inheritance Structures}{
  Based on the \code{ggplot2} existing structure
  (\code{\link[ggplot2]{theme}}), the new individual theme
  elements for the ternary plot are as follows:

  \tabular{llll}{ \strong{NAME} \tab \strong{DESCRIPTION}
  \tab \strong{OBJECT} \tab \strong{INHERITS} \cr
  \code{ternary.options} \tab Ternary specific options \tab
  \code{\link{element_ternary}} \tab \cr
  \code{panel.background.tern**} \tab Background of Ternary
  Plot Area \tab \code{\link{element_rect}} \tab
  \code{rect} \cr \code{axis.tern} \tab Base line for
  ggtern object \tab \code{\link{element_line}} \tab
  \code{line} \cr \code{axis.tern.arrow} \tab Base line for
  ternary arrows \tab \code{\link{element_line}} \tab
  \code{axis.tern} \cr \code{axis.tern.arrow.T} \tab
  Specific line for TOP ternary arrow \tab
  \code{\link{element_line}} \tab \code{axis.tern.arrow}
  \cr \code{axis.tern.arrow.L} \tab Specific line for LHS
  ternary arrow \tab \code{\link{element_line}} \tab
  \code{axis.tern.arrow} \cr \code{axis.tern.arrow.R} \tab
  Specific line for RHS ternary arrow \tab
  \code{\link{element_line}} \tab \code{axis.tern.arrow}
  \cr \code{axis.tern.line} \tab Base line for ternary axes
  \tab \code{\link{element_line}} \tab \code{axis.tern} \cr
  \code{axis.tern.line.T} \tab Specific line for TOP
  ternary axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.line} \cr \code{axis.tern.line.L} \tab
  Specific line for LHS ternary axis \tab
  \code{\link{element_line}} \tab \code{axis.tern.line} \cr
  \code{axis.tern.line.R} \tab Specific line for RHS
  ternary axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.line} \cr \code{axis.tern.text} \tab Base
  text for ggtern object \tab \code{\link{element_text}}
  \tab \code{text} \cr \code{axis.tern.text.T} \tab
  Specific text for TOP species \tab
  \code{\link{element_text}} \tab \code{axis.tern.text} \cr
  \code{axis.tern.text.L} \tab Specific text for LHS
  species \tab \code{\link{element_text}} \tab
  \code{axis.tern.text} \cr \code{axis.tern.text.R} \tab
  Specific text for RHS species \tab
  \code{\link{element_text}} \tab \code{axis.tern.text} \cr
  \code{axis.tern.arrow.text} \tab Base text for arrow
  labels \tab \code{\link{element_text}} \tab
  \code{axis.tern.text} \cr \code{axis.tern.arrow.text.T}
  \tab Specific text for TOP arrow label \tab
  \code{\link{element_text}} \tab
  \code{axis.tern.arrow.text} \cr
  \code{axis.tern.arrow.text.L} \tab Specific text for LHS
  arrow label \tab \code{\link{element_text}} \tab
  \code{axis.tern.arrow.text} \cr
  \code{axis.tern.arrow.text.R} \tab Specific text for RHS
  arrow label \tab \code{\link{element_text}} \tab
  \code{axis.tern.arrow.text} \cr \code{axis.tern.title}
  \tab Base text for Apex Labels \tab
  \code{\link{element_text}} \tab \code{axis.tern.text} \cr
  \code{axis.tern.title.T} \tab Specific text for TOP Apex
  Label \tab \code{\link{element_text}} \tab
  \code{axis.tern.title} \cr \code{axis.tern.title.L} \tab
  Specific text for LHS Apex Label \tab
  \code{\link{element_text}} \tab \code{axis.tern.title}
  \cr \code{axis.tern.title.R} \tab Specific text for RHS
  Apex Label \tab \code{\link{element_text}} \tab
  \code{axis.tern.title} \cr

  \code{axis.tern.ticks} \tab Base ticks for ggtern object
  \tab \code{\link{element_line}} \tab \code{axis.tern} \cr
  \code{axis.tern.ticks.major} \tab Base Major ticks for
  ggtern object \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks} \cr \code{axis.tern.ticks.major.T}
  \tab Major ticks for TOP Axis \tab
  \code{\link{element_line}} \tab
  \code{axis.tern.ticks.major} \cr
  \code{axis.tern.title.major.L} \tab Major ticks for LHS
  Axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks.major} \cr
  \code{axis.tern.title.major.R} \tab Major ticks for RHS
  Axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks.major} \cr
  \code{axis.tern.ticks.minor} \tab Base Minor ticks for
  ggtern object \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks} \cr \code{axis.tern.ticks.minor.T}
  \tab Minor ticks for TOP Axis \tab
  \code{\link{element_line}} \tab
  \code{axis.tern.ticks.minor} \cr
  \code{axis.tern.title.minor.L} \tab Minor ticks for LHS
  Axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks.minor} \cr
  \code{axis.tern.title.minor.R} \tab Minor ticks for RHS
  Axis \tab \code{\link{element_line}} \tab
  \code{axis.tern.ticks.minor} \cr \code{panel.grid.tern}
  \tab Base gridline for ggtern object \tab
  \code{\link{element_line}} \tab \code{axis.tern} \cr
  \code{panel.grid.tern.major} \tab Base major gridline
  \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern} \cr \code{panel.grid.tern.major.T}
  \tab Major gridline for TOP Axis \tab
  \code{\link{element_line}} \tab
  \code{panel.grid.tern.major} \cr
  \code{panel.grid.tern.major.L} \tab Major gridline for
  LHS Axis \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern.major} \cr
  \code{panel.grid.tern.major.R} \tab Major gridline for
  RHS Axis \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern.major} \cr
  \code{panel.grid.tern.minor} \tab Base major gridline
  \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern} \cr \code{panel.grid.tern.minor.T}
  \tab Minor gridline for TOP Axis \tab
  \code{\link{element_line}} \tab
  \code{panel.grid.tern.minor} \cr
  \code{panel.grid.tern.minor.L} \tab Minor gridline for
  LHS Axis \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern.minor} \cr
  \code{panel.grid.tern.minor.R} \tab Minor gridline for
  RHS Axis \tab \code{\link{element_line}} \tab
  \code{panel.grid.tern.minor} \cr } ** \strong{NB:} For
  \code{panel.background.tern}, whilst the ternary area is
  'triangular' per-se, \code{\link{element_rect}} has been
  used, as it actually holds NO information regarding the
  geometry (width, height), only fill, color, size and
  linetype border (ie the style of how it will be
  rendered).
}

