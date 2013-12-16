\name{validate_element}
\alias{\%+\%}
\alias{\%+replace\%}
\alias{add_ggplot}
\alias{calc_element}
\alias{combine_elements}
\alias{opts}
\alias{plot_theme}
\alias{theme_get}
\alias{theme_set}
\alias{theme_update}
\alias{update_theme}
\alias{validate_element}
\title{Overloaded ggplot2 functions}
\usage{
  validate_element(el, elname)

  theme_update(...)

  opts(...)

  plot_theme(x)

  theme_get()

  theme_set(new)

  %+replace%(e1, e2)

  update_theme(oldtheme, newtheme)

  calc_element(element, theme, verbose = FALSE)

  combine_elements(e1, e2)

  %+%(e1, e2)

  add_ggplot(p, object, objectname)
}
\arguments{
  \item{el}{the element}

  \item{elname}{the element name}

  \item{x}{gg object}

  \item{oldtheme}{previous theme object}

  \item{newtheme}{new theme object}

  \item{e1}{first element}

  \item{e2}{second element}

  \item{p}{plot}

  \item{object}{to add}

  \item{objectname}{name of obeject}

  \item{...}{named list of theme settings}

  \item{new}{new theme (a list of theme elements)}

  \item{element}{The name of the theme element to
  calculate}

  \item{theme}{A theme object (like theme_grey())}

  \item{verbose}{If TRUE, print out which elements this one
  inherits from}
}
\description{
  \code{validate_element} is a function which checks the
  validity of a given theme element, against the elements
  table. Since the \code{.elements_tree} is an internal
  function, which is not exported, and modifications could
  not be made, a new (and equivalent) \code{.element_tree}
  is created within ggtern.

  Overloaded ggplot2 Functions

  \code{plot_theme} is a local copy of the method that
  determines the net theme between a plot and the current
  theme

  \code{"\%+replace\%"} replace operator

  \code{update_theme} is a local copy of a ggplot2
  function, which copies elements from the new theme into
  an old theme.

  Calculate the element properties, by inheriting
  properties from its parents

  \code{combine_elements} is a local copy of method that
  combines two theme elements

  \code{"\%+\%"} add operator

  \code{add_ggplot} is a local copy of method that adds
  elements to a gg object.
}
\seealso{
  \code{\link[ggplot2]{calc_element}}
}

