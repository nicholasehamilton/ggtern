\name{theme_get}
\alias{\%+\%}
\alias{\%+replace\%}
\alias{add_ggplot}
\alias{calc_element}
\alias{combine_elements}
\alias{plot_theme}
\alias{theme_get}
\alias{theme_set}
\alias{theme_update}
\alias{update_theme}
\alias{validate_element}
\title{Calculate the element properties, by inheriting properties from its parents}
\usage{
  theme_get()

  theme_set(new)

  theme_update(...)

  calc_element(element, theme, verbose = FALSE)

  combine_elements(e1, e2)

  plot_theme(x)

  validate_element(el, elname)

  update_theme(oldtheme, newtheme)

  %+%(e1, e2)

  %+replace%(e1, e2)

  add_ggplot(p, object, objectname)
}
\arguments{
  \item{e1}{first element}

  \item{e2}{second element}

  \item{x}{gg object}

  \item{el}{the element}

  \item{elname}{the element name}

  \item{oldtheme}{previous theme object}

  \item{newtheme}{new theme object}

  \item{p}{plot}

  \item{object}{to add}

  \item{objectname}{name of obeject}

  \item{new}{new theme (a list of theme elements)}

  \item{...}{named list of theme settings}

  \item{element}{The name of the theme element to
  calculate}

  \item{theme}{A theme object (like theme_grey())}

  \item{verbose}{If TRUE, print out which elements this one
  inherits from}
}
\description{
  Calculate the element properties, by inheriting
  properties from its parents

  \code{combine_elements} is a local copy of method that
  combines two theme elements

  \code{plot_theme} is a local copy of the method that
  determines the net theme between a plot and the current
  theme

  \code{validate_element} is a function which checks the
  validity of a given theme element, against the elements
  table. Since the \code{.elements_tree} is an internal
  function, which is not exported, and modifications could
  not be made, a new (and equivalent) \code{.element_tree}
  is created within ggtern.

  \code{update_theme} is a local copy of a ggplot2
  function, which copies elements from the new theme into
  an old theme.

  \code{"\%+\%"} add operator

  \code{"\%+replace\%"} replace operator

  \code{add_ggplot} is a local copy of method that adds
  elements to a gg object.
}
\seealso{
  \code{\link[ggplot2]{calc_element}}
}

