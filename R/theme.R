#' @description \code{validate_element} is a local copy of the ggplot2 function which checks the validity of a given theme element 
#' against the elements table. Since the \code{.elements_tree} is an internal function, which is not exported, and modifications could not be made, 
#' a new (and equivalent) \code{.element_tree} is created within ggtern to handle the new theme elements created within this package.
#' @param el the element
#' @param elname the element name
#' @rdname overloaded
validate_element <- function(el, elname) {
  eldef <- ggint$.element_tree[[elname]]
  
  if (is.null(eldef)) {
    writeLines("testing")
    stop('"', elname, '" is not a valid theme element name...')
  }
  
  # NULL values for elements are OK
  if (is.null(el)) return()
  
  if (eldef$class == "character") {
    # Need to be a bit looser here since sometimes it's a string like "top"
    # but sometimes its a vector like c(0,0)
    if (!is.character(el) && !is.numeric(el))
      stop("Element ", elname, " must be a string or numeric vector.")
    
  } else if (!inherits(el, eldef$class) && !inherits(el, "element_blank")) {
    stop("Element ", elname, " must be a ", eldef$class, " object.")
  }
  invisible()
}

#' @rdname overloaded
#' @inheritParams ggplot2::theme_update
#' @seealso \code{\link[ggplot2]{theme_update}}
theme_update <- function(...) {
  # Make a call to theme, then add to theme
  theme_set(theme_get() %+replace% do.call(theme, list(...)))
}


#' New Theme Elements
#' 
#' \code{ggtern} creates many new theme elements and inheritances, the following is an outline:
#'
#' Theme elements can inherit properties from other theme elements.
#' For example, \code{axis.title.x} inherits from \code{axis.title}, 
#' which in turn inherits from \code{text}. All text elements inherit
#' directly or indirectly from \code{text}; all lines inherit from
#' \code{line}, and all rectangular objects inherit from \code{rect}.
#'
#' Modifying the newly created items requires the same procedures as introduced in the ggplot2 \code{\link[ggplot2]{theme}} documentation.
#' Some convenience functions have been also newly created, proceed to \code{\link{convenience}} for additional information.
#'
#' @aliases themeelements elements newelements theme-tern newthemes newtheme
#' @aliases theme theme-tern newthemes newtheme
#' @aliases ternary.options axis.tern axis.tern.arrow axis.tern.arrow.T axis.tern.arrow.L axis.tern.arrow.R panel.background.tern axis.tern.line axis.tern.line.T 
#' axis.tern.line.L axis.tern.line.R axis.tern.text axis.tern.text.T axis.tern.text.L axis.tern.text.R axis.tern.arrow.text axis.tern.arrow.text.T axis.tern.arrow.text.L
#' axis.tern.arrow.text.R axis.tern.title axis.tern.title.T axis.tern.title.L axis.tern.title.R axis.tern.ticks axis.tern.ticks.major axis.tern.ticks.major.T axis.tern.ticks.major.L
#' axis.tern.ticks.major.R axis.tern.ticks.minor axis.tern.ticks.minor.T axis.tern.ticks.minor.L axis.tern.ticks.minor.R panel.grid.tern panel.grid.tern.major
#' panel.grid.tern.major.T panel.grid.tern.major.L panel.grid.tern.major.R panel.grid.tern.minor panel.grid.tern.minor.T panel.grid.tern.minor.L panel.grid.tern.minor.R 
#' axis.tern.ticks.outside axis.tern.ticklength.major axis.tern.ticklength.minor axis.tern.arrowsep axis.tern.hshift axis.tern.vshift axis.tern.clockwise axis.tern.showarrows
#' axis.tern.arrowstart axis.tern.arrowfinish axis.tern.padding axis.tern.ticks.showsecondary axis.tern.ticks.showprimary
#' @name themeelements
#' @name theme
#' @section New/Additional Inheritance Structures:
#' Based on the \code{ggplot2} existing structure (\code{\link[ggplot2]{theme}}), the new individual theme elements for the ternary plot are as follows:
#' 
#' \tabular{llll}{
#'   \strong{NAME}            \tab \strong{DESCRIPTION}                          \tab \strong{OBJECT}               \tab \strong{INHERITS}      \cr
#'   \code{panel.background.tern**}    \tab Background of Ternary Plot Area      \tab \code{\link{element_rect}}    \tab \code{rect}            \cr
#'   \code{axis.tern}         \tab Base line for ggtern object                   \tab \code{\link{element_line}}    \tab \code{line}            \cr
#'   \code{axis.tern.vshift} \tab Amount to nudge the plot vertically            \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.hshift} \tab Amount to nudge the plot horizontally          \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.clockwise} \tab Clockwise Axis Precession                   \tab \code{\link{logical}}         \tab                        \cr
#'   \code{axis.tern.showarrows} \tab Show axis arrows or not                   \tab \code{\link{logical}}          \tab                        \cr
#'   \code{axis.tern.padding} \tab Padding between axes and panel edges          \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.arrowsep}\tab Distance between axes and the ternary arrows  \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.arrowstart} \tab Proportion along ternary axis when arrow starts  \tab \code{\link{numeric}}   \tab                        \cr
#'   \code{axis.tern.arrowfinish} \tab Proportion along ternary axis when arrow starts  \tab \code{\link{numeric}}   \tab                        \cr
#'   \code{axis.tern.arrow}   \tab Base line for ternary arrows                  \tab \code{\link{element_line}}    \tab \code{axis.tern}       \cr
#'   \code{axis.tern.arrow.T} \tab Specific line for TOP ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.arrow.L} \tab Specific line for LHS ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.arrow.R} \tab Specific line for RHS ternary arrow           \tab \code{\link{element_line}}    \tab \code{axis.tern.arrow} \cr
#'   \code{axis.tern.line}    \tab Base line for ternary axes                    \tab \code{\link{element_line}}    \tab \code{axis.tern}       \cr
#'   \code{axis.tern.line.T}  \tab Specific line for TOP ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.line.L}  \tab Specific line for LHS ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.line.R}  \tab Specific line for RHS ternary axis            \tab \code{\link{element_line}}    \tab \code{axis.tern.line}  \cr
#'   \code{axis.tern.text}    \tab Base text for ggtern object                   \tab \code{\link{element_text}}    \tab \code{text}            \cr
#'   \code{axis.tern.text.T}  \tab Specific text for TOP species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.text.L}  \tab Specific text for LHS species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.text.R}  \tab Specific text for RHS species                 \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.arrow.text}    \tab Base text for arrow labels              \tab \code{\link{element_text}}    \tab \code{axis.tern.text}  \cr
#'   \code{axis.tern.arrow.text.T}  \tab Specific text for TOP arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.arrow.text.L}  \tab Specific text for LHS arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.arrow.text.R}  \tab Specific text for RHS arrow label       \tab \code{\link{element_text}}    \tab \code{axis.tern.arrow.text}  \cr
#'   \code{axis.tern.title}    \tab Base text for Apex Labels                    \tab \code{\link{element_text}}    \tab \code{axis.tern.text}   \cr
#'   \code{axis.tern.title.T}  \tab Specific text for TOP Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   \code{axis.tern.title.L}  \tab Specific text for LHS Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   \code{axis.tern.title.R}  \tab Specific text for RHS Apex Label             \tab \code{\link{element_text}}    \tab \code{axis.tern.title}  \cr
#'   \code{axis.tern.ticklength.major}\tab Major ticklength                      \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.ticklength.minor}\tab Minor ticklength                      \tab \code{\link{unit}}            \tab                        \cr
#'   \code{axis.tern.ticks.outside}  \tab Base ticks for ggtern object on outside or not \tab \code{\link{logical}} \tab                        \cr
#'   \code{axis.tern.ticks.showprimary} \tab Show primary tickset                \tab \code{\link{logical}}         \tab                        \cr
#'   \code{axis.tern.ticks.showsecondary}\tab Show secondary tickset             \tab \code{\link{logical}}         \tab                        \cr
#'   \code{axis.tern.ticks}          \tab Base ticks for ggtern object           \tab \code{\link{element_line}}    \tab \code{axis.tern}   \cr
#'   \code{axis.tern.ticks.major}    \tab Base Major ticks for ggtern object     \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks}   \cr
#'   \code{axis.tern.ticks.major.T}  \tab Major ticks for TOP Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.title.major.L}  \tab Major ticks for LHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.title.major.R}  \tab Major ticks for RHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.major}  \cr
#'   \code{axis.tern.ticks.minor}    \tab Base Minor ticks for ggtern object     \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks}   \cr
#'   \code{axis.tern.ticks.minor.T}  \tab Minor ticks for TOP Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr
#'   \code{axis.tern.title.minor.L}  \tab Minor ticks for LHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr
#'   \code{axis.tern.title.minor.R}  \tab Minor ticks for RHS Axis               \tab \code{\link{element_line}}    \tab \code{axis.tern.ticks.minor}  \cr

#'   \code{panel.grid.tern}          \tab Base gridline for ggtern object        \tab \code{\link{element_line}}    \tab \code{axis.tern}   \cr
#'   \code{panel.grid.tern.major}    \tab Base major gridline                    \tab \code{\link{element_line}}    \tab \code{panel.grid.tern}   \cr
#'   \code{panel.grid.tern.major.T}  \tab Major gridline for TOP Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.major.L}  \tab Major gridline for LHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.major.R}  \tab Major gridline for RHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.major}  \cr
#'   \code{panel.grid.tern.minor}    \tab Base major gridline                    \tab \code{\link{element_line}}    \tab \code{panel.grid.tern}   \cr
#'   \code{panel.grid.tern.minor.T}  \tab Minor gridline for TOP Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#'   \code{panel.grid.tern.minor.L}  \tab Minor gridline for LHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#'   \code{panel.grid.tern.minor.R}  \tab Minor gridline for RHS Axis            \tab \code{\link{element_line}}    \tab \code{panel.grid.tern.minor}  \cr
#'   \code{ternary.options} (DEP***)  \tab Ternary specific options                      \tab \code{\link{element_ternary}} \tab                        \cr
#' }
#' ** \strong{NB:} For \code{panel.background.tern}, whilst the ternary area is 'triangular' per-se, \code{\link{element_rect}} has been used, 
#' as it actually holds NO information regarding the geometry (width, height), only fill, color, 
#' size and linetype border (ie the style of how it will be rendered).
#' 
#' *** \strong{NB:} Fully Depreciated since \code{ggtern} version 1.0.1.3
#' @rdname terntheme
NULL

#' @rdname terntheme
#' @inheritParams ggplot2::theme
#' @export
theme <- function(..., complete = FALSE) {
  elements <- list(...)
  # Check that all elements have the correct class (element_text, unit, etc)
  mapply(validate_element, elements, names(elements))
  structure(elements, class = c("theme", "gg"), complete = complete)
}

#' Build a theme (or partial theme) from theme elements (ggtern version)
#'
#' \code{opts} is deprecated. See the \code{\link{theme}} function.
#' @param ... Arguments to be passed on to the \code{theme} function.
#' @rdname overloaded
#' @seealso \code{\link[ggplot2]{opts}}
#' @export
opts <- function(...) {
  gg_dep("0.9.1", "'opts' is deprecated. Use 'theme' instead.")
  
  # Add check for deprecated elements
  extra <- NULL
  elements <- list(...)
  if (!is.null(elements[["title"]])) {
    # This is kind of a hack, but fortunately it will be removed in future versions
    gg_dep("0.9.1", paste(sep = "\n",
                          'Setting the plot title with opts(title="...") is deprecated.',
                          ' Use labs(title="...") or ggtitle("...") instead.'))
    
    title <- elements$title
    elements$title <- NULL
    
    return(list(ggtitle(title), do.call(theme, elements)))
  }
  
  do.call(theme, elements)
}


#' \code{plot_theme} is a local copy of the method that determines the net theme between a plot and the current global theme.
#' @param x gg object
#' @rdname overloaded
plot_theme <- function(x) {defaults(x$theme, ggtern::theme_get())}


.theme_new <- (function() {
  theme <- theme_gray()
  list(
    get = function(){theme},
    set = function(new){
      thm <- ifthenelse(inherits(get_last_coord(),"ternary"),theme_gray(),ggplot2::theme_gray())
      missing <- setdiff(names(thm),names(new))
      if (length(missing) > 0)
        warning("New theme missing the following elements: ",paste(missing, collapse = ", "), call. = FALSE)
      old <- theme
      theme <<- new
      invisible(old)
    }
  )
})()

#' @rdname overloaded
#' @export
theme_get <- .theme_new$get

#' @rdname overloaded
#' @export
theme_set <- .theme_new$set



#' \code{add_theme} is a local copy of the ggplot2 function which modifies the current theme, by a proposed theme. 
#' It is slightly modified to handle 'logical' values the same way it handles 'character' or 'numeric' values, 
#' which do not inherit from 'element' objects.
#' @inheritParams ggplot2::add_theme
#' @seealso \code{\link[ggplot2]{add_theme}}
#' @rdname overloaded
add_theme <- function(t1, t2, t2name) {
  if (!is.theme(t2)) {
    stop("Don't know how to add ", t2name, " to a theme object",
         call. = FALSE)
  }
  
  # Iterate over the elements that are to be updated
  for (item in names(t2)) {
    x <- t1[[item]]
    y <- t2[[item]]
    
    if (is.null(x) || inherits(x, "element_blank")) {
      # If x is NULL or element_blank, then just assign it y
      x <- y
    } else if (is.null(y) || is.character(y) || is.numeric(y) || is.logical(y) ||
                 inherits(y, "element_blank")) {
      # If y is NULL, or a string or numeric vector, or is element_blank, just replace x
      x <- y
    } else {
      # If x is not NULL, then copy over the non-NULL properties from y
      # Get logical vector of non-NULL properties in y
      idx <- !vapply(y, is.null, logical(1))
      # Get the names of TRUE items
      idx <- names(idx[idx])
      
      # Update non-NULL items
      x[idx] <- y[idx]
    }
    
    # Assign it back to t1
    # This is like doing t1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    t1[item] <- list(x)
  }
  
  # If either theme is complete, then the combined theme is complete
  attr(t1, "complete") <- attr(t1, "complete") || attr(t2, "complete")
  t1
}

#' \code{"\%+replace\%"} is a local copy of the ggplot2 replace operator, no different other than being exported from the ggtern namespace.
#' @rdname overloaded 
"%+replace%" <- function(e1, e2) {
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }
  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}

#' \code{update_theme} is a local copy of a ggplot2 function, which copies elements from the new theme into an old theme.
#' @param oldtheme previous theme object
#' @param newtheme new theme object
#' @rdname overloaded
update_theme <- function(oldtheme, newtheme) {
  # If the newtheme is a complete one, don't bother searching
  # the default theme -- just replace everything with newtheme
  if (attr(newtheme, "complete"))
    return(newtheme)
  
  # These are elements in newtheme that aren't already set in oldtheme.
  # They will be pulled from the default theme.
  newitems <- ! names(newtheme) %in% names(oldtheme)
  newitem_names <- names(newtheme)[newitems]
  oldtheme[newitem_names] <- theme_get()[newitem_names]
  
  # Update the theme elements with the things from newtheme
  # Turn the 'theme' list into a proper theme object first, and preserve
  # the 'complete' attribute. It's possible that oldtheme is an empty
  # list, and in that case, set complete to FALSE.
  oldtheme <- do.call(theme, c(oldtheme,complete = isTRUE(attr(oldtheme, "complete"))))
  
  oldtheme + newtheme
}

#' \code{calc_element} is a local copy of the ggplot2 function which determines the net element based on inheritances, given input theme.
#' @inheritParams ggplot2::calc_element
#' @seealso \code{\link[ggplot2]{calc_element}}
#' @rdname overloaded
calc_element <- function(element, theme, verbose = FALSE) {
  if (verbose) message(element, " --> ", appendLF = FALSE)
  
  # If this is element_blank, don't inherit anything from parents
  if (inherits(theme[[element]], "element_blank")) {
    if (verbose) message("element_blank (no inheritance)")
    return(theme[[element]])
  }
  
  # If the element is defined (and not just inherited), check that
  # it is of the class specified in .element_tree
  if (!is.null(theme[[element]]) &&
        !inherits(theme[[element]], ggint$.element_tree[[element]]$class)) {
    stop(element, " should have class ", ggint$.element_tree[[element]]$class)
  }
  
  # Get the names of parents from the inheritance tree
  pnames <- ggint$.element_tree[[element]]$inherit
  
  # If no parents, this is a "root" node. Just return this element.
  if (is.null(pnames)) {
    # Check that all the properties of this element are non-NULL
    nullprops <- vapply(theme[[element]], is.null, logical(1))
    if (any(nullprops)) {
      stop("Theme element '", element, "' has NULL property: ",
           paste(names(nullprops)[nullprops], collapse = ", "))
    }
    
    if (verbose) message("nothing (top level)")
    return(theme[[element]])
  }
  
  # Calculate the parent objects' inheritance
  if (verbose) message(paste(pnames, collapse = ", "))
  parents <- lapply(pnames, calc_element, theme, verbose)
  
  # Combine the properties of this element with all parents
  Reduce(combine_elements, parents, theme[[element]])
}

#' \code{combine_elements} is a local copy of the ggplot2 function that combines two theme elements
#' @rdname overloaded
#' @param e1 first element
#' @param e2 second element
combine_elements <- function(e1, e2) {
  
  # If e2 is NULL, nothing to inherit
  if (is.null(e2))  return(e1)
  
  # If e1 is NULL, or if e2 is element_blank, inherit everything from e2
  if (is.null(e1) || inherits(e2, "element_blank"))  return(e2)
  
  # If e1 has any NULL properties, inherit them from e2
  n <- vapply(e1[names(e2)], is.null, logical(1))
  e1[n] <- e2[n]
  
  # Calculate relative sizes
  if (ggint$is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }
  
  e1
}




