#' @description \code{validate_element} is a local copy of the ggplot2 function which checks the validity of a given theme element 
#' against the elements table. Since the \code{.elements_tree} is an internal function, which is not exported, and modifications could not be made, 
#' a new (and equivalent) \code{.element_tree} is created within ggtern to handle the new theme elements created within this package.
#' @param el the element
#' @param elname the element name
#' @rdname overloaded
validate_element <- function(el, elname) {
  eldef <- ggint$.element_tree[[elname]]
  
  if (is.null(eldef)) {
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

#' @rdname theme_elements
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




