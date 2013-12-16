
.theme_new <- (function() {
  theme.tern <- theme_tern_gray()
  theme      <- theme_gray()
  list(
    get = function(){
      ifthenelse(inherits(last_plot(),"ggtern"),theme.tern,theme)
    },
    set = function(new) {
      ifthenelse(inherits(last_plot(),"ggtern"),{
        missing <- setdiff(names(theme_tern_gray()), names(new))
        if (length(missing) > 0) {
          warning("New theme missing the following elements: ",paste(missing, collapse = ", "), call. = FALSE)
        }
        old <- theme.tern
        theme.tern <<- new
        invisible(old)
      },{
        missing <- setdiff(names(theme_gray()), names(new))
        if (length(missing) > 0) {
          warning("New theme missing the following elements: ",
                  paste(missing, collapse = ", "), call. = FALSE)
        }
        old <- theme
        theme <<- new
        invisible(old)
      })
    }
  )
})()

#' @rdname overloaded
theme_get <- .theme_new$get

#' @rdname overloaded
theme_set <- .theme_new$set

#' @rdname overloaded
#' @inheritParams ggplot2::theme_update
theme_update <- function(...) {
  # Make a call to theme, then add to theme
  ggtern::theme_set(ggtern::theme_get() %+replace% do.call(ggtern::theme, list(...)))
}

#' Calculate the element properties, by inheriting properties from its parents
#'
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

#' Overloaded ggplot2 Functions
#' 
#' \code{combine_elements} is a local copy of method that combines two theme elements
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

#' Overloaded ggplot2 Functions
#' 
#' \code{plot_theme} is a local copy of the method that determines the net theme between a plot and the current theme
#' @param x gg object
#' @rdname overloaded
plot_theme <- function(x) {defaults(x$theme, ggtern::theme_get())}

#' Overloaded ggplot2 functions
#' 
#' \code{validate_element} is a function which checks the validity of a given theme element, against the elements table.
#' Since the \code{.elements_tree} is an internal function, which is not exported, and modifications could not be made, 
#' a new (and equivalent) \code{.element_tree} is created within ggtern.
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

#' Overloaded ggplot2 Functions
#' 
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


"+.gg" <- function(e1, e2) {  
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  
  if      (is.theme(e1))  ggint$add_theme(e1, e2, e2name)
  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
}

#' Overloaded ggplot2 Functions
#' 
#' \code{"\%+\%"} add operator
#' @rdname overloaded 
"%+%" <- `+.gg`

#' Overloaded ggplot2 Functions
#' 
#' \code{"\%+replace\%"} replace operator
#' @rdname overloaded 
"%+replace%" <- function(e1, e2) {
  if (!is.theme(e1) || !is.theme(e2)) {
    stop("%+replace% requires two theme objects", call. = FALSE)
  }
  # Can't use modifyList here since it works recursively and drops NULLs
  e1[names(e2)] <- e2
  e1
}

#' Overloaded ggplot2 Functions
#' 
#' \code{add_ggplot} is a local copy of method that adds elements to a gg object.
#' @param p plot
#' @param object to add
#' @param objectname name of obeject
#' @rdname overloaded
add_ggplot <- function(p, object, objectname) {
  if (is.null(object)) return(p)
  p <- ggint$plot_clone(p)
  if (is.data.frame(object)) {
    p$data <- object
  } else if (is.theme(object)) {
    p$theme <- update_theme(p$theme, object)
  } else if (inherits(object, "scale")) {
    p$scales$add(object)
  } else if(inherits(object, "labels")) {
    p <- update_labels(p, object)
  } else if(inherits(object, "guides")) {
    p <- ggint$update_guides(p, object)
  } else if(inherits(object, "uneval")) {
    p$mapping <- defaults(object, p$mapping)
    
    labels <- lapply(object, deparse)
    names(labels) <- names(object)
    p <- update_labels(p, labels)
  } else if (is.coord(object)) {
    p$coordinates <- object
    p
  } else if (is.facet(object)) {
    p$facet <- object
    p
  } else if(is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else if(is.proto(object)) {
    p <- switch(object$class(),
                layer  = {
                  p$layers <- append(p$layers, object)
                  
                  # Add any new labels
                  mapping <- ggint$make_labels(object$mapping)
                  default <- ggint$make_labels(object$stat$default_aes())
                  
                  new_labels <- defaults(mapping, default)
                  p$labels <- defaults(p$labels, new_labels)
                  p
                },
                coord = {
                  p$coordinates <- object
                  p
                }
    )
  } else {
    stop("Don't know how to add ", objectname, " to a plot",
         call. = FALSE)
  }
  ggint$set_last_plot(p)
  p
}