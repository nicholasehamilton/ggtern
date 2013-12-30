"+.gg" <- function(e1, e2){  
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  if      (is.theme(e1))  add_theme(e1, e2, e2name)
  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
}

#' @details \code{"\%+\%"} add operator no change other than exporting from this namespace
#' @rdname overloaded 
"%+%" <- `+.gg`

#' @details \code{add_ggplot} is a local copy of method that adds elements to a gg object.
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
    set_last_coord(object)
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