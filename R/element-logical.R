#' Theme element: logical switch.
#' 
#' @param value logical value
#' @export
element_logical <- function(value=TRUE){
  structure(
    list(value=value),
    class = c("element_logical","logical","element")
  )
}