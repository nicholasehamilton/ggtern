
.approved <-   c("point",
                 "path",
                 "segment",
                 "polygon",
                 "smooth",
                 "text",
                 "density2d",
                 "rug")

.dissaproved <- c("tile",
                  "violin")


#' Strip Unapproved Layers
#' 
#' \code{strip_unapproved} is a function which essentially 'deletes' layers from the current ternary plot, if such layers are not one 
#' of the approved layers. The current list of approved layers are as follows:
#' \itemize{
#'  \item{point}
#'  \item{path}
#'  \item{segment}
#'  \item{polygon}
#'  \item{smooth}
#'  \item{text}
#'  \item{density2d}
#'  \item{rug}
#' }
#' The balance of the available geometries to ggplot2 are either dissaproved, or, work in progress with regards to this package.
#' @param layers list of the layers to strip unnaproved layers from.
#' @return list of approved layers (may be empty if none are approved)
#' @export
strip_unapproved <- function(layers){
  ##Remove Unapproved Ternary Layers:
  cnt <- 0
  for(ix in length(layers):1){
    cnt <- cnt + 1
    l <- layers[[ix]]
    if(inherits(l,"proto")){
      name <- l$geom$objname
      if(is.character(name)){
        if(name %in% .approved){
          #IT IS OK
        }else{
          #IT IS NOT OK
          writeLines(paste0("Removing Layer ",cnt,". '",name,"'' is not an approved proto (for ternary plots) under the present ggtern package ",
                            ifthenelse(name %in% .dissaproved,", furthermore, it is FORBIDDEN!",".")))
          layers[[ix]] <- NULL
        }
      }
    }
  }
  
  #RETURN
  layers
}

