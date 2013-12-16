
.approved <-   c("point",
                 "path",
                 "segment",
                 "polygon",
                 "smooth",
                 "text",
                 "density2dtern",
                 "rug",
                 "Tline",
                 "Lline",
                 "Rline")

.dissaproved <- c("tile",
                  "violin")


#' Strip Unapproved Layers
#' 
#' \code{strip_unapproved} is a function which essentially 'deletes' layers from the current ternary plot, if such layers are not one 
#' of the approved layers. The current list of approved layers are as follows:
#' \itemize{
#'  \item{point, path, segment and polygon}
#'  \item{Tline, Lline and Rline}
#'  \item{smooth and density2d-ggtern}
#'  \item{text}
#'  \item{rug}
#' }
#' The balance of the available geometries to ggplot2 are either dissaproved, or, work in progress with regards to this package.
#' @param layers list of the layers to strip unnaproved layers from.
#' @return list of approved layers (may be empty if none are approved)
#' @export
strip_unapproved <- function(layers){  
  ##Remove Unapproved Ternary Layers:
  L <- length(layers)
  for(ix in L:1){ #backwards.
    if(inherits(layers[[ix]],"proto")){
      name <- layers[[ix]]$geom$objname
      if(is.character(name)){
        if(!name %in% .approved){
            #IT IS NOT OK
            writeLines(paste0("Removing Layer ",(L - ix + 1),". '",name,"' is not an approved proto (for ternary plots) under the present ggtern package",
                              ifthenelse(name %in% .dissaproved,", furthermore, it is FORBIDDEN!",".")))
            layers[[ix]] <- NULL
        }
      }
    }
  }
  
  #RETURN
  layers
}

