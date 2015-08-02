.approved <-   c(point     = "point",
                 path      = "path",
                 line      = "line",
                 segment   = "segment",
                 polygon   = "polygon",
                 text      = "text",
                 contour   = "interpolate_tern",
                 density2d = "density_tern",
                 smooth    = "smooth_tern",
                 polygon   = "polygon_tern",
                 rug       = "rug",
                 Tline     = "Tline",
                 Lline     = "Lline",
                 Rline     = "Rline",
                 confidence= "confidence",
                 errorbarT = "errorbart",
                 errorbarL = "errorbarl",
                 errorbarR = "errorbarr")
.dissaproved <- c(tile     = "tile",
                  violin   = "violin")
.rd_approved <- function(approved=TRUE){
  paste(ifthenelse(approved,"APPROVED","DISAPPROVED")," geometries in \\code{ggtern} are as follows:\n\n",
        "\\itemize{\n",
        paste("  \\item \\code{",
              ifthenelse(approved,"\\link{",""),"geom_",
              names(ifthenelse(approved,.approved,.dissaproved)), "}",
              ifthenelse(approved,"}",""), collapse = "\n", sep = ""),
        "\n}\n", sep = "")
}

#' Approved Geometries
#' 
#' \Sexpr[results=rd,stage=build]{ggtern:::.rd_approved(TRUE)}
#' \Sexpr[results=rd,stage=build]{ggtern:::.rd_approved(FALSE)}
#' 
#' The balance of the available geometries to ggplot2 are work in progress with regards to the \code{ggtern} package. 
#' 
#' Attempting to apply non-approved geometries (ie geometries not in the above list), will result in them being stripped from 
#' the final plot.
#' @name approved_geometries
#' @rdname approved_geometries
NULL


#' Strip Unapproved Layers
#' 
#' \code{strip_unapproved} is an internal function which essentially 'deletes' layers from the current ternary plot in the event that 
#' such layers are not one of the approved layers. Refer to \link{approved_geometries} for the current list of approved geometries.
#' 
#' @param layers list of the layers to strip unnaproved layers from.
#' @return \code{strip_unapproved} returns a list of approved layers (may be empty if none are approved).
#' @seealso \code{\link{approved_geometries}}
#' @keywords internal
strip_unapproved <- function(layers){  
  ##Remove Unapproved Ternary Layers:
  L <- length(layers)
  for(ix in L:1){ #backwards.
    if(inherits(layers[[ix]],"proto")){
      name <- layers[[ix]]$geom$objname
      stat <- layers[[ix]]$stat$objname
      if(!name %in% .approved){
          #IT IS NOT OK
          writeLines(paste0("Removing Layer ",(L - ix + 1),". '",name,"' is not an approved proto (for ternary plots) under the present ggtern package",
                            ifthenelse(name %in% .dissaproved,", furthermore, it is FORBIDDEN!",".")))
          layers[[ix]] <- NULL
      }#else if(!stat %in% .approved){
      #    writeLines(paste0("Removing Layer ",(L - ix + 1),". '",stat,"' is not an approved proto (for ternary plots) under the present ggtern package",
      #                      ifthenelse(name %in% .dissaproved,", furthermore, it is FORBIDDEN!",".")))
      #    layers[[ix]] <- NULL
      #}
    }
  }
  
  #RETURN
  layers
}

