#' @rdname geom_TLRline
#' @aliases StatRline
#' @export
stat_Rline <- function (mapping = NULL, data = NULL, geom = "Rline", position = "identity", Rintercept, ...) {
  StatRline$new(mapping = mapping, data = data, geom = geom, position = position, Rintercept = Rintercept, ...)
}

StatRline <- proto(ggint$Stat, {
  objname <- "Rline"
  calculate <- function(., data, scales, Rintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Rintercept, "R")
    #check if trying to be applied to non ggtern object
    tern_stop(.$objname)
    
    #get the last coords
    lc <- get_last_coord()  
    
    #determine the limits
    Tlim <- is.numericor(lc$limits$T,c(1,0))
    Llim <- is.numericor(lc$limits$L,c(1,0))
    Rlim <- is.numericor(lc$limits$R,c(1,0))
    
    #determine the data
    data <- unique(within(data, {
      x   <- min(Tlim)
      y   <- 1 - min(Tlim) - Rintercept
      z   <- Rintercept
      xend<- 1 - Rintercept - min(Llim)
      yend<- min(Llim)
      zend<- Rintercept
    }))
    #Rename if non-standard coordinate assignment
    data <- rename(data,c("x"=lc$T,
                          "y"=lc$L,
                          "z"=lc$R,
                          "xend"=paste0(lc$T,"end"),
                          "yend"=paste0(lc$L,"end"),
                          "zend"=paste0(lc$R,"end")))
    data
  }
  required_aes <- c("Rintercept")
  default_geom <- function(.) GeomRline
})