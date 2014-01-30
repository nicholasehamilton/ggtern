#' @rdname  geom_TLRline
#' @aliases StatLline
#' @export
stat_Lline <- function (mapping = NULL, data = NULL, geom = "Lline", position = "identity", Lintercept, ...) {
  StatLline$new(mapping = mapping, data = data, geom = geom, position = position, Lintercept = Lintercept, ...)
}

StatLline <- proto(ggint$Stat, {
  objname <- "Lline"
  calculate <- function(., data, scales, Lintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Lintercept, "L") 
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
      x   <- 1 - Lintercept - min(Rlim)
      y   <- Lintercept
      z   <- min(Rlim)
      xend<- min(Tlim)
      yend<- Lintercept
      zend<- 1 - Lintercept - min(Tlim)
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
  required_aes <- c("Lintercept")
  default_geom <- function(.) GeomLline
})