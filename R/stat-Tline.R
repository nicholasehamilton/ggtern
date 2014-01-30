#' @rdname geom_TLRline
#' @aliases StatTline
#' @export
stat_Tline <- function (mapping = NULL, data = NULL, geom = "Tline", position = "identity", Tintercept, ...) {
  StatTline$new(mapping = mapping, data = data, geom = geom, position = position, Tintercept = Tintercept, ...)
}

StatTline <- proto(ggint$Stat, {
  objname <- "Tline"
  calculate <- function(., data, scales, Tintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Tintercept, "T") 
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
      x    <- Tintercept
      y    <- 1-Tintercept-min(Rlim)
      z    <- min(Rlim)
      xend <- Tintercept
      yend <- min(Llim)
      zend <- 1-Tintercept-min(Llim)
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
  required_aes <- c("Tintercept")
  default_geom <- function(.) GeomTline
})