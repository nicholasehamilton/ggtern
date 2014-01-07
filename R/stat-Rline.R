#' @rdname constant-line
#' @aliases StatRline
#' @export
stat_Rline <- function (mapping = NULL, data = NULL, geom = "Rline", position = "identity", Rintercept, ...) {
  StatRline$new(mapping = mapping, data = data, geom = geom, position = position, Rintercept = Rintercept, ...)
}

StatRline <- proto(ggint$Stat, {
  calculate <- function(., data, scales, Rintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Rintercept, "R") 
    
    tern_stop(.$objname)
    
    lc <- get_last_coord()    
    Tlim <- is.numericor(lc$limits$T,c(1,0))
    Llim <- is.numericor(lc$limits$L,c(1,0))
    Rlim <- is.numericor(lc$limits$R,c(1,0))
    
    unique(within(data, {
      x   <- min(Tlim)
      y   <- 1 - min(Tlim) - Rintercept
      z   <- Rintercept
      xend<- 1 - Rintercept - min(Llim)
      yend<- min(Llim)
      zend<- Rintercept
    }))
  }
  required_aes <- c("Rintercept")
  default_geom <- function(.) GeomRline
})