#' @rdname  constant-line
#' @aliases StatLline
#' @export
stat_Lline <- function (mapping = NULL, data = NULL, geom = "Lline", position = "identity", Lintercept, ...) {
  StatLline$new(mapping = mapping, data = data, geom = geom, position = position, Lintercept = Lintercept, ...)
}

StatLline <- proto(ggint$Stat, {
  objname <- "Lline"
  calculate <- function(., data, scales, Lintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Lintercept, "L") 
    
    tern_stop(.$objname)
    
    lc <- get_last_coord()    
    Tlim <- is.numericor(lc$limits$T,c(1,0))
    Llim <- is.numericor(lc$limits$L,c(1,0))
    Rlim <- is.numericor(lc$limits$R,c(1,0))
    
    unique(within(data, {
      x   <- 1 - Lintercept - min(Rlim)
      y   <- Lintercept
      z   <- min(Rlim)
      
      xend<- min(Tlim)
      yend<- Lintercept
      zend<- 1 - Lintercept - min(Tlim)
    }))
  }
  required_aes <- c("Lintercept")
  default_geom <- function(.) GeomLline
})