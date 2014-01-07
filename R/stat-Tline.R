#' @rdname constant-line
#' @aliases StatTline
#' @export
stat_Tline <- function (mapping = NULL, data = NULL, geom = "Tline", position = "identity", Tintercept, ...) {
  StatTline$new(mapping = mapping, data = data, geom = geom, position = position, Tintercept = Tintercept, ...)
}

StatTline <- proto(ggint$Stat, {
  objname <- "Tline"
  calculate <- function(., data, scales, Tintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Tintercept, "T") 
    
    tern_stop(.$objname)
    
    lc <- get_last_coord()    
    Tlim <- is.numericor(lc$limits$T,c(1,0))
    Llim <- is.numericor(lc$limits$L,c(1,0))
    Rlim <- is.numericor(lc$limits$R,c(1,0))
    
    unique(within(data, {
      x    <- Tintercept
      y    <- 1-Tintercept-min(Rlim)
      z    <- min(Rlim)
      xend <- Tintercept
      yend <- min(Llim)
      zend <- 1-Tintercept-min(Llim)
    }))
  }
  required_aes <- c("Tintercept")
  default_geom <- function(.) GeomTline
})