#' @rdname statTLRline
#' @export
stat_Tline <- function (mapping = NULL, data = NULL, geom = "Tline", position = "identity", Tintercept, ...) {
  StatTline$new(mapping = mapping, data = data, geom = geom, position = position, Tintercept = Tintercept, ...)
}

#' @rdname undocumented
StatTline <- proto(ggint$Stat, {
  objname <- "Tline"
  calculate <- function(., data, scales, Tintercept = NULL, ...) {
    data <- ggint$compute_intercept(data, Tintercept, "T") 
    
    lc <- get_last_coord()
    if(!inherits(lc,"ternary")){stop("Tline only relevant for ternary coordinates")}
    
    Tlim <- lc$limits$T; Tlim <- ifthenelse(!is.numeric(Tlim),c(1,0),Tlim)
    Llim <- lc$limits$L; Llim <- ifthenelse(!is.numeric(Llim),c(1,0),Llim)
    Rlim <- lc$limits$R; Rlim <- ifthenelse(!is.numeric(Rlim),c(1,0),Rlim)
    
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