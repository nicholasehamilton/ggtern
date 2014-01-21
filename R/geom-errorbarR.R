#' @rdname ternaryerrorbars
#' @export
geom_errorbarR <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  ggint$GeomErrorbarR$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

ggint$GeomErrorbarR <- proto(Geom,{
  objname <- "errorbarR"
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x","y","z","Rmax","Rmin")
  draw <- function(., data, scales, coordinates, height = NULL, ...) {
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_errorbarR")
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    IX <- coordinates$R
    data$LMAX = data[,IX] - data$Rmax
    data$LMIN = data[,IX] - data$Rmin
    
    df <- with(data, data.frame( 
      x        = ifthenelse(IX=="x",
                            as.vector(rbind(Rmax,x,Rmin)),
                            as.vector(rbind(x+LMAX/2,x,x+LMIN/2))
                 ),
      y        = ifthenelse(IX=="y",
                            as.vector(rbind(Rmax,y,Rmin)),
                            as.vector(rbind(y+LMAX/2,y,y+LMIN/2))
                 ),
      z        = ifthenelse(IX=="z",
                            as.vector(rbind(Rmax,z,Rmin)),
                            as.vector(rbind(z+LMAX/2,z,z+LMIN/2))
                 ),
      colour   = rep(colour,each = 3),
      alpha    = rep(alpha, each = 3),
      size     = rep(size,  each = 3),
      linetype = rep(linetype, each = 3),
      group    = rep(1:(nrow(data)), each = 3),
      stringsAsFactors = FALSE, 
      row.names = 1:(nrow(data)*3)
    ))
    GeomPath$draw(df,scales,coordinates,...)
  }
})