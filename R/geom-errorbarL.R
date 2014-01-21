#' @rdname ternaryerrorbars
#' @export
geom_errorbarL <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  ggint$GeomErrorbarL$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

ggint$GeomErrorbarL <- proto(Geom,{
  objname <- "errorbarL"
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x","y","z","Lmax","Lmin")  
  draw <- function(., data, scales, coordinates, height = NULL, ...) {
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(data),"geom_errorbarL")
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    IX <- coordinates$L
    data$LMAX = data[,IX] - data$Lmax
    data$LMIN = data[,IX] - data$Lmin
    
    df <- with(data, data.frame( 
      x        = ifthenelse(IX=="x",
                            as.vector(rbind(Lmax,x,Lmin)),
                            as.vector(rbind(x+LMAX/2,x,x+LMIN/2))
                 ),
      y        = ifthenelse(IX=="y",
                            as.vector(rbind(Lmax,y,Lmin)),
                            as.vector(rbind(y+LMAX/2,y,y+LMIN/2))
                 ),
      z        = ifthenelse(IX=="z",
                            as.vector(rbind(Lmax,z,Lmin)),
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