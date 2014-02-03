#' @rdname geom_errorbarTLR
#' @aliases GeomErrorbarl
#' @export
geom_errorbarL <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",allow.outside=TRUE,...) { 
  GeomErrorbarl$new(mapping = mapping, data = data, stat = stat, position = position, allow.outside=allow.outside,...)
}

GeomErrorbarl <- proto(Geom,{
  objname <- "errorbarl"
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA,width=NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x","y","z","Lmax","Lmin")  
  reparameterise <- function(., df, params){
    coordinates <- get_last_coord()
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    #Check
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(df),"geom_errorbarL")
    
    IX <- coordinates$L
    df$width <- df$width %||% params$width %||% 0
    
    #How much to scale by
    divby <- apply(df[,c("x","y","z")],1,sum)
    
    #Scale
    df[,c("x","y","z")] <- df[,c("x","y","z")] / divby
    df$Lmax <- df$Lmax / divby
    df$Lmin <- df$Lmin / divby
    
    #Determine Length of Error bar.
    df$LMAX = (df[,IX] - df$Lmax)/2
    df$LMIN = (df[,IX] - df$Lmin)/2
    df
  }
  
  draw <- function(., data, scales, coordinates, height = NULL, allow.outside,...) {
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    IX <- coordinates$L
    df <- with(data, data.frame( 
      x        = ifthenelse(IX == "x",
                            as.vector(rbind(Lmax,Lmax,Lmax,x,Lmin,Lmin,Lmin)),
                            as.vector(rbind(ifthenelse(IX=="y",x+LMAX+width,x+LMAX-width),
                                            ifthenelse(IX=="y",x+LMAX-width,x+LMAX+width),
                                            x+LMAX,x,x+LMIN,
                                            ifthenelse(IX=="y",x+LMIN+width,x+LMIN-width),
                                            ifthenelse(IX=="y",x+LMIN-width,x+LMIN+width))
                            )
      ),
      y        = ifthenelse(IX=="y",
                            as.vector(rbind(Lmax,Lmax,Lmax,y,Lmin,Lmin,Lmin)),
                            as.vector(rbind(ifthenelse(IX=="z",y+LMAX+width,y+LMAX-width),
                                            ifthenelse(IX=="z",y+LMAX-width,y+LMAX+width),
                                            y+LMAX,y,y+LMIN,
                                            ifthenelse(IX=="z",y+LMIN+width,y+LMIN-width),
                                            ifthenelse(IX=="z",y+LMIN-width,y+LMIN+width))
                            )
      ),
      z        = ifthenelse(IX=="z",
                            as.vector(rbind(Lmax,Lmax,Lmax,z,Lmin,Lmin,Lmin)),
                            as.vector(rbind(ifthenelse(IX=="x",z+LMAX+width,z+LMAX-width),
                                            ifthenelse(IX=="x",z+LMAX-width,z+LMAX+width),
                                            z+LMAX,z,z+LMIN,
                                            ifthenelse(IX=="x",z+LMIN+width,z+LMIN-width),
                                            ifthenelse(IX=="x",z+LMIN-width,z+LMIN+width))
                            )
      ),
      colour   = rep(colour,         each = 7),
      alpha    = rep(alpha,          each = 7),
      size     = rep(size,           each = 7),
      linetype = rep(linetype,       each = 7),
      group    = apply(expand.grid(c(1,1,2,2,2,3,3),1:nrow(data))[,2:1],1,function(x)paste(x[1],x[2],sep="-")),
      #group    = rep(1:(nrow(data)), each = 9),
      stringsAsFactors = FALSE, 
      row.names = 1:(nrow(data)*7)
    ))
    discard <- getOption("tern.discard.external")
    options("tern.discard.external" = !allow.outside)
    ret <- GeomPath$draw(df,scales,coordinates,...)
    options("tern.discard.external" = discard)
    ret
  }
})