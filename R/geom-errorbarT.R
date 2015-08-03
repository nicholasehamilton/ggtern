#' Ternary Error Bars
#' 
#' \code{geom_errorbarT}, \code{geom_errorbarL} and \code{geom_errorbarR} are geometries to render error bars
#' for the top, left and right apex species respectively, analogous to \code{\link[ggplot2]{geom_errorbar}} and/or 
#' \code{\link[ggplot2]{geom_errorbarh}} as provided in the base ggplot2 package.
#' 
#' @inheritParams ggplot2::geom_point
#' @param allow.outside logical value indicating whether the error bars can overflow the plot area, if FALSE, bars outiside will be stripped.
#' @section Aesthetics (geom_errorbarT): 
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbart")}
#' 
#' @section Aesthetics (geom_errorbarL):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbarl")}
#' 
#' @section Aesthetics (geom_errorbarR):
#' \Sexpr[results=rd,stage=build]{ggtern:::rd_aesthetics("geom", "errorbarl")}
#' @rdname geom_errorbarTLR
#' @name geom_errorbarTLR
#' @examples
#' #Example with Dummy Data.
#' tmp <- data.frame(x=1/3,
#' y=1/3,
#' z=1/3,
#' Min=1/3-1/6,
#' Max=1/3+1/6)
#' ggtern(data=tmp,aes(x,y,z)) + 
#'   geom_point() + 
#'   geom_errorbarT(aes(Tmin=Min,Tmax=Max))+
#'   geom_errorbarL(aes(Lmin=Min,Lmax=Max))+
#'   geom_errorbarR(aes(Rmin=Min,Rmax=Max)) 
NULL

#' @rdname geom_errorbarTLR
#' @aliases GeomErrorbart
#' @export
geom_errorbarT <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", allow.outside=TRUE,...) { 
  GeomErrorbart$new(mapping = mapping, data = data, stat = stat, position = position, allow.outside=allow.outside,...)
}

GeomErrorbart <- proto(Geom,{
  objname <- "errorbart"
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, height=0.5, alpha = NA,width=NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x","y","z","Tmax","Tmin")
  reparameterise <- function(., df, params){
    coordinates <- get_last_coord()
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    #Check
    required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
    check_required_aesthetics(required_aes, names(df),"geom_errorbarT")
    
    IX <- coordinates$T
    df$width <- df$width %||% params$width %||% 0
    
    #How much to scale by
    divby <- apply(df[,c("x","y","z")],1,sum)
    
    #Scale
    df[,c("x","y","z")] <- df[,c("x","y","z")] / divby
    df$Tmax <- df$Tmax / divby
    df$Tmin <- df$Tmin / divby
    
    #Determine Length of Error bar.
    df$LMAX = (df[,IX] - df$Tmax)/2
    df$LMIN = (df[,IX] - df$Tmin)/2
    df
  }
  
  draw <- function(., data, scales, coordinates, height = NULL,allow.outside,...) {
    if(!inherits(coordinates,"ternary"))
      stop("Coordinates Must be Ternary.")
    
    IX <- coordinates$T  
    df <- with(data, data.frame( 
      x        = ifthenelse(IX == "x",
                            as.vector(rbind(Tmax,Tmax,Tmax,x,Tmin,Tmin,Tmin)),
                            as.vector(rbind(ifthenelse(IX=="y",x+LMAX+width,x+LMAX-width),
                                            ifthenelse(IX=="y",x+LMAX-width,x+LMAX+width),
                                            x+LMAX,x,x+LMIN,
                                            ifthenelse(IX=="y",x+LMIN+width,x+LMIN-width),
                                            ifthenelse(IX=="y",x+LMIN-width,x+LMIN+width))
                            )
      ),
      y        = ifthenelse(IX=="y",
                            as.vector(rbind(Tmax,Tmax,Tmax,y,Tmin,Tmin,Tmin)),
                            as.vector(rbind(ifthenelse(IX=="z",y+LMAX+width,y+LMAX-width),
                                            ifthenelse(IX=="z",y+LMAX-width,y+LMAX+width),
                                            y+LMAX,y,y+LMIN,
                                            ifthenelse(IX=="z",y+LMIN+width,y+LMIN-width),
                                            ifthenelse(IX=="z",y+LMIN-width,y+LMIN+width))
                            )
      ),
      z        = ifthenelse(IX=="z",
                            as.vector(rbind(Tmax,Tmax,Tmax,z,Tmin,Tmin,Tmin)),
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