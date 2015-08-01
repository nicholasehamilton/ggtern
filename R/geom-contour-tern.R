#' Display contours of a 3d surface in 2d (Ternary Version)
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "contour")}
#'
#' @inheritParams geom_point
#' @inheritParams geom_path
#' @export
#' # See stat_contour for examples
geom_contour_tern <- function(mapping = NULL, data = NULL, stat = "ContourTern", position = "identity",
                              lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, ...){
  GeomContourTern$new(mapping = mapping, data = data, stat = stat, position = position,
                      lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, ...)
}

GeomContourTern <- proto(ggint$GeomPath,{
  objname        <- "contour_tern"
  required_aes   <- c("x","y","z","value")
  default_aes    <- function(.) aes(weight=1, colour="#3366FF", size = 0.5, linetype = 1, alpha = NA)
  default_stat   <- function(.) StatContourTern
})

#reparameterise <- function(., df, params){
#  coordinates  <- get_last_coord()
#  if(!inherits(coordinates,"ternary")) stop("Coordinates Must be Ternary.")
  
  #Check
#  required_aes <- sort(unique(c(.$required_aes,coordinates$required_aes)))
#  check_required_aesthetics(required_aes, names(df),"geom_contour_tern")
  
  #IX <- coordinates$T
  #df$width <- df$width %||% params$width %||% 0
  
  #How much to scale by
  #divby <- apply(df[,c("x","y","z")],1,sum)
  
  #Scale
  #df[,c("x","y","z")] <- df[,c("x","y","z")] / divby
  #df$Tmax <- df$Tmax / divby
  #df$Tmin <- df$Tmin / divby
  
  #Determine Length of Error bar.
  #df$LMAX = (df[,IX] - df$Tmax)/2
  #df$LMIN = (df[,IX] - df$Tmin)/2
#  df
#}