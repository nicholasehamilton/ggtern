#' Clip Polygons
#' 
#' Using the using the PolyClip Package, This clips input polygons for 
#' use in the density and contour geometries.
#' @param df a data frame
#' @param coord a ternary coordinate system
#' @plyon items in the data frame to pass to ddply argument
#' @keywords polygon clipping
clipPolygons <- function(df,coord,plyon=c('level','piece','group')){
  if(!inherits(coord,"ternary")) stop("'coord' must be ternary") 
  if(getOption('tern.discard.external')){
    extremes = get_tern_extremes(coord)
    clipee   = transform_tern_to_cart(data=extremes, Tlim=coord$limits$T, Llim=coord$limits$L,Rlim=coord$limits$R)
    connect  = function(x){if(length(x) > 0){c(x,x[1])}else{x}}
    df       = ddply(df,plyon,function(clipor){
      tryCatch({
        clipor = rbind(clipor,clipor[nrow(clipor),])   # Close the Loop
        A      = list(list(x=clipor$x,y=clipor$y))     # The Data
        B      = list(list(x=clipee$x,y=clipee$y))     # The Triangle
        clip   = polyclip(A,B,op="intersection",fillB="evenodd",fillA="evenodd")
        return(data.frame(x=connect(clip[[1]]$x), y=connect(clip[[1]]$y)))
      },error=function(e){
        ## SILENT
      });return(NULL)
    })
  }; df
}