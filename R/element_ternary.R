element_ternary <- function(showarrows =TRUE,
                            padding    =0.15,
                            arrowsep   =0.10,
                            arrowstart =0.25,
                            arrowfinish=0.75,
                            ticklength.major =0.020,
                            ticklength.minor =0.010){
  structure(
    list(padding=padding,arrowsep=arrowsep,showarrows=showarrows,arrowstart=arrowstart,arrowfinish=arrowfinish,
         ticklength.major=ticklength.major,ticklength.minor=ticklength.minor),
    class = c("element_ternary")
  )
}