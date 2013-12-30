
#' Show or Hide Grid
#' 
#' \code{theme_nogrid_minor} ternary theme, no minor grids.
#' @aliases theme_tern_nogrid_minor tern_nogrid_minor
#' @rdname showhidegrid
#' @export
theme_nogrid_minor <- function(){
  theme(panel.grid.tern.minor=element_blank(),
        panel.grid.tern.minor.T=element_blank(),
        panel.grid.tern.minor.L=element_blank(),
        panel.grid.tern.minor.R=element_blank()
  ) 
}
theme_tern_nogrid_minor <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_minor has been superceded by theme_nogrid_minor")
  theme_nogrid_minor()
}
tern_nogrid_minor <- theme_nogrid_minor

#' \code{theme_nogrid_major} ternary theme, no major grids.
#' @rdname showhidegrid
#' @aliases theme_tern_nogrid_major theme_nogrid_major tern_nogrid_major
#' @export
theme_nogrid_major <- function(){
  theme(panel.grid.tern.major=element_blank(),
        panel.grid.tern.major.T=element_blank(),
        panel.grid.tern.major.L=element_blank(),
        panel.grid.tern.major.R=element_blank()
  ) 
}
theme_tern_nogrid_major <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid_major has been superceded by theme_nogrid_major")
  theme_nogrid_major()
}
tern_nogrid_major <- theme_nogrid_major

#' \code{theme_nogrid} ternary theme, no major AND no minor grids.
#' @aliases theme_tern_nogrid tern_nogrid theme_hidegrid tern_hidegrid
#' @rdname showhidegrid
#' @export
theme_nogrid <- function(){
  theme_nogrid_minor() + theme_nogrid_major()
}
theme_tern_nogrid <- function(){
  tern_dep("1.0.1.3","theme_tern_nogrid has been superceded by theme_nogrid")
  theme_nogrid()
}
tern_nogrid    <- tern_hidegrid <- theme_hidegrid <- theme_nogrid