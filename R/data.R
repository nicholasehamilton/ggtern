#' @name Feldspar
#' @title Elkin and Groves Feldspar Data
#' @description This dataset is some data on Feldspar and its phases, as a function of temperature and pressure.
#' @docType data
#' @usage data(Feldspar)
#' @format 1 row per Feldspar composition
#' @source Elkin and Groves, 1990
#' @author Nick Hamilton
#' @examples
#' \donttest{
#'  data(Feldspar)
#'  summary(Feldspar)
#'  ggtern(data=Feldspar,aes(x=An,y=Ab,z=Or)) + geom_point()
#' }
#' @export
NULL