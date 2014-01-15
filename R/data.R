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

#' @name USDA
#' @title USDA Textural Classification
#' @description This dataset was issued by the United States Department of Agriculture (USDA) 
#' in the form of a ternary diagram, this original ternary diagram has been converted and included here.
#' @docType data
#' @usage data(USDA)
#' @format 1row per point, many points per classification representing the extremes of the area.
#' @source Soil Mechanics Level 1, Module 3, USDA Textural Classification Study Guide
#' @author United States Department of Agriculture (USDA)
#' @examples
#' \donttest{
#' #Load the Libraries
#' library(ggtern)
#' library(plyr)
#' library(grid)
#' 
#' #Load the Data.
#' data(USDA)
#' 
#' #Put tile labels at the midpoint of each tile.
#' USDA.LAB <- ddply(USDA,"Label",function(df){apply(df[,1:3],2,mean)})
#' 
#' #Tweak
#' USDA.LAB$Angle=0; USDA.LAB$Angle[which(USDA.LAB$Label == "Loamy Sand")] = -35
#' 
#' #Construct the plot.
#' ggtern(data=USDA,aes(Clay,Sand,Silt,color=Label,fill=Label)) + 
#'   geom_polygon(alpha=0.75,size=0.5,color="black") +
#'   geom_text(data=USDA.LAB,aes(label=Label,angle=Angle),color="black",size=3.5) + 
#'   theme_rgbw() + 
#'   theme_showsecondary() + 
#'   theme_showarrows() + 
#'   weight_percent() + 
#'   theme(legend.justification=c(0,1),legend.position=c(0,1),axis.tern.padding=unit(.15,"npc")) +
#'   labs(title="USDA Textural Classification Chart",fill="Textural Class",color="Textural Class")
#' }
#' @export
NULL