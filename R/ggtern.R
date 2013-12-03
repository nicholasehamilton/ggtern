
ggtern <- function(...){
  plot <- ggplot(...)
  plot <- plot + theme_tern() + theme(ternary.options=element_ternary())
  
  ##ADD THE OPTIONS WHICH WILL FLAG THIS AS A TERNARY OBJECT.
  plot <- plot + labs(T=validLabel(plot$labels$x,"T"),
                      L=validLabel(plot$labels$y,"L"),
                      R=validLabel(plot$labels$z,"R"),
                      W="%")
  
  class(plot) <- c("ggtern",class(plot))
  plot
}








