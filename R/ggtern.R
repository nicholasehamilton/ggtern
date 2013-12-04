
ggtern <- function(...){
  plot <- ggplot(...)
  plot <- plot + theme_tern() + theme(ternary.options=element_ternary())
  plot <- plot + labs(T=validLabel(plot$labels$x,"T"),L=validLabel(plot$labels$y,"L"),R=validLabel(plot$labels$z,"R"), W="%")
  class(plot) <- c("ggtern",class(plot))
  plot
}








