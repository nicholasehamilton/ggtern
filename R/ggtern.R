
ggtern <- function(...){
  plot <- ggplot(...)
  plot <- plot + theme_tern() + theme(ternary.options=element_ternary())
  
  .val <- function(desired,fallback=""){ifthenelse(is.character(desired),desired,fallback)}
  
  plot <- plot + labs(T=.val(plot$labels$x,"T"),L=.val(plot$labels$y,"L"),R=.val(plot$labels$z,"R"), W="%")
  class(plot) <- c("ggtern",class(plot))
  plot
}








