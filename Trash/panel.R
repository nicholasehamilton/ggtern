.panel_hack <- function(){

  # Compute ranges and dimensions of each panel, using the coord.
  train_ranges <- function(panel, coord) {
    compute_range <- function(ix, iy) {
      # TODO: change coord_train method to take individual x and y scales
      ggplot2:::coord_train(coord, list(
                              x = panel$x_scales[[ix]], 
                              y = panel$y_scales[[iy]]
                              ##POSSIBLY ADD T, L, R HERE
                              ))
    }
    
    panel$ranges <- Map(compute_range,panel$layout$SCALE_X,panel$layout$SCALE_Y)
    panel
  }
  
  unlockBinding("train_ranges", asNamespace("ggplot2"))
    assign("train_ranges",train_ranges, asNamespace("ggplot2"))
  lockBinding("train_ranges", asNamespace("ggplot2"))
}