.train_position_ternary <- function(panel, T_scale, L_scale, R_scale) {
  if(is.null(panel$T_scales) && !is.null(T_scale)){
    panel$T_scales <- ggint$scale_clone(T_scale)
  }
  if(is.null(panel$L_scales) && !is.null(L_scale)){
    panel$L_scales <- ggint$scale_clone(L_scale)
  }
  if(is.null(panel$R_scales) && !is.null(R_scale)){
    panel$R_scales <- ggint$scale_clone(R_scale)
  }
  panel
}

#' @rdname undocumented
panel_scales <- function(panel, i) {
  this_panel <- panel$layout[panel$layout$PANEL == i, ]
  scales <- list(
    x = panel$x_scales[[this_panel$SCALE_X]],
    y = panel$y_scales[[this_panel$SCALE_Y]],
    T = panel$T_scales,
    L = panel$L_scales,
    R = panel$R_scales,
    W = panel$Wlabel
  )    
  scales[sapply(scales, is.null)] <- NULL
  scales
}

# Calculate statistics
# 
# @param layers list of layers
# @param data a list of data frames (one for each layer)  
#'@rdname undocumented
calculate_stats <- function(panel, data, layers) {
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(panel, panel_data$PANEL[1])
      l$calc_statistic(panel_data, scales)
    })    
  }) 
}

#' Compute ranges and dimensions of each panel, using the coord.
#' @rdname undocumented
train_ranges <- function(panel, coord) {
  compute_range <- function(ix, iy) {
    scales <- list(x = panel$x_scales[[ix]], 
                   y = panel$y_scales[[iy]],
                   T = panel$T_scales,
                   L = panel$L_scales,
                   R = panel$R_scales,
                   W = panel$Wlabel)
    scales[sapply(scales, is.null)] <- NULL
    
    # TODO: change coord_train method to take individual x and y scales
    ggint$coord_train(coord, scales)
  }
  
  panel$ranges <- Map(compute_range,panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}

.Tlabel <- function(panel, labels) {
  panel$T_scales$name  %||% labels[["T"]] %||% labels$x %||% "T"
}
.Llabel <- function(panel, labels,force=F) {  
  #if(get_last_coord()$clockwise & !force){
  #  writeLines("Llabel -> Rlabel")
  #  .Rlabel(panel,labels,force=T)
  #}else{
    panel$L_scales$name  %||% labels[["L"]] %||% labels$y  %||% "L"
  #}
}
.Rlabel <- function(panel, labels,force=F) {
  #if(get_last_coord()$clockwise & !force){
  #  writeLines("Rlabel -> Llabel")
  #  .Llabel(panel,labels,force=T)
  #}else{
    panel$R_scales$name  %||% labels[["R"]] %||% labels$z  %||% "R"
  #}
} 
.Wlabel <- function(panel, labels) {
  (panel$W %||% labels$W) %||% ""
}

