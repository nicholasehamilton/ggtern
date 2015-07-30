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

#' \code{panel_scales} is a local copy of the ggplot2 function that calculates the scales for each panel, patched for the ternary system.
#' @param panel the particular panel
#' @param i index number
#' @rdname overloaded
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

#' \code{calculate_stats} is a local copy of the ggplot2 function that calculates various stats for each layer.
#' @param layers list of layers
#' @param data a list of data frames (one for each layer)  
#' @rdname overloaded
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

#' \code{train_ranges} is a local copy of the ggplot2 function that computes ranges and dimensions of each panel, using the coord, 
#' and patched for the ternary system.
#' @rdname overloaded
train_ranges <- function(panel, coord) {
  which.scale <- function(x)
    paste0(coord$required_axes[which(coord$required_aes == coord[[x]])],"_scales")
  compute_range <- function(ix, iy) {
    scales <- list(x = panel$x_scales[[ix]], 
                   y = panel$y_scales[[iy]],
                   T = panel$T_scales,
                   L = panel$L_scales,
                   R = panel$R_scales,
                   #T = panel[[which.scale("T")]],
                   #L = panel[[which.scale("L")]],
                   #R = panel[[which.scale("R")]],
                   W = panel$Wlabel)
    scales[sapply(scales, is.null)] <- NULL
    
    # TODO: change coord_train method to take individual x and y scales
    ggint$coord_train(coord, scales)
  }
  panel$ranges <- Map(compute_range,panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}

#--------------------------------------------------
# Axis labels get read by priority. 
#--------------------------------------------------
# 1. Scale Name                            THEN 
# 2. Explicit Label Assignment             THEN 
# 3. Coordinate-System Assigment           THEN 
# 4. Default/Fallback Value
# NOTE: These functions get used in ggtern-build.R, during which T, L and R gets resolved to the specific x, y and z
#       to the particular coord_tern(...) that is being used.
.Tlabel <- function(panel, labels)
  panel$T_scales$name  %||% labels[["T"]] %||% labels[[get_last_coord()$T]] %||% labels$x %||% "T"
.Llabel <- function(panel, labels,force=F)
  panel$L_scales$name  %||% labels[["L"]] %||% labels[[get_last_coord()$L]] %||% labels$y %||% "L"
.Rlabel <- function(panel, labels,force=F)
  panel$R_scales$name  %||% labels[["R"]] %||% labels[[get_last_coord()$R]] %||% labels$z %||% "R"
.Wlabel <- function(panel, labels)
  (panel$W %||% labels$W) %||% ""

