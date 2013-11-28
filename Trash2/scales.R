scale_z_continuous <- function(..., expand = waiver()) {
  continuous_scale(c("z", "zmin", "zmax", "zend", "zintercept"), "position_c", identity, ..., expand = expand, guide = "none")
}

Scales <- setRefClass("Scales", fields = "scales", methods = list(
  find = function(aesthetic) {
    vapply(scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  },
  has_scale = function(aesthetic) {
    any(find(aesthetic))
  },
  add = function(scale) {
    prev_aes <- find(scale$aesthetics)
    if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- scales[prev_aes][[1]]$aesthetics[1]
      message("Scale for '", scalename,
              "' is already present. Adding another scale for '", scalename,
              "', which will replace the existing scale.")
    }
    
    # Remove old scale for this aesthetic (if it exists)
    scales <<- c(scales[!prev_aes], list(scale))
  }, 
  clone = function() {
    new_scales <- lapply(scales, ggplot2:::scale_clone)
    Scales$new(new_scales)
  },
  n = function() {
    length(scales)
  },
  input = function() {
    unlist(lapply(scales, "[[", "aesthetics"))
  }, 
  initialize = function(scales = NULL) {
    initFields(scales = scales)
  },
  non_position_scales = function(.) {
    Scales$new(scales[!find("x") & !find("y") & !find("z")])
  },
  get_scales = function(output) {
    scale <- scales[find(output)]
    if (length(scale) == 0) return()
    scale[[1]]
  }  
))

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing <- function(plot, aesthetics, env) {
  
  # Keep only aesthetics that aren't already in plot$scales
  aesthetics <- setdiff(aesthetics, plot$scales$input())
  
  for (aes in aesthetics) {
    print(paste("Checking Missing:",aes))
    scale_name <- paste("scale", aes, "continuous", sep="_")
    
    scale_f <- find_global(scale_name, env)
    plot$scales$add(scale_f())
  }
}
