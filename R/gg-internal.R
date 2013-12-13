.internals <- c(".all_aesthetics",
                "check_required_aesthetics",
                "set_last_plot",
                "coord_transform.cartesian",
                "scale_clone",
                "coord_train",
                "expand_default",
                "plot_clone",
                "train_cartesian",
                "scales_add_missing",
                "ggname",
                "new_panel",
                "train_layout",
                "map_layout"
)
ggint <- structure(
  mapply(function(.internals, i) getFromNamespace(i,"ggplot2"),.internals, .internals),
  class=c("internal")
)
