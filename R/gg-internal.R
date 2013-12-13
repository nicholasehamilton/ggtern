.internals <- c(".all_aesthetics",
                "check_required_aesthetics",
                "set_last_plot",
                "coord_transform.cartesian",
                "scale_clone",
                "coord_train",
                "expand_default",
                "plot_clone",
                "train_cartesian"
)
ggint <- structure(
  mapply(function(.internals, i) getFromNamespace(i,"ggplot2"),.internals, .internals),
  class=c("internal")
)
