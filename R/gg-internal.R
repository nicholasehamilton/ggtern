.internals <- c(".all_aesthetics",
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
                "map_layout",
                "map_position",
                "new_panel",
                "train_position",
                "Stat",
                "GeomPath",
                "add_group",
                "scales_transform_df",
                "reset_scales",
                "scales_train_df",
                "scales_map_df",
                "order_groups",
                "TopLevel",
                "compute_intercept",
                "combine_elements"
) 
.internals <- unique(.internals)
ggint <- structure(
  mapply(function(.internals, i) getFromNamespace(i,"ggplot2"),.internals, .internals),
  class=c("internal")
)
