
#Create a list of the ggplot2 internal functions that we seek to get access to.
.internals <- unique(
              c(".all_aesthetics",
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
                "GeomPolygon",
                "add_group",
                "scales_transform_df",
                "reset_scales",
                "scales_train_df",
                "scales_map_df",
                "order_groups",
                "TopLevel",
                "compute_intercept",
                "combine_elements",
                "is.rel",
                "facet_render",
                "coord_labels",
                "xlabel",
                "ylabel",
                "element_render",
                "build_guides",
                "is.zero",
                "plot_clone",
                "set_last_plot",
                "make_labels",
                "update_guides",
                "zeroGrob",
                "el_def",
                "digest.ggplot",
                "layer"
))

#' \code{ggint} is a structure which holds all the required internal functions from the ggplot2 namespace, 
#' which can then be used conveniently via \code{ggint$ABC}.
#' @rdname undocumented
ggint <- structure(
  mapply(function(.internals, i) getFromNamespace(i,"ggplot2"),.internals, .internals),
  class=c("internal")
)
