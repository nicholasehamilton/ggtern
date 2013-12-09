GeomPolygon <- proto(ggplot2:::Geom, {
  objname <- "polygon"
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, ...) {
    n <- nrow(data)
    if (n == 1) return()
    
    # Check if group is numeric, to make polygonGrob happy (factors are numeric,
    # but is.numeric() will report FALSE because it actually checks something else)
    if (mode(data$group) != "numeric") data$group <- factor(data$group)
    
    ##-----------------------------------------------------------------------------------------
    ##HACK FOR TERNARY PLOT
    #is.tern <- inherits(coordinates,"ternary")
    #if(is.tern){data = ggplot2:::coord_transform(coordinates,data,scales)}
    ###-----------------------------------------------------------------------------------------
    
    # Sort by group to make sure that colors, fill, etc. come in same order
    munched <- coord_munch(coordinates, data, scales) #MODIFIED coord_munch
    munched <- munched[order(munched$group), ]
    
    # For gpar(), there is one entry per polygon (not one entry per point).
    # We'll pull the first value from each group, and assume all these values
    # are the same within each group.
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx,]
    
    ggname(.$my_name(), gTree(children = gList(
      polygonGrob(munched$x, 
                  munched$y, 
                  default.units = "native",
                  id = munched$group,
                  gp = gpar(
                    col = first_rows$colour,
                    fill = alpha(first_rows$fill, first_rows$alpha),
                    lwd = first_rows$size * .pt,
                    lty = first_rows$linetype
                  )
      )
    )))
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="NA", fill="grey20", size=0.5, linetype=1, alpha = NA)
  required_aes <- c("x", "y")
  guide_geom <- function(.) "polygon"
  
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data, grobTree(
      rectGrob(gp = gpar(col = colour, fill = alpha(fill, alpha), lty = linetype)),
      linesGrob(gp = gpar(col = colour, lwd = size * .pt, lineend="butt", lty = linetype))
    ))
  }
  
})