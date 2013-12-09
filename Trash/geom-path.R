GeomPath <- proto(ggplot2:::Geom, {
  objname <- "path"
  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, ..., na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?")
    }
    
    ##-----------------------------------------------------------------------------------------
    ##HACK FOR TERNARY PLOT
    #is.tern <- inherits(coordinates,"ternary")
    #if(is.tern){data = ggplot2:::coord_transform(coordinates,data,scales)}
    ###-----------------------------------------------------------------------------------------
    
    keep <- function(x) {
      # from first non-missing to last non-missing
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first), 
        rep(TRUE, last - first), 
        rep(FALSE, length(x) - last))
    }    
    # Drop missing values at the start or end of a line - can't drop in the 
    # middle since you expect those to be shown by a break in the line
    missing <- !complete.cases(data[c("x", "y", "size", "colour", "linetype")])
    kept <- ave(missing, data$group, FUN=keep)
    data <- data[kept, ]
    # must be sorted on group
    data <- arrange(data, group)
  
    
    if (!all(kept) && !na.rm) {
      warning("Removed ", sum(!kept), " rows containing missing values", " (geom_path).", call. = FALSE)
    }
    
    munched <- coord_munch(coordinates, data, scales)
    print(munched)
    
    # Silently drop lines with less than two points, preserving order
    rows <- ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())
    
    # Work out whether we should use lines or segments
    attr <- ddply(munched, .(group), function(df) {
      data.frame(
        solid = identical(unique(df$linetype), 1),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines", 
           ", colour, size and linetype must be constant over the line",
           call.=FALSE)
    }
    
    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)  
    
    if (!constant) {
      with(munched, 
           segmentsGrob(
             x[!end],   
             y[!end], 
             x[!start], 
             y[!start],
             default.units="native", arrow = arrow, 
             gp = gpar(
               col = alpha(colour, alpha)[!end], fill = alpha(colour, alpha)[!end],
               lwd = size[!end] * .pt, lty = linetype[!end], 
               lineend = lineend, linejoin = linejoin, linemitre = linemitre
             )
           )
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      with(munched, 
           polylineGrob(
             x, y, id = id, 
             default.units = "native", arrow = arrow, 
             gp = gpar(
               col = alpha(colour, alpha)[start], fill = alpha(colour, alpha)[start],
               lwd = size[start] * .pt, lty = linetype[start], 
               lineend = lineend, linejoin = linejoin, linemitre = linemitre)
           )
      )
    }
  }
  
  draw_legend <- function(., data, ...) {
    data$arrow <- NULL
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data, 
         ggname(.$my_name(), segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units="npc",
                                          gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, 
                                                  lty=linetype, lineend="butt")))
    )
  }
  
  default_stat <- function(.) StatIdentity
  required_aes <- c("x","y")
  default_aes  <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom  <- function(.) "path"
})