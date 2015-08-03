#' Add a smoother. (Ternary Version)
#' 
#' Aids the eye in seeing patterns in the presence of overplotting
#' @seealso \code{\link[ggplot2]{stat_smooth}}
#' @inheritParams ggplot2::stat_smooth
#' @aliases StatSmoothTern
#' @export
stat_smooth_tern <- function (mapping = NULL, data = NULL, geom = "SmoothTern", position = "identity", 
                         method = "auto", formula = y ~ x, se = TRUE, n = 80, fullrange = FALSE, 
                         level = 0.95, na.rm = FALSE, ...) { 
  StatSmoothTern$new(mapping = mapping, data = data, geom = geom, position = position, 
                 method = method, formula = formula, se = se, n = n, fullrange = fullrange, 
                 level = level, na.rm = na.rm, ...)
}

StatSmoothTern <- proto(Statnew, {
  objname <- "smooth_tern"
  calculate_groups <- function(., data, scales, method=ifthenelse(inherits(last_plot(),"ggtern"),"lm","auto"), formula=y~x, ...){
    rows <- daply(data, .(group), function(df) length(unique(df$x)))
    if (all(rows == 1) && length(rows) > 1) {
      message("geom_smooth: Only one unique x value each group.", 
              "Maybe you want aes(group = 1)?")
      return(data.frame())
    }
    
    # Figure out what type of smoothing to do: loess for small datasets,
    # gam with a cubic regression basis for large data
    # This is based on the size of the _largest_ group.
    if (is.character(method) && method == "auto") {
      groups <- count(data, "group")
      
      if (max(groups$freq) < 1000) {
        method <- "loess"
        message('geom_smooth: method="auto" and size of largest group is <1000,',
                ' so using loess.',
                ' Use \'method = x\' to change the smoothing method.')
      } else {
        method <- "gam"
        formula <- y ~ s(x, bs = "cs")
        message('geom_smooth: method="auto" and size of largest group is >=1000,',
                ' so using gam with formula: y ~ s(x, bs = "cs").',
                ' Use \'method = x\' to change the smoothing method.')
      }
    }
    
    if (method == "gam") try_require("mgcv")
    .super$calculate_groups(., data, scales, method = method, formula = formula, ...)
  }
  calculate <- function(., data, scales, method="auto", formula=y~x, se = TRUE, n=80, fullrange=FALSE, xseq = NULL, level=0.95, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_smooth")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }
    
    #---------------------------------------------------------------------------
    #HACK 4 ggtern
    lc <- get_last_coord()
    if(inherits(lc,"ternary")){
      check_required_aesthetics(lc$required_aes, names(data),"coord_tern")
      data <- trytransform(data,lc)
      if(identical(method,lm)){method="lm"}
      if(is.null(xseq)){
        if(fullrange){range=lc$limits$x}else{range=range(data$x, na.rm=TRUE)}
        xseq <- seq(range[1], range[2], length=n)
      }
    }else{
      #---------------------------------------------------------------------------
      #RESUME
      if (is.null(xseq)) {
        if (is.integer(data$x)) {
          if (fullrange) {
            range <- scale_dimension(scales$x, c(0, 0))
          } else {
            xseq <- sort(unique(data$x))
          } 
        } else {
          if (fullrange) {
            range <- scale_dimension(scales$x, c(0, 0))
          } else {
            range <- range(data$x, na.rm=TRUE)  
          } 
          xseq <- seq(range[1], range[2], length=n)
        } 
      }
    }
    
    if (is.character(method)) method <- match.fun(method)
    if (is.null(data$weight)) data$weight <- 1
    
    method.special <- function(...) 
      method(formula, data=data, weights=weight, ...)
    model <- safe.call(method.special, list(...), names(formals(method)))
    predictdf(model, xseq, se, level)
  }
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomSmoothtern
})




