#' \code{ifthenelse} function takes input arguments \code{x}, \code{a} and \code{b} and returns \code{a} if \code{x} is \code{TRUE}, else, returns \code{b}
#' @param x logical input to check
#' @param a value to return if \code{x} is TRUE
#' @param b value to return if \code{x} is FALSE
#' @rdname undocumented
ifthenelse <- function(x,a,b){
  if(!is.logical(x))stop("x argument must be logical")
  if(x){a}else{b}
}

#' \code{is.numericor} function takes input arguments \code{a} and \code{b} and returns \code{a} if \code{a} is numeric, else, returns \code{b}
#' @param a value to return if \code{a} is numeric
#' @param b value to return if \code{a} is not numeric
#' @rdname undocumented
is.numericor <- function(a,b){
  if(missing(b)){stop("b must be provided")}
  if(!is.numeric(b)){stop("b must be numeric")}
  ifthenelse(is.numeric(a),a,b)
}
"%||%" <- function(a, b) {if (!is.null(a)) a else b}

#' \code{get_tern_extremes} determines the limiting ternary coordinates given input coordinates.
#' @param coordinates ggtern coordinate system, inheriting "ternary" and "coord" classes.
#' @param verbose logical indicating verbose reporting to console
#' @param expand numeric value to 
#' @examples get_tern_extremes(coordinates = coord_tern())
#' @return \code{get_tern_extremes} returns data.frame representing the T, L and R amounts (Columns) at each of the tips (extremes) of the ternary plot area (Rows)
#' @rdname undocumented
get_tern_extremes <- function(coordinates,verbose=F,expand=0){
  expand = max(0,is.numericor(expand[1],0)); 
  expand <- c(-expand/2,expand)
  
  if(!inherits(coordinates,"ternary") & !inherits(coordinates,"coord"))stop("coordinates must be ternary coordinates")
  
  Tlim <- coordinates$limits$T; if(!is.numeric(Tlim)){Tlim <- c(0,1)};Tlim <- Tlim + expand 
  Llim <- coordinates$limits$L; if(!is.numeric(Llim)){Llim <- c(0,1)};Llim <- Llim + expand
  Rlim <- coordinates$limits$R; if(!is.numeric(Rlim)){Rlim <- c(0,1)};Rlim <- Rlim + expand 
  
  #get the data.
  ret <- data.frame(T=c(max(Tlim),min(Tlim),min(Tlim)),
                    L=c(min(Llim),max(Llim),min(Llim)),
                    R=c(min(Rlim),min(Rlim),max(Rlim)))
    
  rownames(ret) <- c("AT.T","AT.L","AT.R")
  
  #Check
  agg <- round(apply(ret,1,sum),3)
  err <- 0.001
  ix  <- which(agg > 1.0+err | agg < 1.0-err)
  
  report.data <- data.frame(ret,Total=agg);
  rownames(report.data) <- paste("At Point:",   c("T","L","R"))
  colnames(report.data) <- c("T Amt.","L Amt.","R Amt.","Tot. Amt.")
  if(length(ix) > 0){
      writeLines("ATTENTION: Non-Default Ternary Limits Do not Sum to Unity!")
      print(report.data)
      stop("Extremes must sum to unity.")
  }
  if(min(agg) < 0 - min(expand) | max(ret) > 1 + max(expand)){
    writeLines("ATTENTION: Non-Default Ternary Limits are outside [0,1]")
    print(report.data)
    stop("Negative Values, or Values > 1 are Not Acceptable")
  }else if(verbose){
    writeLines("ATTENTION: Non-Default Ternary Limits are OK.")
    print(report.data)
  }
  
  #Return
  invisible(ret)
}

#' Ternary Transformations
#' 
#' Functions relating to the transformation from the ternary coordinate systems, to the cartesian coordinate system.
#' 
#' @section Transform Ternary Coordinates to Cartesian Coordinates:
#' \code{transform_tern_to_cart(...)} is a function that takes input numeric vectors for the \code{T}, \code{L} and \code{R} species, 
#' or, alternatively, a data.frame with columns \code{T}, \code{L} and \code{R} (Mandatory Column Names), and, transforms the data from the ternary space, 
#' to the cartesian space where \code{x} and \code{y} are in the range \code{[0,1]} and [0,\eqn{sin(\pi/3)}] respectively.
#' The limits for \code{T}, \code{L} and \code{R} \strong{MAY NOT NECESSARILY} be in the range \code{[0,1]}, however, this is the default range.
#' 
#' Since the constituents of each ternary points must sum to \code{1.0}, the user has the option to scale the data so that this is satisfied. 
#' Negative values \emph{may} be of interest when trying to determine the coordinates of a point 'outside' of the ternary plot surface, 
#' however, they must still sum to unity.
#' 
#' Custom Limits can be applied for \code{T}, \code{L} and \code{R} species (by the parameters \code{Tlim}, \code{Llim} and \code{Rlim}, respectively),
#' however, if they are non-standard (ie [0,1]), then checks are made so that non-sensical results are not implied and,
#' an error will be thrown if such non-sensical results exist, IE, All points \strong{MUST} sum to unity given that the axis extremes 
#' should meet at the vertices of the plot area. 
#' 
#' By the above statement, the following constraints must hold \code{TRUE}: 
#' \enumerate{
#'  \item{\strong{max(Tlim)} + \code{min(Llim) + min(Rlim) = 1} AND}
#'  \item{\code{min(Tlim)} + \strong{max(Llim)} + \code{min(Rlim) = 1} AND}
#'  \item{\code{min(Tlim) + min(Llim)} + \strong{max(Rlim)} = 1}
#' }
#' 
#' @param T the concentrations of the \strong{Top} species on the ternary diagram
#' @param L the concentrations of the \strong{Left} species on the ternary diagram
#' @param R the concentrations of the \strong{Right} species on the ternary diagram
#' @param data object of type \code{data.frame} containing columns \code{T}, \code{L} and \code{R}. If not specified (Default), 
#' it will be produced from the \code{T}, \code{L} and \code{R} parameters for use in the function.
#' @param scale logical indicating whether the concentrations should be scaled to sum to unity.
#' @param ... not used
#' @param Tlim the limits of the top axis
#' @param Llim the limits of the left axis
#' @param Rlim the limits of the right axis
#' @return \code{transform_tern_to_cart} returns a \code{data.frame} object with columns \code{x} and \code{y} representing the transformed coordinates, and, number of rows
#' equal to that of the \code{data} argument. In other words, a '1 to 1' transformation from the ternary to the cartesian space. 
#' @rdname ternary_transformations
#' @name   ternary_transformations
#' @aliases transform_tern_to_cart
#' @examples
#' #Species Concentrations
#' T=c(1,0,0) #TOP 
#' L=c(0,1,0) #LEFT
#' R=c(0,0,1) #RIGHT
#' #Transform
#' transform_tern_to_cart(T,L,R)
transform_tern_to_cart <- function(T,L,R,data=data.frame(T=T,L=L,R=R),...,Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1),scale=TRUE){
  if(class(data) != "data.frame")stop("data must be of type 'data.frame'")
  if(length(which(c("T","L","R") %in% colnames(data))) < 3) stop("data must contain columns T, L and R")
  
  Tlim <- sort(Tlim)
  Rlim <- sort(Rlim)
  Llim <- sort(Llim)
  
  d <- data; 
  s <- rowSums(d);
  
  #If scale to composition sum of 1
  if(scale){
    ix <- which(s <= 0)
    if(length(ix) > 0){
      #Consider 0,0,0 to be equal parts (not strictly true, but, to prevent divide by zero)
      d[ix,] <- c(1,1,1)/3
      s[ix]  <- 1.0
    }
    for(i in 1:ncol(d)){d[,i] <- d[,i]/s}
  }
  
  #Adjust for the Limits.
  .adj <- function(input,lim){(input-lim[1])/(lim[length(lim)] - lim[1])}
  d$T <- .adj(d$T,Tlim)
  d$L <- .adj(d$L,Llim)
  d$R <- .adj(d$R,Rlim)
  
  #Calculate
  out.Y <- d$T*tan(pi/3)*0.5
  out.X <- d$R + out.Y*tan(pi/6)
  
  return(data.frame(x=out.X,y=out.Y))
}

#' \code{arrow_label_formatter} is a function that formats the labels directly adjacent to the ternary arrows.
#' @param label character label
#' @param suffix chacater suffix behind each label
#' @param sep the seperator between label and suffix 
#' @rdname undocumented
#' @examples arrow_label_formatter("TOP","Wt.%",sep="/")
arrow_label_formatter <- function(label,suffix="",...,sep="/"){
  if(missing(label))stop("label cannot be missing")
  if(!is.character(label) | !is.character(suffix) | !is.character(sep))stop("label, sep and suffix must be characters")
  if(missing(suffix)){
    label
  }else if(identical(suffix,NULL) | identical(suffix,"") | missing(suffix)){
    label
  }else{
    paste(label,sep,suffix)
  }
}

#' \code{calc_element_plot} Calculates the element properties, by inheriting properties from its parents, 
#' and compares to whether the local plot overrides this value. Based largely off the \code{\link[ggplot2]{calc_element}} 
#' as provided in \code{\link{ggplot2}}
#' @seealso \code{\link[ggplot2]{calc_element}}
#' @param element the element name to calculate
#' @param theme the theme to inherit from
#' @param plot the plot to check locally for theme element, NULL is ok.
#' @param ... not used
#' @rdname undocumented
calc_element_plot <- function(element,theme=theme_update(),...,plot=NULL,verbose=F){
  if(!is.null(plot)){
    if(!inherits(plot,"gg") & !inherits(plot,"ggplot") & !inherits(plot,"ggtern")){
      stop("plot must inherit gg, ggplot or ggtern classes")
    }
  }
  if(!is.character(element))stop("element name must be specified as character")
  ret.plot  <- calc_element(element,theme=plot$theme,verbose=verbose)
  ret.theme <- calc_element(element,theme=theme,     verbose=verbose) 
  ifthenelse(!identical(ret.plot,NULL),ret.plot,ret.theme)
}

#' \code{find_global} is a function that conducts a named search for the \code{name} object instance, within the \code{env} environment. 
#' If an instance doesn't exist within the \code{env} environment, a search is then conducted within the \code{ggtern} and \code{ggplot2} 
#' namespaces \emph{(in that order)}. This is a modified version of the original source as provided in \code{ggplot2}, which has the same functionality, however, the modification is such that the function
#' now additionally searches within the \code{ggtern} namespace prior to the \code{ggplot2} namespace.
#' @param name character name of object to search for
#' @param env environment to search within as first priority
#' @examples find_global('scale_x_continuous')
#' @rdname undocumented
#' @return \code{find_global} returns an instance of the named object (if it exists), or \code{NULL} (if it does not).
find_global <- function (name, env=environment()){  
  if(!is.character(name)){stop("'name' must be provided as a character")}
  if(!inherits(environment(),"environment")){stop("'env' must inherit the environment class")}
  if (exists(name, env)){return(get(name, env))}
  nsenv <- asNamespace("ggtern")
    if(exists(name, nsenv)){return(get(name, nsenv))}
  nsenv <- asNamespace("ggplot2")
    if(exists(name, nsenv)){return(get(name, nsenv))}
  NULL
}


#' @section Attempt Transformation from Ternary to Cartesian Coordinates:
#' \code{trytransform} is an internal function which attempts to make ternary transformation. 
#' If fails, the original data is returned.
#' @param data the dataset
#' @param coord the coordinates
#' @keywords internal
#' @aliases trytransform
#' @return \code{trytransform} returns a \code{data.frame} object regardless of the success of the function operation.
#' @name   ternary_transformations
#' @rdname ternary_transformations
trytransform <- function(data,coord){
  if(missing(coord)){stop("coord are required")}
  bup <- data
  
  Tlim <- coord$limits$T; Llim <- coord$limits$L; Rlim <- coord$limits$R
  tryCatch({
    if(inherits(coord,"ternary")){  
      res <- transform_tern_to_cart(T=data[,coord$T],L=data[,coord$L],R=data[,coord$R],Tlim=Tlim,Llim=Llim,Rlim=Rlim)[,c("x","y")]
      data <- cbind(res,data[,which(!colnames(data) %in% c(coord$T,coord$L,coord$R))])
    }
  },error=function(e){
    warning(e)
    message(e)
    data <- bup
  })
  data
}

#select appropriate limits. internal
.select.lim <- function(a,b,default=c(0,1)){
  if(identical(a,default))a=waiver()
  if(identical(b,default))b=waiver()
  if(identical(a,b)){
    is.numericor(a,default)
  }else if(!is.numeric(a) & is.numeric(b)){
    b
  }else if(is.numeric(a) & !is.numeric(b)){
    a
  }else{
    a <- ifthenelse(is.numeric(a),a,default)
    b <- ifthenelse(is.numeric(b),b,default)
    c(min(a,b),max(a,b))
  }
}


#' \code{remove_outside} is a function that removes, from an input datases, all the rows wich are outside the ternary plot area
#' @param data data.frame
#' @rdname undocumented
remove_outside <- function(data){
  bup <- data
  lp <- last_plot()
  tryCatch({
    if(inherits(lp,"ggtern")){ #ONLY FOR ggtern object
      if(class(data) != "data.frame"){return(data)}
      if(length(which(c("x","y") %in% names(data))) != 2){warning("x and y are required"); return(data)}
      
      coord <- get_last_coord()
      lim <- list(Tlim=coord$limits[["T"]],Llim=coord$limits[["L"]],Rlim=coord$limits[["R"]])
      tri <- transform_tern_to_cart(data=get_tern_extremes(coord),Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)
      ix  <- point.in.polygon(data$x,data$y,tri$x,tri$y)
      return(data[which(ix > 0),])
    }
  },error=function(e){
    #do nothing
  })
  return(bup)
}

#' \code{sink_density} is a function which permits contours on the ternary surface, without running over the ternary borders.
#' @param df data.frame
#' @param remove boolean remove or make zero
#' @param coord coordinates of the type 'ternar', ie coord_tern()
#' @rdname undocumented
sink_density <- function(df,remove=TRUE,coord=stop("coord is required")){
  if(class(df) != "data.frame"){return(df)}
  bup <- df
  tryCatch({
    if(inherits(coord,"ternary")){ #ONLY FOR ggtern object
      tri <- transform_tern_to_cart(data=get_tern_extremes(coord),Tlim=coord$limits$T,Llim=coord$limits$L,Rlim=coord$limits$R)
      inorout <- point.in.polygon(df$x,df$y,tri$x,tri$y)
      inorout[which(inorout > 0)] <- 1
      if(remove){
        df <- df[which(inorout > 0),]
      }else{
        df[which(inorout <= 0),which(names(df) == "z")] <- 0
      }
    }
  },error=function(e){
    message(e)
    df <- bup
  })
  df
}

.hjust.flip    <- function(x,clockwise){if(clockwise){0.5 - (x - 0.5)}else{x}}
.zeroGrob <- grob(cl = "zeroGrob", name = "NULL")

# Euclidean distance between points.
# NA indicates a break / terminal points
.dist_euclidean <- function(x, y) {
  n <- length(x) 
  sqrt((x[-n] - x[-1]) ^ 2 + (y[-n] - y[-1]) ^ 2)
}

#' \code{check_required_aesthetics} is a local copy of the ggplot2 function that checks 
#' if the required aesthetics are present. This is used by geoms and stats to give a more helpful error message
#' when required aesthetics are missing.
#' @param character vector of required aesthetics
#' @param character vector of present aesthetics
#' @param name of object for error message
#' @keywords internal
#' @rdname overloaded
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()
  stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}


#' Give a deprecation error, warning, or messsage, depending on version number (ggtern version)
#'
#' Based on the exported function from ggplot2.
#' 
#' Version numbers have the format <major>.<minor>.<subminor>, like 0.9.2.
#' This function compares the current version number of ggplot2 against the
#' specified \code{version}, which is the most recent version before the
#' function (or other object) was deprecated.
#'
#' \code{tern_dep} will give an error, warning, or message, depending on the
#' difference between the current ggtern version and the specified
#' \code{version}.
#'
#' If the current major number is greater than \code{version}'s major number,
#' or if the current minor number is more than 1 greater than \code{version}'s
#' minor number, give an error.
#'
#' If the current minor number differs from \code{version}'s minor number by
#' one, give a warning.
#'
#' If the current subminor number differs from \code{version}'s subminor
#' number, print a message.
#'
#' @param version The last version of ggtern where this function was good
#'   (in other words, the last version where it was not deprecated).
#' @param msg The message to print.
tern_dep <- function(version, msg) {
  v <- as.package_version(version)
  cv <- packageVersion("ggtern")
  
  # If current major number is greater than last-good major number, or if
  #  current minor number is more than 1 greater than last-good minor number,
  #  give error.
  if (cv[[1,1]] > v[[1,1]]  ||  cv[[1,2]] > v[[1,2]] + 1) {
    stop(msg, " (Defunct; last used in version ", version, ")",
         call. = FALSE)
    
    # If minor number differs by one, give warning
  } else if (cv[[1,2]] > v[[1,2]]) {
    warning(msg, " (Deprecated; last used in version ", version, ")",
            call. = FALSE)
    
    # If only subminor number is greater, give message
  } else if (cv[[1,3]] > v[[1,3]]) {
    message(msg, " (Deprecated; last used in version ", version, ")")
  }
  
  invisible()
}








