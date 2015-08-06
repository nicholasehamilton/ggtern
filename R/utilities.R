#' \code{ifthenelse} function takes input arguments \code{x}, \code{a} and \code{b} and returns \code{a} if \code{x} is \code{TRUE}, else, returns \code{b}
#' @param x logical input to check
#' @param a value to return if \code{x} is TRUE
#' @param b value to return if \code{x} is FALSE
#' @rdname undocumented
ifthenelse <- function(x,a,b){
  if(!is.logical(x))stop("x argument must be logical")
  if(x){a}else{b}
}

#' \code{is.numericor} function takes input arguments \code{A} and \code{B} and returns \code{A} if \code{A} is numeric, else, returns \code{B}
#' @param A value to return if \code{A} is numeric
#' @param B value to return if \code{A} is NOT numeric
#' @rdname undocumented
is.numericor <- function(A,B){
  if(missing(B)){stop("b must be provided")}
  if(!is.numeric(B)){stop("b must be numeric")}
  ifthenelse(is.numeric(A),A,B)
}
"%||%" <- function(a, b) {if (!is.null(a)) a else b}

#' \code{get_tern_extremes} determines the limiting ternary coordinates given input coordinates.
#' @param coordinates ggtern coordinate system, inheriting "ternary" and "coord" classes.
#' @param verbose logical indicating verbose reporting to console
#' @param expand numeric do define the max and min acceptable limits above and below the intended range.
#' @rdname undocumented
get_tern_extremes <- function(coordinates,verbose=F,expand=0){
  expand = is.numericor(expand[1],0); 
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
    writeLines("ATTENTION: Non-Default Ternary Limits are outside the range [0,1]")
    print(report.data)
    stop("Negative Values, or Values > 1 are Not Acceptable",call.=FALSE)
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
#' @aliases transform_tern_to_cart transform_cart_to_tern
transform_tern_to_cart <- function(T,L,R,data=data.frame(T=T,L=L,R=R),...,Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1),scale=TRUE){
  if(class(data) != "data.frame")stop("data must be of type 'data.frame'")
  if(length(which(c("T","L","R") %in% colnames(data))) < 3) stop("data must contain columns T, L and R")
  
  Tlim <- sort(Tlim); Rlim <- sort(Rlim); Llim <- sort(Llim)
  
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
  .adj <- function(input,lim){
    if(is.null(lim)) lim=c(0,1)
    (input-min(lim))/(abs(diff(lim)))
  }
  d$T <- .adj(d$T,Tlim)
  d$L <- .adj(d$L,Llim)
  d$R <- .adj(d$R,Rlim)
  
  #Calculate
  out.Y <- d$T*tan(pi/3)*0.5
  out.X <- d$R + out.Y*tan(pi/6)
  return(data.frame(x=out.X,y=out.Y))
}
transform_cart_to_tern <- function(x,y,data=data.frame(x=x,y=y),...,Tlim=c(0,1),Llim=c(0,1),Rlim=c(0,1)){
  tryCatch({
    if(length(which(c("x","y") %in% colnames(data))) < 2) stop("data must contain columns x and y")
    out.R = data$x - data$y*tan(pi/6)
    out.T = data$y/(tan(pi/3)*0.5)
    out.L = 1 - out.R - out.T
    
    #Undo Scale
    .adj.rev <- function(input,lim){
      input*(abs(diff(lim))) + min(lim)
    }
    out.T = .adj.rev(out.T,Tlim)
    out.L = .adj.rev(out.L,Llim)
    out.R = .adj.rev(out.R,Rlim)
    
    data.frame(T=out.T,L=out.L,R=out.R)
  },error=function(e){
      return(data)
  })
}

.makevalid <- function(x){
  x = x[[1]]
  if(class(x) == 'character'){
    x = gsub("%","'%'",x)
    #x = gsub("/","'/'",x)
    x = gsub('([[:punct:]])\\1+', '\\1', x)
    x = gsub(" ","~",x)
  }
  x
}

#' \code{arrow_label_formatter} is a function that formats the labels directly adjacent to the ternary arrows.
#' @param label character label
#' @param suffix chacater suffix behind each label
#' @param sep the seperator between label and suffix 
#' @rdname undocumented
arrow_label_formatter <- function(label,suffix="",...,sep="/"){
  if(missing(label))stop("label cannot be missing")
  label = .makevalid(label)
  suffix= .makevalid(suffix)
  sep   = .makevalid(sep)
  sep   = ifthenelse(identical(suffix,""),"",sep)
  ret   = label #default
  if(identical(suffix,NULL) | missing(suffix)){
    ret = arrow_label_formatter(label=label,sep=sep,suffix="") #recursive
  }else{
    tryCatch({
      ret   = ifthenelse(class(label) == 'call',as.expression(ret),ret)
      ret   = parse(text=paste(ret,sep,gsub(x=suffix,pattern=" ",replacement="~")))
    },error=function(e){
      message(e) #to console
    })
  }
  return(ret) #result
}

#' \code{calc_element_plot} Calculates the element properties, by inheriting properties from its parents, 
#' and compares to whether the local plot overrides this value. Based largely off the calc_element function as provided
#' in ggplot2
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

#' \code{find_global_tern} is a function that conducts a named search for the \code{name} object instance, within the \code{env} environment. 
#' If an instance doesn't exist within the \code{env} environment, a search is then conducted within the \code{ggtern} and \code{ggplot2} 
#' namespaces \emph{(in that order)}. This is a modified version of the original source as provided in \code{ggplot2}, which has the same functionality, however, the modification is such that the function
#' now additionally searches within the \code{ggtern} namespace prior to the \code{ggplot2} namespace.
#' @param name character name of object to search for
#' @param env environment to search within as first priority
#' @rdname undocumented
find_global_tern <- function (name, env=environment()){  
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
  tryCatch({
    if(inherits(coord,"ternary")){ 
      Tlim <- coord$limits$T; Llim <- coord$limits$L; Rlim <- coord$limits$R
      res <- transform_tern_to_cart(T=data[,coord$T],L=data[,coord$L],R=data[,coord$R],Tlim=Tlim,Llim=Llim,Rlim=Rlim)[,c("x","y")]
      return(cbind(res,data[,which(!colnames(data) %in% c(coord$T,coord$L,coord$R))]))
    }
  },error=function(e){
  })
  bup
}

#' \code{notransform} is an internal function that permits the input argument (evaluate) to be executed with temporary disabling of
#' ternary transformations, existing state is restored
#' @param evaluate code to execute in a protected block from transformation
#' @keywords internal
notransform <- function(evaluate){
 existing <- getOption("tern.dont_transform")    
 options("tern.dont_transform" = TRUE)
 res <- evaluate
 options("tern.dont_transform" = existing)
 invisible(res)
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
      ix  <- sp::point.in.polygon(data$x,data$y,tri$x,tri$y)
      return(subset(data,ix > 0))
    }
  },error=function(e){
    #do nothing
  })
  return(bup)
}

.hjust.flip    <- function(x,clockwise){if(clockwise){0.5 - (x - 0.5)}else{x}}
.zeroGrob      <- grob(cl = "zeroGrob", name = "NULL")

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
#' @keywords internal
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

#' Switch between two values for the ternary system
#' 
#' Returns yes if the current system is ternary, and no if it is not
#' @param yes value to return when the current system is ternary
#' @param no value to return when the current system is not
#' @keywords internal
#' 
iflasttern <- function(yes=stop("yes value required"),no=stop("no value required"))
  ifthenelse(inherits(get_last_coord(),"ternary"),yes,no)

#' Undo a Ternary Transformation
#' 
#' Using the provided ternary coordinates, and a data-frame containing x, y, z values, convert back
#' to the cartesian coordinates
#' @param df data frame containing x, y, z
#' @param coord ternary coordinate system
undoCartesian <- function(df,coord){
  if(inherits(coord,"ternary")){
    ix = as.character(unlist(coord[c("T","L","R")])) 
    df[,ix] <- transform_cart_to_tern(data=df,Tlim=coord$limits$T,Llim=coord$limits$L,Rlim=coord$limits$R)
  }
  df
}

#' Expand a Range of Values
#' 
#' Expands a range of values about its midpoint, by an amount equal to the multiplyer
#' @param x range of values, required to be numeric and of length 2
#' @param m multiplyer
expandRange <- function(x,m=1){
  if(!is.numeric(x) | length(x) != 2) x = c(0,1)
  if(diff(x)   == 0)return(x)
  med = mean(range(x))
  c(med + (x[1] - med)*m[1],
    med + (x[2] - med)*m[1])
}


#' Ternary Limits
#' 
#' Determine the Ternary Limits for Coordinate System
#' @param coord ternary coordinates
ternLimitsForCoord = function(coord){
  lapply(list(Tlim="T",Llim="L",Rlim="R"),function(x){coord$limits[[x]]} )
}


#' Determine Ternary Expansion
#' 
#' Determines the Expansion Buffer on the Ternary Sufrace, 
#' Similar to the 'expand' term in ggplot2 on a rectangular grid
#' @param coord ternary coordinates
#' @param by fraction to expand by
#' @return numeric scalar, of the amount to expand ternary grid
expandTern <- function(coord,by=getOption('tern.expand')){
  tryCatch({
    if(inherits(coord,'ternary')){
      lim = ternLimitsForCoord(coord)
      if(is.numeric(by)) return(max(by)*max(sapply(lim,function(x){
        if(is.null(x) | !is.numeric(x)){ x = c(0,1) }
        diff(x)
      })))
    }
  },error=function(e){
    #pass
  })
  return(by)
}

#' Suppress Colours
#'
#' Function to Suppress Colours
#' @param data ggplot dataframe
#' @param coord ternary coordinates
#' @param toColor the colour to suppress to
#' @param remove should suppressed points be removed entirely
suppressColours <- function(data,coord,toColor="transparent",remove=FALSE){
  if(!inherits(coord,"ternary")) return(data)
  if(!getOption('tern.discard.external')) return(data)
  if(class(data)  != 'data.frame') return(data)
  ix.tern       <- c("T","L","R"); 
  ix.cart       <- c("x","y")
  ix.tern.n     <- c(coord$T,coord$L,coord$R)
  lim           <- ternLimitsForCoord(coord)
  expand        <- expandTern(coord,by=getOption('tern.expand.contour.inner'))
  xtrm          <- get_tern_extremes(coord,expand=expand)[,ix.tern]
  data.extremes <- transform_tern_to_cart(data = xtrm,Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,ix.cart]
  if(all(ix.tern.n %in% names(data))){
    rnm         = function(x){x=x[,ix.tern.n];names(x)=ix.tern;x}
    data.cart   = transform_tern_to_cart(data=rnm(data), Tlim = lim$Tlim,Llim = lim$Llim,Rlim = lim$Rlim)[,ix.cart]
    in.poly     = sp::point.in.polygon(data.cart$x,data.cart$y,as.numeric(data.extremes$x),as.numeric(data.extremes$y))
    outside.ix  = intersect(1:nrow(data),which(in.poly %in% c(0,2,3)))
    if(length(outside.ix) > 0){ 
      if(remove){
        data = data[-outside.ix,]
      } else{
        for(x in c('colour')){
          if(x %in% names(data)){
            data[outside.ix,x] = toColor
          }
        }  
      }
    }
  }
  data
}

#' Enforce Coordinates
#' 
#' Function to Enforce Coordinates
enforceTernaryCoordinates <- function(){
  coordinates  <- get_last_coord()
  if(!inherits(coordinates,"ternary")) stop("Coordinates Must be Ternary.")
  return(coordinates)
}

#' Get Number of Breaks
#' 
#' Calculates the Breaks for Major or Minor Gridlines
#' based on the input limits.
#' @param limits the scale limits
#' @param isMajor major or minor grids
#' @param nMajor number of major breaks
#' @param nMinor number of minor breaks
getBreaks <- function(limits,isMajor,nMajor=5,nMinor=2*nMajor){
  if(is.null(limits)){ limits = c(0,1) }
  if(!all(is.numeric(limits))){ limits=c(0,1) }
  if(diff(range(limits)) == 0){
    return(if(isMajor){getOption("tern.breaks.default")}else{getOption("tern.breaks.default.minor")})
  }else{
    ret   = pretty(limits,n=nMajor)
    if(!isMajor){
      r = range(ret)
      d = diff(r)/(length(ret)-1)
      minor = seq(min(ret)-d,max(ret)+d,by=d/2)
      minor = minor[which(minor >= min(limits) & minor <= max(limits))]
      ret   = minor[which(!minor %in% ret)]
    }
    ret = ret[which(!ret %in% min(limits))]
    ret
  }
}







