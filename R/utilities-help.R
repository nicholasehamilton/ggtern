.firstUpper <- function(s) {paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")}

.aesthetics <- function(x) {
  req_aes <- x$required_aes
  def_aes = NULL
  tryCatch({
    def_aes <- names(x$default_aes())
  },error=function(e){
    #nothing
  })
  def_aes <- setdiff(def_aes, req_aes)
  if (length(req_aes) == 0){
    # Suppress warnings which occur when sorting NULL
    return(suppressWarnings(sort(names(x$default_aes()))))
  }
  if (length(def_aes) == 0){
    return(paste("\\strong{", sort(x$required_aes), "}",sep = ""))
  }
  return(c(paste("\\strong{", sort(x$required_aes), "}", sep = ""), sort(def_aes)))
}

.geom_aesthetics <- function(x) {
  fullname = paste0("Geom",.firstUpper(x))
  .aesthetics(get(fullname))
}

.stat_aesthetics <- function(x) {
  fullname = paste0("Stat",.firstUpper(x))
  .aesthetics(get(fullname))
}

.coord_aesthetics <- function(x){
  fullname = paste0("coord_",x)
  obj <- do.call("coord_tern",args=list())
  .aesthetics(obj)
}

#' \code{rd_aesthetics} is a helper function for documenting aesthetics in R help files.
#' @param type geom, stat or coord
#' @aliases undocumented internal
#' @rdname undocumented
rd_aesthetics <- function(type, name) {
  if(toupper(type)=="GEOM"){
    aes = .geom_aesthetics(name)
  }else if(toupper(type)=="STAT"){
    aes = .stat_aesthetics(name)
  }else if(toupper(type)=="COORD"){
    aes = .coord_aesthetics(name)
  }else{
    obj <- get(.firstUpper(type))
    aes <- .aesthetics(obj$find(name))
  }
  
  paste("\\code{", type, "_", name, "} ",
        "understands the following aesthetics (required aesthetics are in bold):\n\n",
        "\\itemize{\n",
        paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
        "\n}\n", sep = "")
}

#' \code{rd_theme} is a helper function for documenting theme_elements in R help files.
#' @rdname undocumented
rd_theme <- function(){
  dic <- ggint$.element_tree[which(!ggint$.element_tree %in% ggint$.element_tree.orig)]
  nam <- names(dic)
  nolink <- c()
  paste(
    "\nBased on the \\code{ggplot2} existing structure (\\link[ggplot2]{theme}), the \\strong{NEW} individual theme elements for the ternary plot are as follows:",
    "\\tabular{lll}{",
    "\\strong{NAME} \\tab \\strong{OBJECT}/(INHERITS) \\tab \\strong{DESCRIPTION} \\cr",
    paste(sapply(nam,function(x){
      obj  = dic[[x]]
      paste("\\code{",x,"} \\tab \\code{",
            ifthenelse(obj$class %in% nolink,obj$class,paste0("\\link{",obj$class,"}")) ,
            "}",
            ifthenelse(!is.null(obj$inherit),"/(",""),"",
            obj$inherit,
            ifthenelse(!is.null(obj$inherit),")",""),
            " \\tab ",obj$description,sep="")
    }),collapse="\\cr "),
    "\n}\n"
  )
}

