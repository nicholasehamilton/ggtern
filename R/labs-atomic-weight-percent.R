#' Atomic, Weight or Custom Percentage Suffix
#' 
#' By default there are no suffixes behind the arrow label marker (the arrow up next to the ternary axes), 
#' and these functions appends to the set of arrow labels, a value to indicate the nature of the scale.
#' 
#' These are convenience wrappers to \code{labs(W="XYZ")}.

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' @description \code{percent_weight} adds 'Wt. \%' to the arrow marker label as a suffix
#' @rdname labsatomicweightpercent
#' @examples
#' #Create Dummy Plot
#' plot <- ggtern(data=data.frame(THS=1,LHS=0,RHS=0),aes(x=THS,y=LHS,z=RHS))
#' 
#' #Add the Various Suffixes
#' plot + percent_weight()
#' plot + percent_atomic()
#' plot + percent_custom("Percentage")
#' @export
percent_weight <- function(){labs(W="Wt. %")}

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' @description \code{weight_percent} is an alias for \code{percent_weight()}
#' @rdname labsatomicweightpercent
#' @export
weight_percent <- percent_weight

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' 
#' @description \code{percent_atomic} adds 'At. \%' to the arrow marker label as a suffix
#' @rdname labsatomicweightpercent
#' @export
percent_atomic <- function(){labs(W="At. %")}

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' 
#' @description \code{atomic_percent} is an alias for \code{percent_atomic()}
#' @rdname labsatomicweightpercent
#' @export
atomic_percent <- percent_atomic

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' 
#' @description \code{percent_custom} adds a custom suffix to the arrow label marker.
#' @rdname labsatomicweightpercent
#' @export
percent_custom <- function(x){labs(W=as.character(x))}

#' Atomic, Weight or Custom Percentage Suffix
#' 
#' 
#' @description \code{custom_percent} is an alias for \code{percent_custom()}
#' @rdname labsatomicweightpercent
#' @export
custom_percent <- function(x){labs(W=as.character(x))}

#Create Dummy Plot
#plot <- ggtern(data=data.frame(THS=1,LHS=0,RHS=0),aes(x=THS,y=LHS,z=RHS))
#Add the Various Suffixes
#$plot + percent_weight()


