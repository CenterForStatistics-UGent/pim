#' Check whether a pim environment is complete
#'
#' Objects of class \code{\link{pim.environment}} can be created with
#' or without a poset. To check whether an object has a poset included,
#' you use the function \code{is.complete}
#'
#' @param x an object of class \code{\link{pim.environment}}
#'
#' @return a single value TRUE or FALSE
#'
#' @examples
#' # the constructor returns an empty environment without poset
#' is.complete(new.pim.env())
#'
#' # Constructing a pim environment with a poset
#' data("FEVData")
#' FEVenv <- new.pim.env(FEVData, compare="unique")
#' is.complete(FEVenv)
#'
#' @export
is.complete <- function(x){
  if(inherits(x,'pim.environment')){
    x@is.complete
  } else {
    stop('x needs to be a pim.environment')
  }
}
