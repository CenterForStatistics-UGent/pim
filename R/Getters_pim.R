#' Getters for slots of a pim object
#' 
#' @param x a pim object
#' @param object a pim object
#' @param ... arguments passed to other methods. Currently ignored.
#' 
#' @rdname pim-getters
#' @name pim-getters
#' @aliases keep.data getters-pim
#' 
#' @return \code{keep.data()}: a single logical value indicating whether
#' the model matrix and pseudo responses were stored in the 
#' \code{\link{pim}} object.
#' 
#' @examples
#' data('FEVData')
#' themodel <- pim(FEV ~ Age + Height, data = FEVData)
#' keep.data(themodel)
#' fitted(themodel)
#' 
#' @include pim-class.R
#' @export
keep.data <- function(x){
  if(!inherits(x,"pim"))
    stop("Object is not a pim object")
  else
    x@keep.data
}

#' @rdname pim-getters
#' @export
setGeneric("fitted")

fitted.pim <- function(object, ...){
  object@fitted
}
#' @rdname pim-getters
#' @return \code{fitted()}: a numeric vector with the fitted
#' values for the pseudo-observations. 

setMethod("fitted",
          signature = "pim",
          fitted.pim)


