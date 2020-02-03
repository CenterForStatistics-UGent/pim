#' Methods for vcov
#'
#' This package defines an S4 generic for \code{\link[stats]{vcov}} and methods for list and pim classes.
#'
#' @param object any object.
#' @param ... arguments passed to other methods. Currently ignored
#'
#' @return the variance-covariance matrix
#'
#' @seealso \code{\link[stats]{vcov}} in the stats package.
#'
#' @examples
#' data(FEVData)
#' Model <- pim(FEV~ Age + Smoke*Sex , data=FEVData)
#' vcov(Model)

#' @docType methods
#' @include pim-class.R
#' @export

# There is no help page for vcov here, as it behaves as shown on the
# help page of \code{\link[stats]{vcov}} in the stats package.

setGeneric('vcov')

vcov.pim <- function(object,...){
  object@vcov
}

#' @rdname vcov
setMethod('vcov',
          'pim',
          vcov.pim)

vcov.list <- function(object,...){
  object$vcov
}

#' @rdname vcov
setMethod('vcov',
          'list',
          vcov.list)
