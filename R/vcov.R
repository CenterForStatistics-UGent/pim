#' Methods for vcov
#'
#' Extract the variance-covariance matrix from a pim model.
#' This package defines an S4 generic for \code{\link[stats]{vcov}} and methods for list and pim classes. Other than that, \code{vcov} works as described in
#' \code{stats::\link[stats]{vcov}}.
#'
#' @param object any object.
#' @param ... arguments passed to other methods. Currently ignored
#'
#' @return A matrix with the variance-covariance values
#'
#' @seealso \code{stats::\link[stats]{vcov}} in the stats package.
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
