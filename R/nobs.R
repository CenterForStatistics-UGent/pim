#' Extract the number of observations
#'
#' This function extracts the number of observations in an object
#' of class \code{\link{pim.environment}} or \code{\link{pim}},
#' or the number of observations
#' for which a \code{\link{pim.poset}} is constructed. If applied to
#' a matrix or data.frame, it returns the number of rows.
#' For any other object it
#' does the same as \code{\link{length}}.
#'
#' @note This package imports the generic \code{\link[stats]{nobs}} from the package
#' \code{\link{stats4}}.
#'
#' @param object an object of the class \code{\link{pim.environment}} or \code{\link{pim.poset}}
#' @param ... arguments passed to other methods.
#'
#' @return In case the function is called on a \code{pim.environment}
#' or a \code{pim.poset} object,
#' an integer with the number of (foreseen) observations. If the
#' pim.environment is empty, it returns \code{0}.
#'
#' In all other cases, it returns the output of either \code{\link{nrow}} (for
#' matrices and data.frames)
#' or \code{\link{length}}.
#'
#' @seealso \code{\link[stats]{nobs}} for the general method for fit objects.
#'
#' @examples
#' data('FEVData')
#' Model <- pim(FEV~ Smoke*Sex , data=FEVData)
#' nobs(FEVData)
#' nobs(Model)
#' # Get the pim environment out of the model:
#' theenv <- penv(Model)
#' nobs(Model)
#'
#' @include pim.environment-class.R
#' @export
#' @name nobs

#' @rdname  nobs
setMethod("nobs",
          signature = "pim.environment",
          function(object){
            if(identical(object@nobs,integer(0))){
              0
            } else {
              object@nobs
            }
          })

#' @rdname nobs
setMethod("nobs",
          signature = "pim",
          function(object){
            out <- object@penv@nobs
            if(identical(out, integer(0)))
              0
            else
              out
          })

#' @rdname nobs
setMethod("nobs",
          signature="pim.poset",
          function(object){
            object@nobs
          })

#' @rdname nobs
setMethod("nobs",
          signature="matrix",
          function(object){
            nrow(object)
          })

#' @rdname nobs
setMethod("nobs",
          signature="data.frame",
          function(object){
            nrow(object)
          })

