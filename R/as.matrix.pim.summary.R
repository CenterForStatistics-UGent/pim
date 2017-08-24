#' Convert a pim.summary object to a matrix
#' 
#' This function converts a summary object to a matrix so values can be
#' extracted from it.
#' 
#' @param x a \code{pim.summary} object
#' @param ... additional arguments to be passed to or from methods.
#' This one is ignored.
#' 
#' @return a matrix with the estimate, standard error, Z value and
#' probability for every coefficient. 
#' 
#' @include pim.summary-class.R
#' @rdname as.matrix.pim.summary
#' @name as.matrix.pim.summary
#' @aliases as.matrix
#' @export
setGeneric("as.matrix")

as.matrix.pim.summary <- function(x, ...){
  cbind(
    Estimate = coef(x),
    "Std. Error" = x@se,
    "z value" = x@zval,
    "Pr(>|z|)" = x@pr
  )
}

#' @rdname as.matrix.pim.summary
setMethod("as.matrix",
          "pim.summary",
          as.matrix.pim.summary)

#' @rdname as.matrix.pim.summary
setMethod("as.matrix",
          "pim",
          function(x, ...){
            as.matrix(summary(x))
          }
          )
