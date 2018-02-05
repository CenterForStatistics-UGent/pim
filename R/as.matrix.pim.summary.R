#' Convert a pim.summary object to a matrix
#'
#' This function converts a summary object to a matrix so values can be
#' extracted from it. This function is provided mainly for internal use.
#' One can extract immediately from a \code{\link{pim.summary}} object
#' using standard matrix extraction.
#'
#' @param x a \code{pim.summary} object
#' @param ... additional arguments to be passed to or from methods.
#' This one is ignored.
#'
#' @return a matrix with the estimate, standard error, Z value and
#' probability for every coefficient.
#'
#' @seealso \code{\link{Extract.pim.summary}} for more information on
#' extracting from a \code{pim.summary} object.
#'
#' @examples
#' data(FEVData)
#' Model <- pim(FEV~ Smoke*Sex , data=FEVData)
#'
#' thesummary <- summary(Model)
#' as.matrix(thesummary)
#'
#' @include pim.summary-class.R
#' @rdname as.matrix.pim.summary
#' @name as.matrix.pim.summary
#' @aliases as.matrix
#' @export
setGeneric("as.matrix")

as.matrix.pim.summary <- function(x, ...){
  out <- cbind(x@coef,
               x@se,
               x@zval,
               x@pr)
  colnames(out) <- c("Estimate",
                     "Std. Error",
                     "z value",
                     "Pr(>|z|)")
  out
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
