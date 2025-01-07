#' Extract method for pim.summary objects
#'
#' This method allows to extract data directly from a \code{\link{pim.summary}}
#' object. It's exactly the same as extracting from \code{as.matrix(thesummary)}.
#'
#' @param x a \code{\link{pim.summary}} object to extract information from.
#' @param i indices specifying row to extract or replace. They are used in the same way as you woul after transforming \code{x} to a matrix.
#' @param j see i, but for columns
#' @param drop see \code{\link[base:Extract]{Extract}}
#'
#' @return the selected matrix
#'
#' @examples
#' data(FEVData)
#' Model <- pim(FEV~ Smoke*Sex , data=FEVData)
#'
#' thesummary <- summary(Model)
#' thesummary[,2:3]
#' thesummary["Sex"]
#'
#' @docType methods
#' @include pim.summary-class.R
#' @rdname Extract.pim.summary
#' @name Extract.pim.summary
#' @aliases [,pim.summary-method
#' @export
setMethod("[",
          "pim.summary",
          function(x,i,j,drop = TRUE){
            as.matrix(x)[i,j,drop = drop]
          })
