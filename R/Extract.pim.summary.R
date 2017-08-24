#' Extract method for pim.summary objects
#' 
#' This method allows to extract data directly from a \code{\link{pim.summary}}
#' object. It's exactly the same as extracting from \code{as.matrix(thesummary)}.
#' 
#' @inheritParams base::Extract
#' @param j see i
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
