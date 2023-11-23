#' Extract the coefficients from a pim or pim.summary object
#' 
#' This function works like \code{\link[stats]{coef}} from the \code{stats}
#' package. It extracts the coefficients from the objects.
#' 
#' @param object a \code{pim} or \code{pim.summary} object
#' @param ... currently ignored.
#' 
#' @return a named vector with the coefficients. 
#' 
#' @examples 
#' data("FEVData")
#' Model <- pim(FEV~ Age + Smoke*Sex , data=FEVData)
#' coef(Model)
#' summ <- summary(Model)
#' coef(summ)
#' 
#' @rdname coef
#' @include pim-class.R pim.summary-class.R
#' @export


setGeneric('coef')

#' @method coef pim
#' @export
coef.pim <- function(object,...){
  object@coef
}

coef.pim.summary <- function(object,...){
  object@coef
}

#' @rdname coef
setMethod('coef',
          'pim',
          coef.pim)

#' @rdname coef
setMethod('coef',
          'pim.summary',
          coef.pim.summary)