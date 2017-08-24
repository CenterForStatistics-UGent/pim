#' The summary function for the pim class
#' 
#' The function \code{\link[base]{summary}} is a generic function. We provide
#' a method for objects of the \code{\link{pim-class}}. 
#' 
#' @param object an object of the class pim.
#' @param h0 a numeric value or a vector as long as the number of coefficients
#' with the value that defines the null hypothesis to test against
#' @param ... arguments passed to other methods. Currently ignored.
#' 
#' @return a \code{\link{pim.summary}} object
#' 
#' @examples 
#' data(FEVData)
#' Model <- pim(FEV~ Age + Smoke*Sex , data=FEVData)
#' summary(Model)
#' 
#' @rdname summary.pim
#' @name summary.pim
#' @aliases summary
#' @export
setGeneric('summary')

summary.pim <- function(object, h0 = 0,
                        ...){
  coefs <- coef(object)
  
  se <- sqrt(diag(vcov(object)))
  zval <- (coefs - h0) / se
  pr <- 2*pnorm(-abs(zval))
  
  new("pim.summary",
      formula=formula(object),
      model = model(object),
      link = link(object),
      coef = coefs,
      se = se,
      zval = zval,
      pr = pr,
      h0 = h0
  )
}

#' @rdname summary.pim
setMethod('summary',
          signature='pim',
          summary.pim)
