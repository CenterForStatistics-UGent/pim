#' Calculate Wald confidence intervals around the coefficients of a PIM
#' 
#' This function returns Wald confidence intervals around the 
#' coefficients of a fitted \code{\link{pim}} object
#' 
#' @param object a \code{\link{pim}} or \code{\link{pim.summary}} object 
#' @param parm a specification of which parameters are to be given confidence intervals. Either a vector of numbers or a vector of names.
#' If missing, all parameters are considered
#' @param level The confidence level required.
#' @param ... extra arguments to methods
#' 
#' @rdname confint.pim
#' @name confint.pim
#' @aliases confint.pim.summary confint
#' @export
#' 
setGeneric("confint")

confint.pim <- function(object, parm, level = 0.95, ...){
  # This code is almost literally copied from confint.default
  # because that one doesn't work with S4
  # kinda stupid, but well... 
  cf <- coef(object)
  pnames <- names(cf)
  if(missing(parm))
    parm <- pnames
  else if(is.numeric(parm))
    parm <- pnames[parm]
  
  a <- (1-level)/2
  a <- c(a, 1-a)
  
  pct <- format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

confint.pim.summary <- function(object, parm, level = 0.95, ...){
  # This code is almost literally copied from confint.default
  # because that one doesn't work with S4
  # kinda stupid, but well... 
  cf <- coef(object)
  pnames <- names(cf)
  if(missing(parm))
    parm <- pnames
  else if(is.numeric(parm))
    parm <- pnames[parm]
  
  a <- (1-level)/2
  a <- c(a, 1-a)
  
  pct <- format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ses <- object@se[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


#' @rdname confint.pim
#' @export 
setMethod('confint',
          'pim',
          confint.pim)

#' @rdname confint.pim
#' @export
setMethod('confint',
          'pim.summary',
          confint.pim.summary)
