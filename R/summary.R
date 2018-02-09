#' The summary function for the pim class
#'
#' The function \code{\link[base]{summary}} is a generic function. We provide
#' a method for objects of the \code{\link{pim-class}}. This method
#' returns the estimates, standard error and an asymptotic test
#' based to define whether the estimates differ significantly from zero.
#' This test assumes an asymptotic normal distribution of the estimates.
#'
#' The null hypothesis used in these tests is set automatically in the case
#' \code{h0 = NULL}. By default it is set to 0 for all parameters, but
#' this might not always make sense. In the case of an identity link and
#' a single binary predictor variable, the estimate of the
#' intercept should be tested against 0.5 instead of 0.
#'
#' @param object an object of the class pim.
#' @param h0 a numeric value or a vector as long as the number of coefficients
#' with the value that defines the null hypothesis to test against. See Details.
#' @param ... arguments passed to other methods. Currently ignored.
#'
#' @return a \code{\link{pim.summary}} object with the estimate, standard
#' error, Z value and corresponding p value.
#'
#' @seealso \code{\link{as.matrix.pim.summary}} for converting this summary
#' to a matrix.
#'
#' @examples
#' data(FEVData)
#' Model <- pim(FEV~ Age + Smoke*Sex , data=FEVData)
#' summ <- summary(Model)
#'
#' # Value can be extracted like from a matrix
#' summ["Smoke",] # To extract everything from the estimate for smoke
#' summ[, 4] # To extract all p-values
#'
#' @rdname summary.pim
#' @name summary.pim
#' @aliases summary
#' @export
setGeneric('summary')

summary.pim <- function(object, h0 = NULL,
                        ...){
  coefs <- coef(object)

  if(is.null(h0)){
    if(link(object) == "identity"){
      warning(paste("Null hypothesis is set to 0, but link is identity.",
                    "The performed tests may be meaningless."))
      h0 = 0 # TODO: set h0 correctly. When 0.5?
    }else {
      h0 = 0
    } # END ifelse link == identity
  } # END if is null

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
