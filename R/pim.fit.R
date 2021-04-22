#' Fitter function for a probabilistic index model
#'
#' This is the basic computing engine called by \code{\link{pim}}
#' to get the estimates for the coefficients and the variance-
#' covariance matrices. This function currently only spits out
#' these components using the sandwich estimators.
#'
#'
#' @param x a model matrix with as many rows as \code{y}.
#' @param y a vector with the pseudo-responses
#' @param link a character vector with a link function
#' @param estim a character vector or a function indicating the solver
#' to be used for estimating the coefficients. By default this is
#' the function \code{\link[nleqslv]{nleqslv}}. Other possibilities are
#' given in the help page on \code{\link{estimators}}.
#' @param start A numeric vector with the starting values for the fitting
#' algorithm, if required.
#' @param vcov.estim a function to determine the variance-covariance matrix.
#' Possibilities are \code{\link{sandwich.vcov}} and \code{link{score.vcov}}.
#' Defaults to \code{sandwich.vcov}
#' @param penv An environment, \code{\link{pim.environment}} or
#' \code{\link{pim.poset}} object containing the poset functions.
#' Alternatively this can be a list of two numeric vectors, containing the
#' poset indices for the left and right side of the pim.
#'
#' @param weights currently not implemented
#' @param ... Further arguments that need to be passed to
#' the estimation function. The most relevant is \code{construct},
#' allowing you to write your own score function for
#' the numerical optimization. See also \code{\link{estimators}}
#'
#' @return A list with the following elements
#' \describe{
#'  \item{coefficients}{a numeric vector with the coefficients}
#'  \item{vcov}{a numeric matrix with the variance-covarianc matrix for
#'  the coefficients}
#'  \item{fitted}{a numeric vector with the raw fitted values}
#'  \item{estim}{a list with two components named \code{coef} and \code{vcov}
#'  containing information on the used estimators for both.}
#' }
#'
#' @seealso \code{\link{model.matrix}} for how to construct a valid model matrix
#' for a pim, \code{\link{pim}} for the general user interface
#'
#' @export
pim.fit <- function(x,y,link = "logit",
                    estim = 'estimator.nleqslv',
                    start = rep(0,ncol(x)),
                    vcov.estim = 'sandwich.vcov',
                    weights = NULL,
                    penv,
                    ...
                    )
{
  estimF <- match.fun(estim)
  vcov.estimF <- match.fun(vcov.estim)
  # if(! is.null(weights)) {print("flag: pim.fit.R")}
  res <- estimF(x, y, link = link, weights = weights, start=start, ...)

  fits <- x %*% res$coef
  dim(fits) <- NULL

  if(!is.list(penv)) penv <- poset(penv, as.list=TRUE)

  vc <- vcov.estimF(fits, x, y, weights, link, penv)

  return(list(coefficients = res$coef,vcov = vc, fitted=fits,
              estim = list(coef = estim, vcov = vcov.estim)))

}
