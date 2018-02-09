#' Fitter function for a probabilistic index model
#'
#' This is the basic computing engine called by \code{\link{pim}}
#' to get the estimates for the coefficients and the variance-
#' covariance matrices, based on the model matrix and the pseudo-responses
#' of the model. It allows you finer control over the actual procedure
#' used to fit the model, but most parameters can be controlled by using
#' the main modeling function \code{\link{pim}}.
#'
#' The parameters \code{estim} and \code{vcov.estim} allow you to
#' plug in your own optimalization code. The package provides a couple
#' of \code{\link{estimators}} based on different other packages, but
#' one can write similar functions if one wants to use a different
#' optimization algorithm.
#'
#' In case you write your own, the function should take the exact same
#' arguments as those described in \code{\link{estimators}}. More
#' specifically, the arguments \code{x}, \code{y}, \code{start} and
#' \code{link} are passed -in that order- to these functions.
#'
#' When providing your own estimator or your own score function (see
#' also \code{\link{CreateScoreFun}}), you have to make sure it is
#' either compatible with the estimator for the variance-covariance
#' matrix or you have to provide your own estimator there as well.
#' This estimator should take an extra argument \code{poset} needed to
#' pass a list with the poset (see also \code{link{poset}}). The
#' function \code{pim.fit} extracts this list from the value of
#' penv.
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
#' @param bias.reduced a logical value indicating whether or not the
#' bias reduced version of the pim should be fitted.
#'
#' @param ... Further arguments that need to be passed to
#' the estimation function. The most relevant is \code{construct},
#' allowing you to write your own score function for
#' the numerical optimization. See also \code{\link{estimators}}.
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
#' for a pim, \code{\link{pim}} for the general user interface.
#'
#' @examples
#' data("FEVData")
#' # Create the "model frame"
#' FEVenv <- new.pim.env(FEVData, compare="unique")
#' # This includes the poset
#' pos <- poset(FEVenv, as.list=TRUE)
#'
#' # create the formula and bind it to the pim.environment.
#' FEVform <- new.pim.formula(
#'   Age ~ I(L(Height) - R(Height))  ,
#'   FEVenv
#' )
#'
#' # Use this formula object to construct the model matrix
#' # use the default model ( difference )
#' MM <- model.matrix(FEVform)
#'
#' # Use this formula object to construct the pseudo response
#' Y <- response(FEVform)
#'
#' # Now pim.fit can do what it does
#' res <- pim.fit(MM,Y, estim = "estimator.glm", penv=FEVenv)
#'
#' @export
pim.fit <- function(x,y,link = "logit",
                    estim = 'estimator.nleqslv',
                    start = rep(0,ncol(x)),
                    vcov.estim = 'sandwich.vcov',
                    weights = NULL,
                    penv,
                    bias.reduced = FALSE,
                    ...
                    )
{

  estimF <- match.fun(estim)
  vcov.estimF <- match.fun(vcov.estim)

  if(bias.reduced){
    theArgs <- names(formals(estimF))
    if("construct" %in% theArgs){
      res <- estimF(x, y, link = link, start = start,
                    construct = "CreateBRScoreFun")
    } else {
      msg <- paste(
        "The estimator function cannot set the correct constructor function.",
        "Please use one which has an argument 'construct' or put",
        "bias.reduced to FALSE and provide a custom function for estimation.",
        "See also ?estimators.")
      stop(msg)
    }
  } else {
    res <- estimF(x, y, link = link, start=start, ...)
  }


  fits <- x %*% res$coef
  dim(fits) <- NULL

  if(!is.list(penv)) penv <- poset(penv, as.list=TRUE)

  vc <- vcov.estimF(fits, x, y, weights, link, penv)

  return(list(coefficients = res$coef,vcov = vc, fitted=fits,
              estim = list(coef = estim, vcov = vcov.estim)))

}
