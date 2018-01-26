#' Estimator functions for probabilistic index models
#'
#' This page documents different possibilities for solving the score function
#' of a probabilistic index model or \code{\link{pim}}. All functions mentioned on this page, are essentially wrappers around different solver functions.
#'
#' All functions share the same three arguments, being the design matrix \code{x},
#' the response vector \code{y} and the start values for the estimating function.
#' If you follow the same principles, you can write your own wrapper function
#' for any solver function of your choice.
#'
#' The solvers \code{estimator.nleqslv} and \code{estimator.BBsolve}
#' allow for specification of your own score function as well. For
#' this, you have the possibility to provide a constructor function
#' that takes three arguments
#' \describe{
#' \item{x}{The model matrix}
#' \item{y}{the vector with pseudo-observations}
#' \item{link}{a character vector specifying the link}
#' }
#' The function should return a function that can be used in
#' either \code{\link[nleqslv]{nleqslv}} or
#' \code{\link[BB]{BBsolve}}. If you don't specify this constructor function,
#' the package will use the constructor function \code{\link{CreateScoreFun}}
#' to provide the score function.
#'
#' @section WARNING: If you specify your own score function
#' without changing the estimators for the variance-covariance
#' matrix, this vcov matrix will be blatantly wrong!!!!!
#'
#'
#' @seealso \code{\link[nleqslv]{nleqslv}}, \code{\link[stats:glm]{glm.fit}},
#' \code{\link[BB]{BBsolve}} for more information on the fitting
#' algorithms.
#'
#' @param x a model matrix for the respective pim model. See also
#' \code{\link{model.matrix}}.
#'
#' @param y a vector with the response for the respective pim model.
#' @param start a vector as long as there are columns in \code{x}, containing
#' the starting values for the algorithm
#' @param link a character vector describing the link function. This
#' link function is used to adapt the calculation depending on the
#' link used in the fitting process.
#' @param method A vector of integers specifying which
#' Barzilai-Borwein steplengths should be used in a consecutive
#' manner. The methods will be used in the order specified.
#' More information on the help page of \code{\link{BBsolve}}.
#' @param construct a function that creates the score function
#' used by either \code{\link{nleqslv}} or
#' \code{\link{BBsolve}} for numerical optimization. See Details.
#' The estimator \code{estimator.glm} doesn't allow
#' for specification of your own score function.
#' @param ... extra arguments passed down to the actual solver function. See details.
#' @param control a list with extra controlling parameters for
#' \code{BBsolve}. See the help page of \code{\link{BBsolve}} for
#' more information.
#'
#' @return a list with following elements:
#'  \item{coef}{the estimated coefficients}
#'
#' @seealso \code{\link{vcov.estimators}}, \code{\link{pim.fit}} and
#' \code{\link{pim}} for more information on the fitting process
#' @examples
#' ## myconstruct creates a score function that uses the identity link.
#' ## The argument link is ignored, but it can be used to implement
#' ## score functions for different link functions in a single constructor function.
#'
#' myconstruct <- function(x,y,link){
#'  ## this function is returned
#'  function(beta){
#'    xb <- as.vector(x %*% beta)
#'    colSums(x * (y - xb))
#'   }
#' }
#'
#' data(ChickWeight)
#'
#' ## This model uses the function myconstruct to create the link function.
#' ## By using the argument link = "identity", you make sure that the varicance-
#' ## covariance matrix is estimated correctly.
#'
#' themodel <- pim(weight ~ Diet, data = ChickWeight,
#'                 construct = myconstruct,
#'                 link = "identity")
#'
#' # compare coefficients to
#' themodel2 <- pim(weight ~ Diet, data = ChickWeight,
#'                 link = "identity")
#' coef(themodel)
#' coef(themodel2)
#'
#' ## Keep in mind that you should always think about how the variance-covariance
#' ## matrix should be estimated. If you naively apply a score function without
#' ## giving thought to the vcov estimator, the output of summary() will
#' ## make no sense.
#'
#' @import nleqslv
#' @name estimators
#' @aliases estimator.nleqslv estimator.glm
#' @rdname estimators
#' @export
estimator.nleqslv <-
  function(x,y,start=rep(0,ncol(x)), link="logit",
           construct = NULL, ...){
    construct <- if(is.null(construct)) CreateScoreFun else
                    match.fun(construct)
    fn <- construct(x,y,link)

    res <- nleqslv(start,fn, ...)

    if(res$termcd != 1){
      warning(paste("nleqslv says:", res$message,"\n",
                    "See ?nleqslv for more info."),
              call. = FALSE)
    }
    list(coef = res$x)

  }

#' @rdname estimators
#' @export
estimator.glm <-
  function(x, y, start= rep(0,ncol(x)), link="logit", ...){

    if(is.character(link)){

      family <- .glmfamilies[[link]]

      if(is.null(family))
        stop("Link function",link,"not recognized by estimator.glm")
    }

    res <- withCallingHandlers(
      glm.fit(x,y, start = start, family=family, ...),
      warning = catch.noninteger.handler
    )
    list(coef = res$coef)
  }

#' @rdname estimators
#' @import BB
#' @export
estimator.BB <-
  function(x, y, start= rep(0,ncol(x)), link="logit",
           construct = NULL,
           method = c(1,2,3),
           control=list(NM = c(FALSE,TRUE)), ...){

    construct <- if(is.null(construct)) CreateScoreFun else
      match.fun(construct)
    fn <- construct(x,y,link)
    res <- BBsolve(start,fn, control = control, method = method,...)

    if(res$convergence != 0 ){
      warning(paste("BBsolve says:", res$message, "\n",
                    "See ?BBsolve for more info."),
              call. = FALSE)
    }

    list(coef = res$par)
  }
