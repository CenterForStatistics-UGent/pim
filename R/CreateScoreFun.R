#' Create a score function for use in a pim.
#'
#' These functions create a suitable score function for the fitting
#' process of a probabilistic index model. For a normal fit you would
#' use the function \code{CreateScoreFun}. A bias-reduced pim is
#' fitted using the function \code{CreateBRScoreFun}.
#'
#' @param Z the model matrix of pseudo-observations
#' @param Y a vector with the response of the pseudo-observations
#' @param link a character vector indicating the link function
#' to be used. For \code{CreateScoreFun} this can be
#' \code{"probit"}, \code{"logit"} or \code{"identity"}. For the
#' function \code{CreateBRScoreFun} only the options \code{"probit"}
#' and \code{"logit"} are available.
#' @param W a vector with weights.
#'
#' @return a function used for estimating the coefficients by
#' the estimator functions.
#'
#' @section NOTE: This function is not exported.
#' @rdname CreateScoreFun

CreateScoreFun <-function(Z,Y,
                          link = "probit",
                          W = NULL)
{


  if (link == "probit") {
    U.func <- function(beta) {
      Zbeta <- c(Z %*% beta)
      pZbeta <- pnorm(Zbeta)
      summat<-Z * dnorm(Zbeta) * c(Y - pZbeta)/c(pZbeta * (1 - pZbeta))
      if(! is.null(W)) summat<-summat * W
      colSums(summat)
    }
  }
  else if (link == "logit"){
    U.func <- function(beta) {
      Zbeta <- c(Z %*% beta)
      summat<-Z * c(Y - plogis(Zbeta))
      if(! is.null(W)) summat<-summat * W
      colSums(summat)
    }
  }
  else if (link == "identity")
  {
    U.func <- function(beta) {
      Zbeta <- as.vector(c(Z %*% beta))
      summat<-Z * c(Y - Zbeta)
      if(! is.null(W)) summat<-summat * W
      colSums(summat)
    }
  }
  else
  {
    msg <- paste("Unsupported link function for CreateScoreFun:", link,
                 "\nOptions are: 'probit', 'logit' or 'identity'")
    stop(msg)
  }
  return(U.func)
}

#' @rdname CreateScoreFun
#' @aliases CreateBRScoreFun
CreateBRScoreFun <- function(Z, Y,
                             link = "probit",
                             W = NULL){

  if(link == "probit"){
    U.func <- function(beta) {
      Zbeta <- c(Z %*% beta)
      # var PI = 0.25 ( pnorm(0) * (1 - pnorm(0)))
      summat <- Z * dnorm(Zbeta) * c(Y - pnorm(Zbeta))/0.25
      if(! is.null(W)) summat<-summat * W
      colSums(summat)
    }
  } else if(link == "logit"){
    U.func <- function(beta) {
      Zbeta <- c(Z %*% beta)
      # var PI = 0.25 ( plogis(0) * (1 - plogis(0)))
      fv <- plogis(Zbeta)
      summat <- Z * fv * (1-fv) * c(Y - fv)/0.25
      if(! is.null(W)) summat<-summat * W
      colSums(summat)
    }
  } else {
    msg <- paste("Unsupported link function for CreateScoreFun:", link,
                 "\nOptions are: 'probit' or 'logit'")
    stop(msg)
  }
  return(U.func)
}
