#' Create a score function for use in a pim.
#'
#' This function creates a suitable score function for the fitting
#' process of a probabilistic index model.
#'
#' @param Z the model matrix of pseudo-observations
#' @param Y a vector with the response of the pseudo-observations
#' @param link a character vector indicating the link function
#' to be used.
#' @param W a vector with weights.
#'
#' @return a function used for estimating the coefficients by
#' the estimator functions.
#'
#' @section NOTE: This function is not exported.

CreateScoreFun <-function(Z,Y,
                          link = c("probit","logit","identity"),
                          W=NULL)
{
  link <- match.arg(link)

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
    stop(paste("Unsupported link function for CreateScoreFun:"), link)
  }
  return(U.func)
}
