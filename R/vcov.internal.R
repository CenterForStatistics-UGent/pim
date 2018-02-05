#' Internal functions for vcov estimation
#'
#' These functions serve as preparation functions to calculute the variance-
#' covariance matrix of a pim using any of the \code{\link{vcov.estimators}}
#' provided in this package. The result of these preparation functions
#' is used by the \code{\link{sandwich.estimator}} and
#' \code{\link{score.estimator}} functions respectively.
#'
#' @param Zbeta fitted values
#' @param Z design matrix
#' @param Y pseudo responses
#' @param link character vector with link function
#' @param W vector with weights
#'
#' @note These functions should NOT be called by the user.
#'
#' @rdname vcov.internal
#' @name vcov.internal
#' @aliases U.sandwich U.score
U.sandwich <- function(Zbeta, Z, Y, link, W=NULL)
{
  #   if(! is.null(W))
  # 	{
  # 		warning("Currently, weights are not supported in Uforposandwich.default They will be ignored.")
  # 	}
  #Zbeta <- c(Zbeta)
  #Y <- c(Y)
  if(link == "probit")
  {
    fv <- pnorm(Zbeta)
    var.PI <- fv*(1-fv)
    var.PI[var.PI == 0] <- 0.01 #correction, not mentioned in the article, low impact
    m.d <- dnorm(Zbeta)
    m.dd <- -m.d*Zbeta
    res <- Y-fv
    U <- Z*m.d*res/var.PI
    if(!is.null(W))
    {
      U<-W * U
      U.diff <- t(Z)%*% diag(W) %*%(Z*c((var.PI*(m.dd*res - m.d^2) - res*m.d^2*(1-2*fv))/var.PI^2))
    }
    else
    {
      U.diff <- t(Z)%*%(Z*c((var.PI*(m.dd*res - m.d^2) - res*m.d^2*(1-2*fv))/var.PI^2))
    }
  }
  else if(link == "logit")
  {
    fv <- plogis(Zbeta)
    var.PI <- fv*(1-fv)
    var.PI <- ifelse(var.PI==0,0.01,var.PI) #correction, not mentioned in the article, low impact
    U <- Z*c(Y-fv)
    if(!is.null(W))
    {
      U<-W * U
      U.diff <- -t(Z)%*% diag(W) %*%(Z*c(var.PI))
    }
    else
    {
      U.diff <- -t(Z)%*%(Z*c(var.PI))
    }
  }
  else if(link == "identity")
  {
    #note: for identity, we leave out the variance. This is OK
    fv<-Zbeta
    U <- Z*c(Y-fv)
    if(!is.null(W))
    {
      U<-W * U
      U.diff <- -t(Z)%*% diag(W) %*%(Z)
    }
    else
    {
      U.diff <- -t(Z)%*%(Z)
    }
  }
  else
  {
    stop(paste("Unsupported link function for U.sandwich:"), link)
  }

  rv<-list(U=U, U.diff=U.diff, fv=fv, shared.factor=1, switched.factor=1, self.factor=1)
  return(rv)
}

#' @rdname vcov.internal
U.score <- function(Zbeta, Z, Y, link, W=NULL)
{
  if(link != "identity")
  {
    stop("You are using Uforposandwich.fakeH0 and thus probably varianceestimator.H0. This is only correct if the link function is the identity.")
  }
  if(! is.null(W))
  {
    warning("Currently, weights are not supported in Uforposandwich.fakeH0. They will be ignored.")
  }
  Zbeta <- c(Zbeta)

  tZ<-t(Z)
  fakeU<-t(solve(tZ %*% Z) %*% tZ)

  rv<-list(U=fakeU, U.diff=NULL, fv=Zbeta, shared.factor=1/12, switched.factor=-1/12, self.factor=1/4)
  return(rv)
}
