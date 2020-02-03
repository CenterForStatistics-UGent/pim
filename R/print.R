#' Print methods for the different object types
#'
#' Printing \code{pim}, \code{pim.environment}, \code{pim.formula} and
#' \code{pim.poset} objects.
#'
#' @param x the object
#' @param digits an integer that defines the number of digits printed
#' @param n number of observations shown by \code{print}
#' @param show.vcov a logical value indicating whether the variance-
#' covariance matrix should be shown or not. Defaults to \code{FALSE}
#' @param ... arguments passed to other methods. Currently ignored
#'
#' @return invisible NULL
#' @include pim-class.R pim.environment-class.R pim.formula-class.R pim.poset-class.R
#'
#' @examples
#' data(FEVData)
#' Model <- pim(FEV~ Smoke*Sex , data=FEVData)
#' print(Model)
#' print(penv(Model))
#' # You get the drift
#'
#' @importFrom utils head
#' @export
setGeneric('print')

#------------------------
# print method for pim
#------------------------

print.pim <- function(x, digits = max(3L, getOption("digits") - 3L),
                      show.vcov = FALSE, ...){
  orig <- paste(deparse(x@formula@orig))
  coefs <- coef(x)
  vc <- vcov(x)
  cat('\nProbabilistic Index Model:\nFormula: ',orig,
      "\nType: ", x@model,
      "\nLink: ", x@link,
      "\n\n")

  if (length(coefs)) {
    cat("Coefficients:\n")
    print.default(format(coefs, digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  if(show.vcov){
    cat("VCOV matrix:\n")
    print.default(format(vc, digits = digits), print.gap = 2L,
                  quote=FALSE)
  }
  invisible(NULL)
}

# show method for pim
setMethod('show',
          'pim',
          function(object){print(object, show.vcov = FALSE)})

#' @rdname print
# print method for pim
setMethod('print',
          'pim',
          print.pim)

#------------------------
# print method for pim.environment
#------------------------

print.pim.environment <- function(x, digits = max(3L, getOption("digits") - 3L), n = 6L, ...){
  no <- nobs(x)
  nc <- length(classes(x))

  complete <- is.complete(x)
  ww <- if(complete) "with" else "without"
  cat('\nPIM environment with',no,
      'observations of',nc,'variables.\n\n')

  print(head(as.data.frame(x), n = n))

  if(n<no) cat("(Only first",n,"observations shown.)\n")

  cat("\n",ww,"poset\n")
  if(complete)
    print(t(poset(x))[,seq_len(n)])

  if(n<no) cat("(Only first",n,"columns shown.)\n")

  invisible(NULL)

}

# show method for pim.environment
setMethod('show',
          'pim.environment',
          function(object){print(object)})

#' @rdname print
# print method for pim.environment
setMethod('print',
          'pim.environment',
          print.pim.environment)

#------------------------
# print method for pim.poset
#------------------------

print.pim.poset <- function(x, digits = max(3L, getOption("digits") - 3L), n = 6L, ...){
  no <- nobs(x)

  cat('\nPIM poset for',no,
      'observations.\n','comparison:',compare(x),'\n\n')

  print(t(poset(x))[,seq_len(n)])

  if(n<no) cat("(Only first",n,"columns shown.)\n")

  invisible(NULL)

}

# show method for pim.poset
setMethod('show',
          'pim.poset',
          function(object){print(object)})

#' @rdname print
# print method for pim.poset
setMethod('print',
          'pim.poset',
          print.pim.poset)

#------------------------
# print method for pim.formula
#------------------------

print.pim.formula <- function(x, digits = max(3L, getOption("digits") - 3L), ...){

  intercept <- has.intercept(x)
  ww <- if(intercept) 'with' else 'without'
  cat('\nPIM formula',ww,'intercept:\n')
  print(x@orig)
  cat('\nLeft hand side:',deparse(lhs(x)))


}

# show method for pim.formula
setMethod('show',
          'pim.formula',
          function(object){print(object)})

#' @rdname print
# print method for pim.formula
setMethod('print',
          'pim.formula',
          print.pim.formula)

#------------------------
# print method for pim.summary
#------------------------


print.pim.summary <- function(x, digits = max(3L, getOption("digits") - 3L),...){
  orig <- paste(deparse(formula(x@formula)))
  cat("pim.summary of following model : \n", orig)


  cat("\nType: ", model(x),
      "\nLink: ", link(x),"\n\n")

  Tab <- cbind(
    Estimate = coef(x),
    "Std. Error" = x@se,
    "z value" = x@zval,
    "Pr(>|z|)" = x@pr
  )
  cat("\n")
  printCoefmat(Tab, digits = digits)

  if(length(x@h0) == 1){
    cat("\nNull hypothesis: b =",x@h0,"\n")
  } else {
    thenames <- rownames(Tab)
    cat("\nNull hypotheses:\n")
    for(i in seq_along(x@h0)){
      cat(" b_",thenames[i]," = ",x@h0[i],"\n", sep="")
    }
    cat("\n")
  }

}

setMethod("show",
          "pim.summary",
          function(object){
            print.pim.summary(object)
          })
