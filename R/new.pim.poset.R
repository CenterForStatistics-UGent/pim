#' Create a pim.poset environment
#'
#' This function allows you to create a \code{\link{pim.poset}} environment
#' that can be added to a \code{\link{pim.environment}} object. You can
#' use this function to create a custom poset, but in general it's safer
#' to use the relevant arguments of the \code{\link{pim}} function.
#' That way more safety checks are carried out.
#'
#' @details A poset (or pseudo observation set) in the context of
#' probabilistic index models is a set of indices that determines
#' which observations are compared with one another. It is used to
#' construct the pseudo-observations on which the model is fitted.
#' You can think of a poset as a "pseudo-observation set".
#'
#' The most convenient way to use this function, is by specifying
#' a character value for the argument \code{compare}. The value "unique"
#' creates a poset in such a way that only unique combinations of two
#' observations are used in the model. The value "all" creates all
#' possible L-R combinations between the observations.
#'
#' If you want to define the poset yourself, you can pass either a matrix
#' or a list with 2 elements as value for the argument \code{compare}.
#' Columns of the matrix or elements of the list should either be named
#' "L" and "R", or be unnamed. When unnamed, the function takes the first
#' column/element as the left poset, and the second column/element as
#' the right poset. If the (col)names are anything else but "L" and "R",
#' these names are ignored and the first column is seen as "L".
#'
#' @section Note:
#' You can omit the argument \code{compare} if you supply a value for
#' \code{nobs}. You can also omit the argument \code{nobs} if you
#' provide a matrix or list as value for \code{compare}. The function
#' will try to deduct the number of observations from the highest
#' index value present in the matrix/list
#'
#' You can't omit both arguments together though, as the function
#' needs at least some information on the number of observations
#' the poset is designed for.
#'
#' @param compare A character value, matrix or list indicating how the
#' poset should be constructed. Defaults to the default value of
#' \code{\link{create.poset}}. See Details section for more information.
#'
#' @param nobs An integer value determining the number of observations
#' this poset is created for. If compare is not a character value,
#' the number of observations
#'
#' @param parent An optional environment that serves as the parent for the
#' \code{pim.poset} environment. By default this is the environment
#' from which the function is called. Note that for a correct functioning,
#' the parent environment should be set to the \code{\link{pim.environment}}
#' this object is part of. This is done automatically by the function
#' \code{\link{add.poset}}.
#'
#' @param comp.value a character value to be used as value for the
#' compare slot of the object. Defaults to 'custom' and
#' should be left at the default without
#' a very good reason to change it.
#'
#' @param ... arguments passed to other methods.
#'
#' @return an \code{\link{pim.poset}} object that can be used to
#' replace the poset in a pim environment.
#'
#' @seealso \code{\link{add.poset}} for more information on how to
#' adapt the poset of a \code{pim.environment} object.
#'
#' @section Warning:
#' Changing the value of \code{comp.value} by hand might result in
#' errors or a wrongly fitted model. The argument exists for internal
#' purposes and possible extensions later on, but should not be used.
#'
#' @examples
#' mypos <- new.pim.poset('unique',n=10) # creates empty environment
#' ls(mypos)
#' # Using the created poset functions L and R
#' # Note this is purely as illustration, this makes no sense
#' # in the context of a pim analysis.
#' mypos$L(1:10)
#' mypos$R(1:10)
#'
#' @include pim.poset-class.R
#'
#'
#' @export
setGeneric("new.pim.poset",
           function(compare,nobs,parent=parent.frame(),...){
             standardGeneric("new.pim.poset")
           })

#' @rdname new.pim.poset
setMethod("new.pim.poset",
          signature=c(compare="character",
                      nobs="numeric"),
          function(compare,nobs,parent,...){
            poset <- create.poset(compare,n=nobs)
            out <- new.pim.poset(poset,nobs,parent,...)
            out@compare <- compare
            out
          }
          )

#' @rdname new.pim.poset
setMethod("new.pim.poset",
          signature=c(compare="matrix",
                      nobs="numeric"),
          function(compare,nobs,parent,...){
            if(ncol(compare) != 2 )
              stop("matrix should have 2 columns")
            poset <- lapply(seq_len(ncol(compare)),
                            function(i) compare[,i])
            names(poset) <-
              if(is.null(namescomp <- colnames(compare)))
                c("L","R") else namescomp

            new.pim.poset(poset,nobs,parent,...)
          })

#' @rdname new.pim.poset
setMethod("new.pim.poset",
          signature=c(compare="list",
                      nobs="numeric"),
          function(compare,nobs,parent,comp.value='custom',...){

            if(length(compare) != 2L )
              stop("Compare should contain exact 2 columns/elements")
            names <- names(compare)
            if(is.null(names)){
              names(compare) <- c("L","R")
            } else if(!all(match(c("L","R"),names,0L) > 0)){
              names(compare) <- c("L","R")
            }
            out <- new("pim.poset")
            parent.env(out) <- environment()
              # This makes sure the object compare can be found
              # Will be set correctly at the end of the function
            out@compare <- comp.value
            out@nobs <- as.integer(nobs)

            eval(quote({
              L <- .make.posfun(compare[['L']])
              R <- .make.posfun(compare[['R']])
            }),envir=out)
            parent.env(out) <- parent
            out
          })

#' @rdname new.pim.poset
setMethod("new.pim.poset",
          signature=c(compare="matrix",
                      nobs="missing"),
          function(compare,parent,...){
            nobs <- max(compare)
            new.pim.poset(compare,nobs,parent,...)
          })

#' @rdname new.pim.poset
setMethod("new.pim.poset",
          signature=c(compare="list",
                      nobs="missing"),
          function(compare,parent,...){
            nobs <- max(unlist(compare,FALSE,FALSE))
            new.pim.poset(compare,nobs,parent,...)
          })

#' @rdname new.pim.poset
setMethod('new.pim.poset',
          signature=c(compare='missing',
                      nobs='numeric'),
          function(nobs, parent, ...){
            compare <- create.poset(n=nobs)
            new.pim.poset(compare, nobs, parent, comp.value='unique',...)
          })
