#' The pim.environment class
#' 
#' This S4 class inherits from the S3 class \code{\link{environment}}.
#' The environment serves as a container to hold the data, poset and the
#' poset related functions of a probabilistic index model generated
#' by the function \code{\link{pim}}. The objects of this class
#' behave much like an environment, but contain some extra slots
#' with information on the objects inside the environment.
#' 
#' @section Note: 
#' This class is not exported, so it can't be extended as for now. 
#' Although it is possible to use the function \code{\link{new}} for
#' creation of new instances, users are strongly advised to use
#' the function \code{\link{new.pim.env}} in case they need to manually
#' create a new instance of the class \code{pim.environment}.
#' 
#' @slot poset an environment of class \code{\link{pim.poset}} containing the  poset-related functions
#' (normally these are \code{\link{L}} and \code{\link{R}}). This 
#' environment has the object itself as parent.
#' @slot data.names a character vector containing the names of the
#' vectors that represent the data
#' @slot nobs integer value indicating the number of observations in the 
#' environment
#' @slot classes a _named_ list containing the classes of the
#' objects inside the environment. Note that the value should be the one
#' given by \code{\link{class}}.
#' @slot is.complete a logical value indicating whether or not the poset
#' was added before. 
#' 
#' @aliases pim.environment
#' @include pim.poset-class.R
#' 
setClass("pim.environment",
         slots=c(
           poset="pim.poset",
           data.names="character",
           nobs= "integer",
           classes = "list",
           is.complete = "logical"
           ),
         contains="environment",
         prototype= list(is.complete=FALSE),
         validity=function(object){
           allnames <- ls(object)
           given <- object@data.names
           out <- TRUE
           dups <- duplicated(given)
           ndata <- length(get(allnames[1],
                                 envir=object@.xData,inherits=FALSE))  
           
#            if(!all(sapply(object@.xData, is.variable))){
#              out <- "Not all elements in the environment are useable as variable"
# These lines check whether all objects are valid variables. Problem is
# that they expect all objects to be vectors, and this wouldn't allow
# extensions to eg survival fits with Surv() objects.

           if(!.equal.nobs(as.environment(object))){
             out <- "Not all variables in the environment have the same length."
           } else if(object@nobs != ndata){
             out <- "nobs doesn't match number of observations"
           } else if(any(dups)){
             out <- gettextf(
               "The names '%s' occur more than once",
               .lpaste(given[dups])
               ) 
           } else if(!.same.elements(allnames,given)){
             out <- "Names in environment and slots don't match."
           } else if(!.same.classes(object@.xData,
                                    object@classes)){
             out <- "Classes slot is incorrect."
           } else if(!valid.classes(object@classes)){
             out <- "Some classes are incompatible with pim.environment"
           } else if(object@is.complete){
             if(identical(ls(object@.xData), character(0))){
               out <- "The object is not complete: no data."
             } else if(identical(ls(object@poset), character(0))){
               out <- "The object is not complete: no poset"
             }
           }
           out
         })


