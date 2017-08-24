#' The pim.poset class
#' 
#' The pim.poset class is an S4 class that inherits from \code{\link{environment}}
#' and contains the poset functions for a \code{\link{pim}}. It's a class
#' used internally and should not be adapted by the user. The correct
#' interpretation of the formula is dependent on this object. The object
#' mainly functions as a slot in object of class 
#' \code{\link{pim.environment}}.
#' 
#' @slot compare a character value with the type of poset. This can
#' take the values "unique", "all" and "custom".
#' @slot nobs an integer value describing the number of observations 
#' for which this poset is meant to be used. 
#' 
#' @section Note:
#' The pim.poset class doesn't really make sense to be used on itself.
#' It is part of the \code{\link{pim.environment}} class and shouldn't
#' be used outside this context.
#' @aliases pim.poset
setClass("pim.poset",
         slots = c(compare="character",
                 nobs="integer"),
         contains = "environment",
         validity= function(object){
           out <- TRUE
           if(length(object@compare) !=1 ){
             out <- "Compare should be a single value."
           } else if(length(nobs) !=1){
             out <- "nobs should be a single value."
           } else if(!object@compare %in% c("unique",
                                            "all",
                                            "custom")){
             out <- "Compare should be any of unique, all or custom."
           }
           out
         })
