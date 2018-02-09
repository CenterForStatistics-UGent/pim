#' Extract information from pim.environment and pim.poset objects
#'
#' These functions serve to extract the information contained
#' in the objects of class \code{\link{pim.environment}} and
#' \code{\link{pim.poset}}.
#'
#' @param x an object of class \code{pim.environment} or \code{pim.poset}
#' @param object an object of class \code{pim} or \code{pim.summary}
#' @param ... arguments passed to and from other methods.
#'
#' @return \code{classes()}: A named vector with the classes of the data
#' contained in the \code{pim.environment}
#'
#' @note The functions shouldn't be used normally by the user of the
#' package. The \code{\link{pim.poset}} class only makes sense in
#' the context of a \code{pim.environment}. So for almost all use of
#' the package you should never access it directly.
#'
#' @seealso \code{\link{nobs}}, \code{\link{poset}}, \code{\link{is.complete}},
#' \code{\link{pim.environment-class}}, \code{\link{pim.poset-class}},
#' \code{\link{pim-class}}, \code{\link{pim.summary-class}}
#'
#' @examples
#' data(DysData)
#' DysPimEnv <- new.pim.env(DysData,poset=TRUE)
#' classes(DysPimEnv)
#' names(DysPimEnv)
#' compare(DysPimEnv)
#'
#' themodel <- pim(SPC_D2 ~ Chemo, data = DysData, model = 'difference')
#' model(themodel)
#' thesummary <- summary(themodel)
#' model(thesummary)
#'
#' @aliases names compare model link
#' @include pim.environment-class.R
#' @export
setGeneric('classes', function(x) standardGeneric('classes'))

#' @export
#' @rdname classes
setMethod('classes',
          signature='pim.environment',
          function(x){
            unlist(x@classes)
          })

## NOTE:
# names is a primitive function, hence a S4 generic
# is already available in the base package. Creating
# a generic in the package here only results in warnings.
# The generic won't be created, so when trying to export,
# there's nothing to export.

#' @return \code{names()}: For an object of class \code{pim.environment} the names
#' of the variables in the object. For an object of class \code{pim.poset},
#' the name of the poset functions inside the environment
#' @export
#' @rdname classes
setMethod('names',
          signature='pim.environment',
          function(x){
            x@data.names
          })

#' @export
#' @rdname classes
setMethod('names',
          signature='pim.poset',
          function(x){
            ls(x)
          })

#' @export
#' @rdname classes
#' @return \code{compare()}: A character value indicating how the comparison
#' is defined in a \code{pim.poset} object, or the poset-slot of
#' a \code{pim.environment} object respectively.
setGeneric('compare',function(x) standardGeneric('compare'))

#' @export
#' @rdname classes
setMethod('compare',
          signature=c('pim.environment'),
          function(x){
            x@poset@compare
          })

#' @export
#' @rdname classes
setMethod('compare',
          signature=c('pim.poset'),
          function(x){
            x@compare
          })

#' @return \code{model()}: a character value that displays
#' the type of model (difference, marginal, regular or customized)
#' @rdname classes
#' @export
setGeneric('model', function(object, ...){
  standardGeneric('model')})

#' @rdname classes
setMethod('model',
          'pim',
          function(object){object@model})

#' @rdname classes
setMethod('model',
          'pim.summary',
          function(object){object@model})

#' @return \code{link()}: a character value that displays
#' the type of link (difference, marginal, regular or customized)
#' @rdname classes
#' @export
setGeneric('link', function(object, ...){
  standardGeneric('link')})

#' @rdname classes
setMethod('link',
          'pim',
          function(object){object@link})

#' @rdname classes
setMethod('link',
          'pim.summary',
          function(object){object@link})
