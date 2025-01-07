#' Probabilistic Index Models
#'
#' Fit a probabilistic index model. Note that this
#' version is NOT compatible with the previous version used in the original
#' publications on probabilistic index models. If you want to try out the
#' original code, please install the package pimold from R-Forge.
#' You can install the old package using:
#'
#' \code{install.packages('pimold', repos = 'http://R-Forge.R-project.org')}
#'
#' @name pim-package
#' @aliases pim-package
#' @author Joris Meys \email{Joris.Meys@@UGent.be} Jan De Neve \email{Jan.DeNeve@@UGent.be} original package and engine code by Nick Sabbe.
#' @references \url{http://r-forge.r-project.org/projects/pim/}
#' @keywords package
#' @importFrom stats4 nobs
#' @importFrom stats dnorm glm.fit plogis pnorm printCoefmat qnorm
#' @importFrom utils head
#' @import methods
"_PACKAGE"

# All things needed for S4 definitions.
setOldClass("terms")
setOldClass("formula")


