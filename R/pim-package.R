#' Probabilistic Index Models
#'
#' This package offers a toolset to construct and evaluate probabilistic
#' index models. To learn about what these models are and how to use
#' the pim package to fit them, take a look at \code{vignette("pim")}.
#'
#' The package has been thoroughly reworked since the original publication
#' of the pim methodology. The code examples from the original publication
#' are not going to work with this version of the package. For reference,
#' the old version is still available at the original RForge repository
#' \url{https://r-forge.r-project.org/projects/pim/} under the heading
#' R packages.
#'
#' @name pim-package
#' @aliases pim-package
#' @docType package
#' @author Joris Meys \email{Joris.Meys@@UGent.be} Jan De Neve \email{Jan.DeNeve@@UGent.be} original package and engine code by Nick Sabbe.
#' @references \url{https://github.com/CenterForStatistics-UGent/pim}
#' @keywords package
#' @importFrom stats4 nobs
#' @importFrom stats dnorm glm.fit plogis pnorm printCoefmat qnorm
#' @importFrom utils head
#' @import methods
NULL

# All things needed for S4 definitions.
setOldClass("terms")
setOldClass("formula")


