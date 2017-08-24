#' The data contained in the pim package
#' 
#' The pim package contains different datasets for use in examples and tests. Currently, you find the datasets \code{\link{DysData}}, \code{\link{EngelData}} and \code{\link{FEVData}}. More information can be found on the respective help pages.
#' 
#' @name pimdata
#' @docType data
#' @details The data contained in the package has following structures
#' \itemize{
#'   \item{\code{\link{EngelData}}}: A single numeric predictor variable and a response
#'   \item{\code{\link{FEVData}}}: A data frame with a numeric response variable and 4 additional numeric predictor values.
#' 	\item{\code{\link{DysData}}}: A dataframe with 3 factors and a numeric variable as predictors. The outcome is a factor with 4 levels.
#' 	\item{\code{SNP_XRCC1__77}} Genotype of this SNP. A factor with three levels: "TT", "TC" and "CC"
#' 	\item{\code{SPC_D2}} Dose of radiation that reached 2% of the SPC muscle
#' 	\item{\code{SNP_XRCC1__77TC}} 1 if \code{SNP_XRCC1__77} is "TC", 0 otherwise
#' }
#' @keywords data pim EngelData DysData FEVData
NULL