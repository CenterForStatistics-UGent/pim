#' This is the Dysphagia data
#'
#' @name DysData
#' @docType data
#' @details These are the columns and their meanings
#' \itemize{
#' 	\item{\code{out}} Outcome: a factor with values 1 to 4, indicating the outcome
#' 	\item{\code{Chemo}} Whether the patient underwent chemotherapy ("ja" is yes, "nee" is no)
#' 	\item{\code{SNP_XRCC1__77}} Genotype of this SNP. A factor with three levels: "TT", "TC" and "CC"
#' 	\item{\code{SPC_D2}} Dose of radiation that reached 2% of the SPC muscle
#' 	\item{\code{SNP_XRCC1__77TC}} 1 if \code{SNP_XRCC1__77} is "TC", 0 otherwise
#' }
#' @keywords data Dysphagia
NULL
