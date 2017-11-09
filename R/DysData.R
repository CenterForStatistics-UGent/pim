#' Dysphagia induced by cancer treatment
#'
#' This data stems from research trying to predict dysphagia in cancer
#' patients following treatment with intensity-modulated radiotherapy.
#' Patients with head and neck cancer were evaluated
#' for the intensity of dysphagia, and for single nucleotide
#' polymorphisms (SNPs) in a series of different genes. The dataset in
#' this package contains only the data for SNPs in the gene XRCC1. This data is published before by \href{https://doi.org/10.1016/j.radonc.2013.03.021}{De Ruyck et al (2013)}.
#'
#'These are the columns and their meanings
#' \itemize{
#' 	\item{\code{out}} intensity of the dysphagia on a scale of 1 to 4,
#' 	with 4 being severe. Scoring is done using CTCAE.
#' 	\item{\code{Chemo}} Whether the patient underwent chemotherapy ("ja" is yes, "nee" is no)
#' 	\item{\code{SNP_XRCC1__77}} Genotype of SNP for XRCC1. This factor has three levels: "TT", "TC" and "CC"
#' 	\item{\code{SPC_D2}} Dose of radiation that reached 2% of the SPC muscle
#' 	\item{\code{SNP_XRCC1__77TC}} 1 if \code{SNP_XRCC1__77} is "TC", 0 otherwise
#' }
#'
#' @format A data frame with 188 observations and 5 variables
#' @source De Ruyck et al, 2013. A predictive model for dysphagia following
#' IMRT for head and neck cancer: Introduction of the
#' EMLasso technique. \emph{Radiotherapy and Oncology 107, pp 295-299}.
#' DOI: \url{https://doi.org/10.1016/j.radonc.2013.03.021}
#' @keywords data Dysphagia
"DysData"
