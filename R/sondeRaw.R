#' Uncorrected water quality monitoring data
#'
#' A data set containing un-corrected measurements from a YSI Sonde 6600
#'
#' @docType data
#'
#' @usage data(sondeRaw)
#'
#' @format a dataframe with 1528 rows and 11 variables
#' \describe{
#'    \item{Date}{Date of measurement}
#'    \item{Time}{Time of measurement}
#'    \item{Temp}{Temperature in degrees C}
#'    \item{SpCond}{Specific conductivity in mS/cm}
#'    \item{pH}{pH}
#'    \item{pHmV}{Potential reading from pH sensor in mV}
#'    \item{Chloride}{Chloride in mg/L}
#'    \item{AmmoniumN}{Ammonium-Nitrogen in mg/L}
#'    \item{NitrateN}{Nitrate-Nitrogen in mg/L}
#'    \item{`Turbidity+`}{Turbidity in NTU}
#'    \item{DO}{Dissolved Oxygen in \% sat}
#' }
#'
#' @source Saint Louis University Geochemistry Lab
#'
#' @examples
#' str(sondeRaw)
#' head(sondeRaw)
#'
"sondeRaw"
