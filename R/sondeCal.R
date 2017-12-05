#' Calibration values for water quality monitoring data correction
#'
#' A data set containing correction values used in practice drift corrections
#'
#' @docType data
#'
#' @usage data(sondeCal)
#'
#' @format a dataframe with 7 rows and 3 variables
#' \describe{
#'    \item{Parameter}{The parameter that you will be correcting for}
#'    \item{'Cal Standard'}{The value that the instrument should be reading for the given parameter}
#'    \item{'Cal Value'}{The value that the instrument is actually reading for the given parameter}
#' }
#'
#' @source Saint Louis University Geochemistry Lab
#'
#' @examples
#' str(sondeCal)
#' head(sondeCal)
#'
"sondeCal"
