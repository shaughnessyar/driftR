#' Two-point drift correction
#'
#' @param dataFrame The working data frame
#' @param varName The name of the variable to correct
#' @param calValLow A number
#' @param calStdLow A number
#' @param calValHigh A number
#' @param calStdHigh A number
#' @param correctVar Name of value or variable generated from \code{\link{dr_correct()}}
#' @return A list of values for the specified \code{varName} corrected for drift
#' @examples
#' dr_clean2(df, pH, 7.05, 7, 10.25, 10, corrfactors)
#' dr_clean2(df, Chloride, 7.95, 10, 847, 1000, df$corrections)
"dr_clean2" <- function(dataFrame,varName,calValLow,calStdLow,calValHigh,calStdHigh,correctVar) {
  corrVal <- base::eval(base::substitute(correctVar), dataFrame)
  raw <- base::eval(base::substitute(varName), dataFrame)
  low <- calStdLow+(corrVal*(calStdLow-calValLow))
  high <- calStdHigh-(corrVal*(calStdHigh-calValHigh))
  correct <- (((raw-low)/(high-low))*(calStdHigh-calStdLow))+calStdLow
  return(correct)
}
