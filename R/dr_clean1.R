#' One-point drift correction
#'
#' @param dataFrame The working data frame
#' @param varName The name of the variable to correct
#' @param calVal A number
#' @param calStd A number
#' @param correctVar Name of value or variable generated from \code{\link{dr_correct}}
#' @return A list of values for the specified \code{varName} corrected for drift
#' @examples
#' \dontrun{
#' dr_clean1(df, SpCond, 1.05, 1, corrfactors)
#' dr_clean1(df, DO, 96.4, 99, df$corrections)
#'}
#'
#'@export
"dr_clean1" <- function(dataFrame, varName, calVal, calStd, correctVar) {
  corrVal <- base::eval(base::substitute(correctVar), dataFrame)
  raw <- base::eval(base::substitute(varName), dataFrame)
  correct <- raw + (corrVal*(calVal-calStd))
  return(correct)
}
