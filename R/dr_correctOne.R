#' One-point drift correction
#' @description This command takes the raw data from the water-quality instrument, utilizes the values generated from \code{\link{dr_correct}} and returns data that accounts for drift over time. This is done via a one point calibration standard, which it typical for specific conductivity, dissolved oxygen, and turbidity.
#' @param dataFrame The working data frame
#' @param varName The name of the variable to correct
#' @param calVal The number that the instrument was actually reading for the parameter
#' @param calStd The number that the instrument should have been reading for that standard; i.e. the standard value
#' @param factorVar Name of value or variable generated from \code{\link{dr_correct}}
#' @return A list of values for the specified \code{varName} corrected for drift
#' @examples
#' \dontrun{
#' dr_correctOne(df, SpCond, 1.05, 1, corrfactors)
#' dr_correctOne(df, DO, 96.4, 99, df$corrections)
#'}
#'
#'@export
dr_correctOne <- function(.data, sourceVar, cleanVar, calVal, calStd, factorVar) {

  # quote input variables
  cleanVar <- quo_name(enquo(cleanVar))
  sourceVar <- enquo(sourceVar)
  factorVar <- enquo(factorVar)

  # create new variable
  mutate(.data, !!cleanVar := (!!sourceVar) + ( (!!factorVar) * (calVal - calStd)))
}
