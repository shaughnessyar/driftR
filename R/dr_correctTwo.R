#' Two-point drift correction
#' @description This command takes the raw data from the water-quality instrument, utilizes the values generated from \code{\link{dr_factor}} and returns data that accounts for drift over time. This is done via a two point calibration standard, which it typical for pH and chloride.
#' @param .data The working data frame
#' @param sourceVar The name of the variable to correct
#' @param cleanVar The new variable
#' @param calValLow The number that the instrument was actually reading for the low standard
#' @param calStdLow The number that the instrument should have been reading for that standard; i.e. the low standard value
#' @param calValHigh The number that the instrument was actually reading for the high standard
#' @param calStdHigh The number that the instrument should have been reading for that standard; i.e. the high standard value
#' @param factorVar Name of value or variable generated from \code{\link{dr_factor}}
#' @return A list of values for the specified \code{varName} corrected for drift
#' @examples
#' \dontrun{
#' dr_correctTwo(df, pH, 7.05, 7, 10.25, 10, corrfactors)
#' dr_correctTwo(df, Chloride, 7.95, 10, 847, 1000, df$corrections)
#'}
#'
#' @export
dr_correctTwo <- function(.data, sourceVar, cleanVar, calValLow, calStdLow, calValHigh, calStdHigh, factorVar) {

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  high = low = NULL

  # quote input variables
  cleanVar <- quo_name(enquo(cleanVar))
  sourceVar <- enquo(sourceVar)
  factorVar <- enquo(factorVar)

  # calculate parameters and create new variable
  .data %>%
    mutate(low := calStdLow + ((!!factorVar) * (calStdLow - calValLow))) %>%
    mutate(high := calStdHigh - ((!!factorVar) * (calStdHigh - calValHigh))) %>%
    mutate(!!cleanVar := ((((!!sourceVar) - low) / (high - low) ) * (calStdHigh - calStdLow) ) + calStdLow) %>%
    select(-low, -high)
}
