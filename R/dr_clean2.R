#' Two-point drift correction
#' @description This command takes the raw data from the water-quality instrument, utilizes the values generated from \code{\link{dr_correct}} and returns data that accounts for drift over time. This is done via a two point calibration standard, which it typical for pH and chloride.
#' @param dataFrame The working data frame
#' @param varName The name of the variable to correct
#' @param calValLow The number that the instrument was actually reading for the low standard
#' @param calStdLow The number that the instrument should have been reading for that standard; i.e. the low standard value
#' @param calValHigh The number that the instrument was actually reading for the high standard
#' @param calStdHigh The number that the instrument should have been reading for that standard; i.e. the high standard value
#' @param correctVar Name of value or variable generated from \code{\link{dr_correct}}
#' @return A list of values for the specified \code{varName} corrected for drift
#' @examples
#' \dontrun{
#' dr_clean2(df, pH, 7.05, 7, 10.25, 10, corrfactors)
#' dr_clean2(df, Chloride, 7.95, 10, 847, 1000, df$corrections)
#'}
#'
#' @export
dr_clean2 <- function(.data, sourceVar, cleanVar, calValLow, calStdLow, calValHigh, calStdHigh, correctVar) {

  # quote input variables
  cleanVar <- quo_name(enquo(cleanVar))
  sourceVar <- enquo(sourceVar)
  correctVar <- enquo(correctVar)

  # calculate parameters and create new variable
  .data %>%
    mutate(low := calStdLow + ((!!correctVar) * (calStdLow - calValLow))) %>%
    mutate(high := calStdHigh - ((!!correctVar) * (calStdHigh - calValHigh))) %>%
    mutate(!!cleanVar := ((((!!sourceVar) - low) / (high - low) ) * (calStdHigh - calStdLow) ) + calStdLow) %>%
    select(-low, -high)
}
