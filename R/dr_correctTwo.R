#' Two-point drift correction
#'
#' @description A wrapper around \code{dplyr::mutate()} that creates a corrected value for each observation of the
#'     specified variable based on two data points.
#'
#' @details This command takes the raw data from the water-quality instrument, utilizes the values
#'    generated from \code{\link{dr_factor}} and returns data that accounts for drift over time.
#'    This is done via a two-point calibration standard, which it typical for pH and chloride.
#'
#' @usage dr_correctTwo(.data, sourceVar, cleanVar, calValLow, calStdLow,
#'                       calValHigh, calStdHigh, factorVar)
#'
#' @param .data A tbl
#' @param sourceVar Name of variable to correct
#' @param cleanVar New variable name for corrected data
#' @param calValLow A numeric value; the number that the instrument was actually reading for
#'     the low standard
#' @param calStdLow A numeric value; the number that the instrument should have been reading
#'     for that standard; i.e. the low standard value
#' @param calValHigh A numeric value; the number that the instrument was actually reading
#'     for the high standard
#' @param calStdHigh A numeric value; the number that the instrument should have been reading
#'     for that standard; i.e. the high standard value
#' @param factorVar Name of variable generated using \code{\link{dr_factor}}
#'
#' @return An object of the same class as \code{.data} with the new corrected variable added
#' to the other data in \code{.data}.
#'
#' @seealso \code{\link{dr_factor}} for correction factor creation,
#'     \code{\link{dr_correctOne}} for the two-point drift correction
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @examples
#' testData <- data.frame(
#'    Date = c("9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015"),
#'    Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
#'    Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
#'    pH = c(7.18, 7.14, 7.14, 7.13, 7.13, 7.13),
#'    corrFac = c(0.0000000, 0.2003995, 0.4007989, 0.6005326, 0.8002663, 1.0000000),
#'    stringsAsFactors = FALSE
#'  )
#'
#'  dr_correctTwo(testData, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01, calStdLow = 7,
#'      calValHigh = 11.8, calStdHigh =  10, factorVar = corrFac)
#'
#' @export
dr_correctTwo <- function(.data, sourceVar, cleanVar, calValLow, calStdLow, calValHigh, calStdHigh, factorVar) {

  # save parameters to list
  paramList <- as.list(match.call())

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  high = low = NULL

  # check for missing parameters
  if (missing(sourceVar)) {
    stop('A existing variable name with data to be corrected must be specified for sourceVar')
  }

  if (missing(cleanVar)) {
    stop('A new variable name must be specified for clearnVar')
  }

  if (missing(calValLow)) {
    stop('A numeric value must be specified for calValLow')
  }

  if (missing(calStdLow)) {
    stop('A numeric value must be specified for calStdLow')
  }

  if (missing(calValHigh)) {
    stop('A numeric value must be specified for calValHigh')
  }

  if (missing(calStdHigh)) {
    stop('A numeric value must be specified for calStdHigh')
  }

  if (missing(factorVar)) {
    stop('An existing variable name with the correction factor data must be specified for factorVar')
  }

  # quote input variables
  cleanVar <- rlang::quo_name(rlang::enquo(cleanVar))

  if (!is.character(paramList$sourceVar)) {
    source <- rlang::enquo(sourceVar)
  } else if (is.character(paramList$sourceVar)) {
    source <- rlang::quo(!! rlang::sym(sourceVar))
  }

  sourceVarQ <- rlang::quo_name(rlang::enquo(source))

  if (!is.character(paramList$factorVar)) {
    factor <- rlang::enquo(factorVar)
  } else if (is.character(paramList$factorVar)) {
    factor <- rlang::quo(!! rlang::sym(factorVar))
  }

  factorVarQ <- rlang::quo_name(rlang::enquo(factor))

  # check variables
  if(!!sourceVarQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {var}, given for sourceVar, cannot be found in the given data frame',
                    var = sourceVarQ))
  }

  if(!!factorVarQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {var}, given for factorVar, cannot be found in the given data frame',
                    var = factorVarQ))
  }

  if(!!cleanVar %in% colnames(.data)) {
    stop(glue::glue('A variable named {var}, given for cleanVar, already exists in the given data frame',
                    var = cleanVar))
  }

  # check input parameters
  if (!(typeof(calValLow) %in% c('integer', 'double'))) {
    stop(glue::glue('calValLow value {calValLow} not acceptable - value should numeric'))
  }

  if (!(typeof(calStdLow) %in% c('integer', 'double'))) {
    stop(glue::glue('calStdLow value {calStdLow} not acceptable - value should numeric'))
  }

  if (!(typeof(calValHigh) %in% c('integer', 'double'))) {
    stop(glue::glue('calValHigh value {calValHigh} not acceptable - value should numeric'))
  }

  if (!(typeof(calStdHigh) %in% c('integer', 'double'))) {
    stop(glue::glue('calStdHigh value {calStdHigh} not acceptable - value should numeric'))
  }

  # calculate parameters and create new variable
  .data %>%
    dplyr::mutate(low := calStdLow + ((!!factor) * (calStdLow - calValLow))) %>%
    dplyr::mutate(high := calStdHigh - ((!!factor) * (calStdHigh - calValHigh))) %>%
    dplyr::mutate(!!cleanVar := ((((!!source) - low) / (high - low) ) * (calStdHigh - calStdLow) ) + calStdLow) %>%
    dplyr::select(-low, -high)
}
