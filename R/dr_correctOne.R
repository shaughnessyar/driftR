#' One-point drift correction
#'
#' @description A wrapper around \code{dplyr::mutate()} that creates a corrected value for each observation of the
#'     specified variable based on one data point.
#'
#' @details This function takes the raw data from the water-quality instrument, utilizes the values generated from
#'     \code{\link{dr_factor}} and returns data that accounts for drift over time. This is done via a one-point
#'     calibration standard, which it typical for specific conductivity, dissolved oxygen, and turbidity.
#'
#' @usage dr_correctOne(.data, sourceVar, cleanVar, calVal, calStd, factorVar)
#'
#' @param .data A tbl
#' @param sourceVar Name of variable to correct
#' @param cleanVar New variable name for corrected data
#' @param calVal A numeric value; the value that the instrument was actually reading for the parameter
#' @param calStd A numeric value; the value that the instrument should have been reading for that
#'     standard; i.e. the standard value
#' @param factorVar Name of variable generated using \code{\link{dr_factor}}
#'
#' @return An object of the same class as \code{.data} with the new corrected variable added
#' to the other data in \code{.data}.
#'
#' @seealso \code{\link{dr_factor}} for correction factor creation,
#'     \code{\link{dr_correctTwo}} for the two-point drift correction
#'
#' @importFrom rlang :=
#'
#' @examples
#' testData <- data.frame(
#'    Date = c("9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015"),
#'    Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
#'    Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
#'    SpCond = c(0.754, 0.750, 0.750, 0.749, 0.749, 0.749),
#'    corrFac = c(0.0000000, 0.2003995, 0.4007989, 0.6005326, 0.8002663, 1.0000000),
#'    stringsAsFactors = FALSE
#'  )
#'
#' dr_correctOne(testData, sourceVar = SpCond, cleanVar = SpCond_Corr,
#'     calVal = 1.05, calStd = 1, factorVar = corrFac)
#'
#' @export
dr_correctOne <- function(.data, sourceVar, cleanVar, calVal, calStd, factorVar) {

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(sourceVar)) {
    stop('A existing variable name with data to be corrected must be specified for sourceVar')
  }

  if (missing(cleanVar)) {
    stop('A new variable name must be specified for clearnVar')
  }

  if (missing(calVal)) {
    stop('A numeric value must be specified for calVal')
  }

  if (missing(calStd)) {
    stop('A numeric value must be specified for calStd')
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
  if (!(typeof(calVal) %in% c('integer', 'double'))) {
    stop(glue::glue('calVal value {calVal} not acceptable - value should numeric'))
  }

  if (!(typeof(calStd) %in% c('integer', 'double'))) {
    stop(glue::glue('calStd value {calStd} not acceptable - value should numeric'))
  }

  # create new variable
  dplyr::mutate(.data, !!cleanVar := (!!source) + ( (!!factor) * (calStd - calVal)))
}
