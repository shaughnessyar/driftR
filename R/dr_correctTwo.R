#' Two-point drift correction
#'
#' @description A wrapper around `dplyr::mutate()` that creates a corrected value for each observation of the
#'     specified variable based on two data points.
#'
#' @details This command takes the raw data from the water-quality instrument, utilizes the values
#'    generated from \code{\link{dr_factor}} and returns data that accounts for drift over time.
#'    This is done via a two-point calibration standard, which it typical for pH and chloride.
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
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' dr_correctTwo(df, pH, 7.05, 7, 10.25, 10, corrfactors)
#' dr_correctTwo(df, Chloride, 7.95, 10, 847, 1000, df$corrections)
#'}
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
