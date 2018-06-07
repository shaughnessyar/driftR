#' Creating correction factors
#'
#' @description A wrapper around \code{dplyr::mutate()} that creates a correction factor for each observation.
#'
#' @details Correction factors are calculated based on the time of the observation and the total amount of time
#'     that the instrument had been deployed. They are used in the equations for both the one-point and two-point
#'     drift corrections.
#'
#' @usage dr_factor(.data, corrFactor, dateVar, timeVar, tz = NULL,
#'     format = c("MDY", "YMD"), keepDateTime = TRUE)
#'
#' @param .data A tbl
#' @param corrFactor New variable name for correction factor data
#' @param dateVar Name of variable containing date data
#' @param timeVar Name of variable containing time data
#' @param tz String name of timezone, defaults to system's timezone
#' @param format Either "MDY" or "YMD" for \code{dateVar} -
#'     \strong{\emph{deprecated as of \code{driftR} v1.1}}
#' @param keepDateTime A logical statement to keep an intermediate dateTime variable
#'
#' @return An object of the same class as \code{.data} with the new correction factor variable added
#' to the other data in \code{.data} as well as a dateTime variable if keepDateTime = TRUE.
#'
#' @seealso \code{\link{dr_correctOne}} for correction factor creation,
#'     \code{\link{dr_correctTwo}} for the two-point drift correction
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @examples
#' testData <- data.frame(
#'    Date = c("9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015"),
#'    Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
#'    Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
#'    SpCond = c(0.754, 0.750, 0.750, 0.749, 0.749, 0.749),
#'    stringsAsFactors = FALSE
#'  )
#'
#' dr_factor(testData, corrFactor = corrFac, dateVar = Date, timeVar = Time, keepDateTime = TRUE)
#'
#' @export
dr_factor <- function(.data, corrFactor, dateVar, timeVar, tz = NULL, format = c("MDY", "YMD"), keepDateTime = TRUE) {

  # save parameters to list
  paramList <- as.list(match.call())

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  dateTime = totTime = dateTimePOSIX = NULL

  # check for deprecated paramater
  if (!missing(format)) {
    warning("Argument format is deprecated; dates and times are now automatically parsed as of v1.1.", call. = FALSE)
  }

  # check for missing parameters
  if (missing(corrFactor)) {
    stop('A new variable name must be specified for corrFactor')
  }

  if (missing(dateVar)) {
    stop('An existing variable with date data must be specified for dateVar')
  }

  if (missing(timeVar)) {
    stop('An existing variable with time data must be specified for timeVar')
  }

  # quote input variables
  corrFactor <- rlang::quo_name(rlang::enquo(corrFactor))

  if (!is.character(paramList$dateVar)) {
    date <- rlang::enquo(dateVar)
  } else if (is.character(paramList$dateVar)) {
    date <- rlang::quo(!! rlang::sym(dateVar))
  }

  dateQ <- rlang::quo_name(rlang::enquo(date))

  if (!is.character(paramList$timeVar)) {
    time <- rlang::enquo(timeVar)
  } else if (is.character(paramList$timeVar)) {
    time <- rlang::quo(!! rlang::sym(timeVar))
  }

  timeQ <- rlang::quo_name(rlang::enquo(time))

  # check variables
  if(!!dateQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {var}, given for dateVar, cannot be found in the given data frame',
                    var = dateQ))
  }

  if(!!timeQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {var}, given for timeVar, cannot be found in the given data frame',
                    var = timeQ))
  }

  if(!!corrFactor %in% colnames(.data)) {
    stop(glue::glue('A variable named {var}, given for corrFactor, already exists in the given data frame',
                    var = corrFactor))
  }

  # prepare time zone
  if (is.null(tz)){

    tz <- Sys.timezone()

  }

  # concatenate date and time, apply date-time format, and calculate correction factor
  .data %>%
    dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
    dplyr::mutate(dateTimePOSIX =
                    lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                               tz = tz)) %>%
    dplyr::mutate(dateTimePOSIX = base::as.numeric(dateTimePOSIX)) %>%
    dplyr::mutate(totTime = utils::tail(dateTimePOSIX, n=1) - utils::head(dateTimePOSIX, n=1)) %>%
    dplyr::mutate(!!corrFactor := (dateTimePOSIX - utils::head(dateTimePOSIX, n=1)) / totTime) -> .data

  # selectively remove variables
  if (keepDateTime == TRUE){
    .data <- dplyr::select(.data, -c(dateTimePOSIX, totTime))
  } else if (keepDateTime == FALSE){
    .data <- dplyr::select(.data, -c(dateTimePOSIX, totTime, dateTime))
  }

  # return data
  return(.data)
}
