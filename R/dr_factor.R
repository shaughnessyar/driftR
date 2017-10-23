#' Creating correction factors
#'
#' A wrapper around `dplyr::mutate()` that creates a correction factor for each observation.
#'
#' Correction factors are calculated based on the time of the observation and the total amount of time
#' that the instrument had been deployed.
#'
#' @usage dr_factor(.data, corrFactor, dateVar, timeVar, format = c("MDY", "YMD"))
#'
#' @param .data A tbl
#' @param corrFactor New variable name for correction factor data
#' @param dateVar Vector containing date data
#' @param timeVar Vector containing time data
#' @param format Either "MDY" or "YMD" for \code{dateVar}
#'
#' @return An object of the same class as \code{.data} with the new correction factor variable added
#' to the other data in \code{.data}.
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
#' dr_factor(testData, corrFactor = corrFac, dateVar = Date, timeVar = Time, format = "MDY")
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#'
#' @export
dr_factor <- function(.data, corrFactor, dateVar, timeVar, format = c("MDY", "YMD")) {

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  dateTime = totTime = NULL

  # quote input variables
  corrFactor <- quo_name(enquo(corrFactor))
  date <- enquo(dateVar)
  dateQ <- quo_name(enquo(dateVar))
  time <- enquo(timeVar)
  timeQ <- quo_name(enquo(timeVar))

  # check variables
  if(!!dateQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {dv}, given for dateVar, cannot be found in the given data frame',
                    dv = dateQ))
  }

  if(!!timeQ %nin% colnames(.data)) {
    stop(glue::glue('Variable {tv}, given for timeVar, cannot be found in the given data frame',
                    tv = timeQ))
  }

  if(!!corrFactor %in% colnames(.data)) {
    stop(glue::glue('A variable named {cf}, given for corrFactor, already exists in the given data frame',
                    cf = corrFactor))
  }

  # set format
  if (format == "MDY"){
    dayTimeFormat <- "%m/%d/%Y %H:%M:%S"
  }
  else if (format == "YMD"){
    dayTimeFormat <- "%Y-%m-%d %H:%M:%S"
  }
  else {
    stop("Invalid date-time format - use either MDY or YMD and ensure they are quoted")
  }

  # concatenate date and time, apply date-time format, and calculate correction factor
  .data %>%
    dplyr::mutate(dateTime = stringr::str_c(!!date, !!time, sep = " ", collapse = NULL)) %>%
    dplyr::mutate(dateTime = base::as.POSIXct(dateTime, format = dayTimeFormat)) %>%
    dplyr::mutate(dateTime = base::as.numeric(dateTime)) %>%
    dplyr::mutate(totTime = utils::tail(dateTime, n=1) - utils::head(dateTime, n=1)) %>%
    dplyr::mutate(!!corrFactor := (dateTime - utils::head(dateTime, n=1)) / totTime) %>%
    select(-dateTime, -totTime)
}
