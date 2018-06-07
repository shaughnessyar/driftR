#' Replacing problematic observations from the monitoring period
#'
#' @description \code{dr_replace()} includes two approaches for identifying problematic
#'     observations for specific measurements that should be recoded as missing
#'     values (\code{NA}).
#'
#' @details During monitoring, a sensor malfunction may impact only a single element of
#'     a given reading. Removing the entire observation may therefore be imprudent.
#'     \code{dr_replace()} provides two methods for identifying these values and declaring
#'     them as missing. Values can be identified by specifying one or two timepoints
#'     in the data where problematic measurements begin, end, or fall between. Values
#'     can also be identified based on a problematic sensor value or range of values
#'     using an expression.
#'
#' @usage dr_replace(.data, sourceVar, cleanVar = NULL, overwrite = FALSE, dateVar = NULL,
#'     timeVar = NULL, from = NULL, to = NULL, tz = NULL, exp)
#'
#' @param .data A tbl
#' @param sourceVar Name of variable to replace missing values in
#' @param cleanVar New variable name for cleaned data
#' @param overwrite A logical scalar. Should the current variable be overwritten instead
#'     of creating a new variable?
#' @param dateVar Name of variable containing date data
#' @param timeVar Name of variable containing time data
#' @param from Beginning date and (optionally) time to remove observations
#' @param to End date and (optionally) time to remove observations
#' @param tz String name of timezone, defaults to system's timezone
#' @param exp Unquoted expression
#'
#' @return An object of the same class as \code{.data} with specified observations
#'     recoded as missing.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom lubridate parse_date_time
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom stringr str_c
#'
#' @examples
#' testData <- data.frame(
#'    Date = c("9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/19/2015", "9/21/2015"),
#'    Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
#'    Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
#'    SpCond = c(0.754, 0.750, 0.750, 0.749, 0.749, 0.749),
#'    stringsAsFactors = FALSE
#'  )
#'
#'  dr_replace(testData, sourceVar = Temp, dateVar = Date, timeVar = Time,
#'      from = "2015-09-19 12:30:51", to = "2015-09-21 12:35:51")
#'  dr_replace(testData, sourceVar = Temp, dateVar = Date, timeVar = Time,
#'      from = "2015-09-19", to = "2015-09-21")
#'  dr_replace(testData, sourceVar = Temp, dateVar = Date, timeVar = Time, from = "2015-09-19")
#'  dr_replace(testData, sourceVar = Temp, dateVar = Date, timeVar = Time, to = "2015-09-19")
#'  dr_replace(testData, sourceVar = Temp, cleanVar = temp2, dateVar = Date, timeVar = Time,
#'      to = "09/19/2015 12:35:51")
#'  dr_replace(testData, sourceVar = Temp, overwrite = TRUE, exp = Temp > 14.75)
#'
#' @export
dr_replace <- function(.data, sourceVar, cleanVar = NULL, overwrite = FALSE, dateVar = NULL, timeVar = NULL, from = NULL, to = NULL, tz = NULL, exp){

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$sourceVar)) {
    source <- rlang::enquo(sourceVar)
  } else if (is.character(paramList$sourceVar)) {
    source <- rlang::quo(!! rlang::sym(sourceVar))
  }

  sourceVarQ <- rlang::quo_name(rlang::enquo(source))

  if (overwrite == FALSE & !is.null(paramList$cleanVar)){

    if (!is.character(paramList$cleanVar)) {
      clean <- rlang::enquo(cleanVar)
    } else if (is.character(paramList$cleanVar)) {
      clean <- rlang::quo(!! rlang::sym(cleanVar))
    }

    cleanVarQ <- rlang::quo_name(rlang::enquo(clean))

  } else if (overwrite == FALSE & is.null(paramList$cleanVar)) {

    cleanVar <- stringr::str_c(sourceVarQ, "_na", sep = "")
    clean <- rlang::quo(!! rlang::sym(cleanVar))
    cleanVarQ <- rlang::quo_name(rlang::enquo(clean))

  } else if (overwrite == TRUE){

    if (!is.character(paramList$sourceVar)) {
      clean <- rlang::enquo(sourceVar)
    } else if (is.character(paramList$sourceVar)) {
      clean <- rlang::quo(!! rlang::sym(sourceVar))
    }

    cleanVarQ <- rlang::quo_name(rlang::enquo(clean))

  }

  if (!is.character(paramList$dateVar)) {
    date <- rlang::enquo(dateVar)
  } else if (is.character(paramList$dateVar)) {
    date <- rlang::quo(!! rlang::sym(dateVar))
  }

  if (!is.character(paramList$timeVar)) {
    time <- rlang::enquo(timeVar)
  } else if (is.character(paramList$timeVar)) {
    time <- rlang::quo(!! rlang::sym(timeVar))
  }

  # quote expression
  exp_enq <- enquo(exp)

  # determine replacement approach
  if (missing(exp) & length(paramList) >= 6){
    approach <- 1
  } else if (!missing(exp) & length(paramList) == 4){
    approach <- 2
  } else if (!missing(exp) & (!is.null(paramList$cleanVar) | !is.null(paramList$overwrite)) & length(paramList) == 5){
    approach <- 2
  }  else {
    stop("The combination of arguments supplied for dr_replace is ambiguous.")
  }

  if (approach == 1){

    cleanData <- dr_replace_time(.data, source = source, cleanVarQ = cleanVarQ, clean = clean,
                                 date = date, time = time, from = from, to = to, tz = tz)
    message("Replacement approach - completed using the date/time arguments.")
    return(cleanData)

  } else if (approach == 2){

    cleanData <- dr_replace_exp(.data, source = source, cleanVarQ = cleanVarQ, clean = clean,
                                replace_exp = exp_enq)
    message("Replacement approach - completed using the expression.")
    return(cleanData)

  }

}

# approach 1
dr_replace_time <- function(.data, source = NULL, cleanVarQ = NULL, clean = NULL, date = NULL, time = NULL, from = NULL, to = NULL, tz = NULL){

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  dateTime = dateTimeParse = NULL

  # prepare time zone
  if (is.null(tz)){

    tz <- Sys.timezone()

  }

  # prepare from
  if (!is.null(from)){

    fromVal <- parseFrom(from)

  }

  # prepare to
  if (!is.null(to)){

    toVal <- parseTo(to)

  }

  # perform replace
  if (!is.null(from) & !is.null(to)){
    # drop all observations outside of given range

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::mutate(!!cleanVarQ := (!!source)) %>%
      dplyr::mutate(!!cleanVarQ := ifelse(dateTimeParse < fromVal | dateTimeParse >= toVal, !!clean, NA)) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  }else if (is.null(from) & !is.null(to)){
    # drop all observations up to the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::mutate(!!cleanVarQ := (!!source)) %>%
      dplyr::mutate(!!cleanVarQ := ifelse(dateTimeParse >= toVal, !!clean, NA)) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  } else if (!is.null(from) & is.null(to)){
    # drop all observations beginning with the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::mutate(!!cleanVarQ := (!!source)) %>%
      dplyr::mutate(!!cleanVarQ := ifelse(dateTimeParse < fromVal, !!clean, NA)) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  }

}

# approach 2
dr_replace_exp <- function(.data, source = NULL, cleanVarQ = NULL, clean = NULL, replace_exp){

  .data %>%
    dplyr::mutate(!!cleanVarQ := (!!source)) %>%
    dplyr::mutate(!!cleanVarQ := ifelse(!!replace_exp, NA, !!clean)) -> .data

}
