#' Dropping observations from the monitoring period
#'
#' @description \code{dr_drop()} includes three approaches for removing observations from the
#'     monitoring period. Observations may be removed by specifying the number to remove from
#'     the head and/or the tail of the observation. They may also be removed by specifying
#'     one or two timepoints in the data where problematic observations begin, end, or
#'     fall between. Finally observations may be removed based on a problematic sensor value
#'     or range of values using an expression.
#'
#' @details When taking the instrument out of the water, there are often several observations that pass
#'     before the run can be downloaded. Additionally, once the instrument is in the water, it often
#'     takes about 30 minutes for the sensors to equilibrate. This function allows you to drop
#'     observations from the bottom and top of the data set for each of those issues respectively. This
#'     function also provides approaches for removing observations from the middle of the data set.
#'
#' @usage dr_drop(.data, head = NULL, tail = NULL, dateVar = NULL, timeVar = NULL, from = NULL,
#'     to = NULL, tz = NULL, exp)
#'
#' @param .data A tbl
#' @param head An integer >= 1 specifying the number of rows to be removed from the top
#'     of \code{.data} (or \code{NULL})
#' @param tail An integer >= 1 specifying the number of rows to be removed from the bottom
#'     of \code{.data} (or \code{NULL})
#' @param dateVar Name of variable containing date data
#' @param timeVar Name of variable containing time data
#' @param from Beginning date and (optionally) time to remove observations
#' @param to End date and (optionally) time to remove observations
#' @param tz String name of timezone, defaults to system's timezone
#' @param exp Unquoted expression
#'
#' @return An object of the same class as \code{.data} with specified observations removed.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom glue glue
#' @importFrom lubridate parse_date_time
#' @importFrom rlang enquo
#' @importFrom rlang quo
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
#'  dr_drop(testData, head = 2)
#'  dr_drop(testData, tail = 1)
#'  dr_drop(testData, head = 2, tail = 1)
#'  dr_drop(testData, dateVar = Date, timeVar = Time, from = "9/19/2015")
#'  dr_drop(testData, dateVar = Date, timeVar = Time, from = "9/18/2015 12:25:51")
#'  dr_drop(testData, dateVar = Date, timeVar = Time, to = "9/19/2015")
#'  dr_drop(testData, dateVar = Date, timeVar = Time, to = "9/18/2015 12:25:51")
#'  dr_drop(testData, dateVar = Date, timeVar = Time, from = "9/18/2015 12:25:51",
#'      to = "9/19/2015 12:30:51")
#'  dr_drop(testData, dateVar = Date, timeVar = Time, from = "9/18/2015 12:00", to = "9/19/2015 13:00")
#'  dr_drop(testData, exp = Temp > 14.7)
#'
#' @export
dr_drop <- function(.data, head = NULL, tail = NULL, dateVar = NULL, timeVar = NULL, from = NULL, to = NULL, tz = NULL, exp){

  # save parameters to list
  paramList <- as.list(match.call())

  if (length(paramList) <= 2) {
    return(stop('At least 1 observation must be removed from the data frame'))
  }

  # quote input variables
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
  filter_exp_enq <- enquo(exp)

  # determine drop approach
  if (!is.null(head) & !is.null(tail) & length(paramList) == 4){
    approach <- 1
  } else if (!is.null(head) & is.null(tail) & length(paramList) == 3){
    approach <- 1
  } else if (is.null(head) & !is.null(tail) & length(paramList) == 3){
    approach <- 1
  } else if (is.null(head) & is.null(tail) & missing(exp)){
    approach <- 2
  } else if (!missing(exp) & length(paramList) == 3){
    approach <- 3
  } else {
    stop("The combination of arguments supplied for dr_drop is ambiguous.")
  }

  if (approach == 1){

    cleanData <- dr_drop_slice(.data, head = head, tail = tail)
    message("Drop approach - completed using the head and/or tail arguments.")
    return(cleanData)

  } else if (approach == 2){

    cleanData <- dr_drop_time(.data, date = date, time = time, from = from, to = to, tz = tz)
    message("Drop approach - completed using the date/time arguments.")
    return(cleanData)

  } else if (approach == 3){

    cleanData <- dr_drop_exp(.data, filter_exp = filter_exp_enq)
    message("Drop approach - completed using the expression.")
    return(cleanData)

  }

}

# approach 1
dr_drop_slice <- function(.data, head = NULL, tail = NULL){

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  n = NULL

  # Check for input errors
  if (!is.null(head)) {
    if (!(typeof(head) %in% c('integer', 'double'))) {
      return(stop(glue::glue('Head value {head} not acceptable - value should be NULL or integer >= 1')))
    }

    if ((head %% 1 != 0) | (head <= 0)) {
      return(stop(glue::glue('Head value {head} not acceptable - value should be NULL or integer >= 1')))
    }
  }

  if (!is.null(tail)) {
    if (!(typeof(tail) %in% c('integer', 'double'))) {
      return(stop(glue::glue('Tail value {tail} not acceptable - value should be NULL or integer >= 1')))
    }

    if ((tail %% 1 != 0) | (tail <= 0)) {
      return(stop(glue::glue('Tail value {tail} not acceptable - value should be NULL or integer >= 1')))
    }
  }

  # calculate slice positions
  headPos <- head+1

  rows <- nrow(.data)
  tailPos <- rows-tail

  # evaluate head and tail
  if (base::is.null(head)){
    dplyr::slice(.data, 1:tailPos)
  }

  else if (base::is.null(tail)) {
    dplyr::slice(.data, headPos:n())
  }

  else {
    dplyr::slice(.data, headPos:tailPos)
  }

}

# approach 2
dr_drop_time <- function(.data, date = NULL, time = NULL, from = NULL, to = NULL, tz = NULL){

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

  # perform drop
  if (!is.null(from) & !is.null(to)){
    # drop all observations outside of given range

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse < fromVal | dateTimeParse >= toVal) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  } else if (is.null(from) & !is.null(to)){
    # drop all observations up to the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse >= toVal) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  } else if (!is.null(from) & is.null(to)){
    # drop all observations beginning with the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse < fromVal) %>%
      dplyr::select(-dateTime, -dateTimeParse) -> .data

  }


}

# approach 3
dr_drop_exp <- function(.data, filter_exp){

  .data %>%
    filter(!(!!filter_exp)) -> .data

}
