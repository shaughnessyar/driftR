#'
#' @export
dr_drop_full <- function(.data, head = NULL, tail = NULL, date = NULL, time = NULL, from = NULL, to = NULL, exp = NULL, tz = NULL){

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$date)) {
    dateVar <- rlang::enquo(date)
  } else if (is.character(paramList$date)) {
    dateVar <- rlang::quo(!! rlang::sym(date))
  }

  if (!is.character(paramList$time)) {
    timeVar <- rlang::enquo(time)
  } else if (is.character(paramList$time)) {
    timeVar <- rlang::quo(!! rlang::sym(time))
  }

  if (!missing(head) & !missing(tail)){

    cleanData <- dr_drop_slice(.data, head = head, tail = tail)
    message("Drop approach - completed using the head and tail arguments.")
    return(cleanData)

  } else if (!missing(date) & !missing(time)){

    cleanData <- dr_drop_time(.data, date = dateVar, time = timeVar, from = from, to = to, tz = tz)
    message("Drop approach - completed using the time arguments.")
    return(cleanData)

  } else if (!missing(exp)){

    cleanData <- dr_drop_exp(.data, exp = exp)
    message("Drop approach - completed using the expression.")
    return(cleanData)

  }

}

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

  if (is.null(head) & is.null(tail)) {
    return(stop('At least 1 observation must be removed from the data frame'))
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

dr_drop_time <- function(.data, date = NULL, time = NULL, from = NULL, to = NULL, tz = NULL){

  # prepare time zone
  if (is.null(tz)){

    tz <- Sys.timezone()

  }

  # prepare from
  if (!is.null(from)){

    fromVal <- as.character(
      lubridate::parse_date_time(from, orders = c("ymd", "dmy", "mdy", "ymd HMS", "dmy HMS", "mdy HMS"))
      )

  }

  # prepare to
  if (!is.null(to)){

    toVal <- as.character(
      lubridate::parse_date_time(to, orders = c("ymd", "dmy", "mdy", "ymd HMS", "dmy HMS", "mdy HMS"))
    )

  }

  # perform drop
  if (!is.null(from) & !is.null(to)){
    # drop all observations outside of given range

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "dmy HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse >= fromVal & dateTimeParse <= toVal) -> .data

  } else if (is.null(from) & !is.null(to)){
    # drop all observations up to the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "dmy HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse < toVal) -> .data

  } else if (!is.null(from) & is.null(to)){
    # drop all observations beginning with the specified date/time

    .data %>%
      dplyr::mutate(dateTime := stringr::str_c(!!date, !!time, sep = " ")) %>%
      dplyr::mutate(dateTimeParse =
                      lubridate::parse_date_time(dateTime, orders = c("ymd HMS", "dmy HMS", "mdy HMS"),
                                                 tz = tz)) %>%
      dplyr::filter(dateTimeParse > fromVal) -> .data

  }


}
