#' Dropping observations from beginning and end of the monitoring period
#'
#' @description A wrapper around \code{dplyr::slice()} for removing observations from both the \code{head}
#'     and the \code{tail}.
#'
#' @details When taking the instrument out of the water, there are often several observations that pass
#'     before the run can be downloaded. Additionally, once the instrument is in the water, it often
#'     takes about 30 minutes for the sensors to equilibrate. This function allows you to drop
#'     observations from the bottom and top of the dataset for each of those issues respectively.
#'
#' @usage dr_drop(.data, head = NULL, tail = NULL)
#'
#' @param .data A tbl
#' @param head An integer >= 1 specifying the number of rows to be removed from the top
#'     of \code{.data} (or \code{NULL})
#' @param tail An integer >= 1 specifying the number of rows to be removed from the bottom
#'     of \code{.data} (or \code{NULL})
#'
#' @return An object of the same class as \code{.data} with specified observations removed.
#'
#' @importFrom dplyr n
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
#'  dr_drop(testData, head = 2)
#'  dr_drop(testData, tail = 1)
#'  dr_drop(testData, head = 2, tail = 1)
#'
#' @export
dr_drop <- function(.data, head = NULL, tail = NULL){

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
