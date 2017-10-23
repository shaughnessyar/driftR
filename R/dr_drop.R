#' Dropping observations for equilibration period
#'
#' A wrapper around `dplyr::slice()` for removing observations from both the `head` and the `tail`.
#'
#' When taking the instrument out of the water, there are often several observations that pass
#' before the run can be downloaded. Additionally, once the instrument is in the water, it often
#' takes about 30 minutes for the sensors to equilibrate. This function allows you to drop
#' observations from the bottom and top of the dataset for each of those issues respectively.
#'
#' @usage dr_drop(.data, head = NULL, tail = NULL)
#'
#' @param .data A tbl
#' @param head An integer specifying the number of rows to be removed from the top of \code{.data}
#' @param tail An integer specifying the number of rows to be removed from the bottom of \code{.data}
#'
#' @return An object of the same class as \code{.data} with specified operations removed.
#'
#' @importFrom dplyr slice
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
    if (!(typeof(head) %in% c('integer', 'double')) | head < 0) {
      return(stop('Head value not acceptable - value should be NULL or >= 0'))
    }
  }

  if (!is.null(tail)) {
    if (!(typeof(tail) %in% c('integer', 'double')) | tail < 0) {
      return(stop('Tail value not acceptable - value should be NULL or >= 0'))
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
