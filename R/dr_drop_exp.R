#' Dropping observations from beginning and end of the monitoring period, over a specified date range, or if a value meets a specified criteria
#'
#' @description A wrapper around \code{dplyr::slice()} for removing observations from both the \code{head}
#'     and the \code{tail}. Additional capabilityies include removing from a specified date range or by a logical expression
#'
#' @details When taking the instrument out of the water, there are often several observations that pass
#'     before the run can be downloaded. Additionally, once the instrument is in the water, it often
#'     takes about 30 minutes for the sensors to equilibrate. This function allows you to drop
#'     observations from the bottom and top of the dataset for each of those issues respectively.
#'     Another issue arrises if the water level drops below the level of the sensors in the middle of a deployment.
#'     \code{dr_drop()} can remove this data from the dataset by either entire timeperiods or by individual parameters.
#'     Lastly, sometimes there is a malfunction in a sensor and it has erroneous measurements taken throughout the dataset. This
#'     function can remove data points above or below a certain threshold for a specified parameter.
#'     Acceptable combinations of parameters are 1) .data, head, 2) .data, tail, 3) .data, head, tail,
#'     4).data, from, to, var, 5) .data, from, to, dropAll, and 6).data, expression.
#'
#' @usage dr_drop_exp(.data, head, tail, from , to , var, dropAll, expression)
#'
#' @param .data A tbl
#' @param head An integer >= 1 specifying the number of rows to be removed from the top
#'     of \code{.data} (or \code{NULL})
#' @param tail An integer >= 1 specifying the number of rows to be removed from the bottom
#'     of \code{.data} (or \code{NULL})
#' @param from A vector of two strings: Date and Time for the start of the period to be dropped
#' @param to A vector of two strings: Date and Time for the end of the period to be dropped
#' @param var Name of variable to be dropped.
#' @param dropAll A logical statement that will drop all observations in the specified period
#' @param expression A string containing the variable name an expression to drop
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
#'  dr_drop(testData, head = 2, tail = 1)
#'  dr_drop(testData, from = c("9/18/2015", "12:10:49"),
#'      to = c("9/18/2015", "12:15:50"), dropAll = TRUE)
#'  dr_drop(testData, from = c("9/18/2015", "12:10:49"),
#'      to = c("9/18/2015", "12:15:50"), var = Temp)
#'  dr_drop(testData, expression = "SpCond >= 0.75")
#'
#' @export
dr_drop_exp <- function(.data, head, tail, from, to, var, dropAll, expression){

  n = NULL

  option1 <- c("", ".data", "head")
  option2 <- c("", ".data", "tail")
  option3 <- c("", ".data", "head", "tail")
  option4 <- c("", ".data", "from", "to", "dropAll")
  option5 <- c("", ".data", "from", "to", "var")
  option6 <- c("", ".data", "expression")

  testList <- list(option1, option2, option3, option4, option5, option6)

  paramList <- as.list(match.call())

  .args <- as.list(match.call()[-1])

  if(list(names(paramList)) %nin% testList){

    stop("Incorrect argument combination. See dr_drop documentation.")

  } else if(which(testList %in% list(names(paramList)))==1){

    .data <- do.call(dropMethod1, .args)

  } else if(which(testList %in% list(names(paramList)))==2){

    .data <- do.call(dropMethod1, .args)

  } else if(which(testList %in% list(names(paramList)))==3){

    .data <- do.call(dropMethod1, .args)

  } else if(which(testList %in% list(names(paramList)))==4){

    .data <- do.call(dropMethod2, .args)

  } else if(which(testList %in% list(names(paramList)))==5){

    .data <- do.call(dropMethod2, .args)

  } else if(which(testList %in% list(names(paramList)))==6){

    .data <- do.call(dropMethod3, .args)

  }

  return(.data)

}
