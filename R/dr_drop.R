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
#'     function can remove data points above or below a certain threshold for a specified parameter
#'
#' @usage dr_drop((.data, head = NULL, tail = NULL, from = c("Date", "Time"), to =  c("Date", "Time"), expression = c("symb", "val"), var = NA, dropAll = FALSE)
#'
#' @param .data A tbl
#' @param head An integer >= 1 specifying the number of rows to be removed from the top
#'     of \code{.data} (or \code{NULL})
#' @param tail An integer >= 1 specifying the number of rows to be removed from the bottom
#'     of \code{.data} (or \code{NULL})
#' @param ... other optional parameters(i.e., from, to, expression, var, dropAll)
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
#'  dr_drop(testData, from = c("9/18/2015", "12:10:49"), to = c("9/18/2015", "12:15:50"), dropAll = TRUE)
#'  dr_drop(testData, from = c("9/18/2015", "12:10:49"), to = c("9/18/2015", "12:15:50"), var = Temp)
#'  dr_drop(testData, expression = c(">", 14.5), var = SpCond)
#'
#' @export
dr_drop <- function(.data, head = NULL, tail = NULL, ...){

  # To prevent NOTE from R CMD check 'no visible binding for global variable'
  n = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  if (base::is.null(head) == FALSE | base::is.null(tail) == FALSE){

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
  else if ("from" %in% names(paramList) && "to" %in% names(paramList)){
    dateVar <-colnames(.data[,which(grepl(gsub("()", "", paramList$from[2]), .data))])
    timeVar <-colnames(.data[,which(grepl(gsub("()", "", paramList$from[3]), .data))])
    dateVar <- rlang::quo(!! rlang::sym(dateVar))
    timeVar <- rlang::quo(!! rlang::sym(timeVar))
    startDate <- gsub("()", "", paramList$from[2])
    startTime <- gsub("()", "", paramList$from[3])
    endDate <- gsub("()", "", paramList$to[2])
    endTime <- gsub("()", "", paramList$to[3])
    a <- "newDate"
    b <- "newTime"
    newDate <- rlang::quo_name(rlang::enquo(a))
    newTime <- rlang::quo_name(rlang::enquo(b))
    .data <- .data %>% dplyr::mutate(newDate := (!!dateVar)) %>% dplyr::mutate(newTime := (!!timeVar))
    start <- which(grepl(startDate, .data$newDate)==TRUE & grepl(startTime, .data$newTime)==TRUE)
    end <- which(grepl(endDate, .data$newDate)==TRUE & grepl(endTime, .data$newTime)==TRUE)
    .data$newDate <- NULL
    .data$newTime <- NULL

    if ("dropAll" %in% names(paramList) && paramList$dropAll == TRUE){

      .data[start:end,] <- NA
      return(.data)
    }

    else if ("dropAll" %nin% names(paramList)){
      var <- paramList$var
      if (!is.character(paramList$var)) {
        dropVar <- rlang::enquo(var)
      } else if (is.character(paramList$var)) {
        dropVar <- rlang::quo(!! rlang::sym(var))
      }
      dropVarQ <- rlang::quo_name(rlang::enquo(var))

      if(!!dropVarQ %nin% colnames(.data)) {
        stop(glue::glue('Variable {var}, given for var, cannot be found in the given data frame',
                        var = dropVarQ))
      }
      .data[c(start:end), dropVarQ] <- NA
      return(.data)
    }
  }
  else if ("expression" %in% names(paramList)){
    if (!is.character(paramList$var)) {
      dropVar <- rlang::enquo(var)
    } else if (is.character(paramList$var)) {
      dropVar <- rlang::quo(!! rlang::sym(var))
    }
    dropVarQ <- rlang::quo_name(rlang::enquo(var))

    if(!!dropVarQ %nin% colnames(.data)) {
      stop(glue::glue('Variable {var}, given for var, cannot be found in the given data frame',
                      var = dropVarQ))
    }
    e <- "tempVar"
    tempVar <- rlang::quo_name(rlang::enquo(e))
    .data <- .data %>% dplyr::mutate(tempVar := (!!dropVar))
    num <- gsub("()", "", paramList$expression[3])
    if (gsub("()", "", paramList$expression[2]) == ">"){
      index_list <- which(.data$tempVar > num)
      .data$tempVar <- NULL
      .data[c(index_list), dropVarQ] <- NA
      return(.data)
    }
    else if (gsub("()", "", paramList$expression[2]) == "<"){
      index_list <- which(.data$tempVar < num)
      .data$tempVar <- NULL
      .data[c(index_list), dropVarQ] <- NA
      return(.data)
    }
    else if (gsub("()", "", paramList$expression[2]) == ">="){
      index_list <- which(.data$tempVar >= num)
      .data$tempVar <- NULL
      .data[c(index_list), dropVarQ] <- NA
      return(.data)
    }
    else if (gsub("()", "", paramList$expression[2]) == "<="){
      index_list <- which(.data$tempVar <= num)
      .data$tempVar <- NULL
      .data[c(index_list), dropVarQ] <- NA
      return(.data)
    }
    else if (gsub("()", "", paramList$expression[2]) == "=="){
      index_list <- which(.data$tempVar == num)
      .data$tempVar <- NULL
      .data[c(index_list), dropVarQ] <- NA
      return(.data)
    }
  }
  else{
    stop("At least 1 observation must be removed from the data frame")
  }
}
