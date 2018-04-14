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
#' @export
dr_replace <- function(.data, sourceVar, cleanVar = NULL, overwrite = FALSE, dateVar = NULL, timeVar = NULL, from = NULL, to = NULL, tz = NULL, exp){

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$cleanVar)) {
    clean <- rlang::enquo(cleanVar)
  } else if (is.character(paramList$cleanVar)) {
    clean <- rlang::quo(!! rlang::sym(cleanVar))
  }

  cleanVarQ <- rlang::quo_name(rlang::enquo(clean))

  if (!is.character(paramList$sourceVar)) {
    source <- rlang::enquo(sourceVar)
  } else if (is.character(paramList$sourceVar)) {
    source <- rlang::quo(!! rlang::sym(sourceVar))
  }

  sourceVarQ <- rlang::quo_name(rlang::enquo(source))

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
  if (missing(exp) & length(paramList) > 6){
    approach <- 1
  } else if (!missing(exp) & length(paramList) <= 6){
    approach <- 2
  }

  if (approach == 1){

    cleanData <- dr_replace_time(.data, date = date, time = time, from = from, to = to, tz = tz)
    message("Replacement approach - completed using the time arguments.")
    return(cleanData)

  } else if (approach == 2){

    cleanData <- dr_replace_exp(.data, source = source, cleanVarQ = cleanVarQ, clean = clean,
                                replace_exp = exp_enq)
    message("Replacement approach - completed using the expression.")
    return(cleanData)

  }

}

# approach 1
dr_replace_time <- function(.data, date = date, time = time, from = from, to = to, tz = tz){


}

# approach 2
dr_replace_exp <- function(.data, source = NULL, cleanVarQ = NULL, clean = NULL, replace_exp){

  .data %>%
    mutate(!!cleanVarQ := (!!source)) %>%
    mutate(!!cleanVarQ := ifelse(!!replace_exp, NA, !!clean)) -> .data

}
