dropMethod2 <- function(.data, from = NULL, to = NULL, var = NULL, dropAll = NULL){

  paramList <- as.list(match.call())
  dateVar <-colnames(.data[,which(grepl(paramList$from[[2]], .data))])
  timeVar <-colnames(.data[,which(grepl(paramList$from[[3]], .data))])
  dateVar <- rlang::quo(!! rlang::sym(dateVar))
  timeVar <- rlang::quo(!! rlang::sym(timeVar))
  startDate <- paramList$from[[2]]
  startTime <- paramList$from[[3]]
  endDate <- paramList$to[[2]]
  endTime <- paramList$to[[3]]
  a <- "newDate"
  b <- "newTime"
  newDate <- rlang::quo_name(rlang::enquo(a))
  newTime <- rlang::quo_name(rlang::enquo(b))
  .data <- .data %>% dplyr::mutate(newDate := (!!dateVar)) %>% dplyr::mutate(newTime := (!!timeVar))
  start <- which(grepl(startDate, .data$newDate)==TRUE & grepl(startTime, .data$newTime)==TRUE)
  end <- which(grepl(endDate, .data$newDate)==TRUE & grepl(endTime, .data$newTime)==TRUE)
  .data$newDate <- NULL
  .data$newTime <- NULL

  if ("dropAll" %in% names(paramList)){

    .data[start:end,] <- NA
    return(.data)
  }

  else if ("var" %in% names(paramList)){
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
