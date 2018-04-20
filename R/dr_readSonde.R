#' Import raw data from a YSI Multivariable V2 Sonde
#'
#' @description This function imports the raw data from a YSI Sonde and formats the data set as a tibble.
#'     If \code{defineVar} is set to \code{TRUE} (the default option), units of measurement will not be
#'     included in the first observation.
#'
#' @usage dr_readSonde(file, defineVar = TRUE)
#'
#' @param file The name of the file which the data are to be read from. Each row of the table appears
#'     as one line of the file. If it does not contain an absolute path, the file name is relative to
#'     the current working directory.
#' @param defineVar Logical scalar that determines if the units of measurement are included in the first
#'     observation. If they are included, all vectors will be read in as character.
#'
#' @return A tibble with the formatted data and the variable types defined if \code{defineVar = TRUE}
#'
#' @examples
#' \dontrun{
#' dr_readSonde("data.csv")
#' dr_readSonde("data.csv", defineVar = TRUE)
#'}
#'
#' @export
dr_readSonde <- function(file, defineVar = TRUE) {
  #warning message
  .Deprecated("dr_read")
  
  # check file
  if(!file.exists(file)){
    stop('File cannot be found. Check file name spelling and ensure it is saved in the working directory.')
  }

  # check defineVar
  if (!(typeof(defineVar) %in% c('logical'))) {
    stop(glue::glue('defineVar value {defineVar} not acceptable - value should TRUE or FALSE'))
  }

  if (defineVar == FALSE) {
    allContent <- base::readLines(file)
    df <- utils::read.csv(base::textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)
    df <- tibble::as_tibble(df)
    return(df)
  }
  else if (defineVar == TRUE){
    allContent <- base::readLines(file)
    skipSecond <- allContent[-2]
    df <- utils::read.csv(base::textConnection(skipSecond), header = TRUE, stringsAsFactors = FALSE)
    df <- tibble::as_tibble(df)
    return(df)
  }
}
