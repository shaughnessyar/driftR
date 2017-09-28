#' Import raw data
#'
#' @param fileExtention Location of data
#' @param defineVar Logical statement
#' @return A dataframe with with the raw data and the variable types defined if \code{defineVar = TRUE}
#' @examples
#' dr_readSonde("~/fileLocation/data.csv")
#' dr_readSonde("~/fileLocation/data.csv", defineVar= TRUE)
"dr_readSonde" <- function(fileExtention, defineVar=FALSE) {
  if (is.null(defineVar)) {
    allContent = readLines(fileExtention)
    df = read.csv(textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)
    df <- as_tibble(df)
    return(df)
  }
  else if (defineVar == "TRUE"){
    allContent = readLines(fileExtention)
    skipSecond = allContent[-2]
    df = read.csv(textConnection(skipSecond), header = TRUE, stringsAsFactors = FALSE)
    df$Date <- as.Date(df$Date, "%m/%d/%y")
    df$Time <- parse_time(df$Time,format='%H:%M:%S')
    df <- as_tibble(df)
    return(df)
  }
  else {
    allContent = readLines(fileExtention)
    df = read.csv(textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)
    df <- as_tibble(df)
    return(df)
  }
}
