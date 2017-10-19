#' Import raw data
#'
#' @param fileExtention Location of data
#' @param defineVar Logical statement
#' @return A dataframe with with the raw data and the variable types defined if \code{defineVar = TRUE}
#' @examples
#' \dontrun{
#' dr_readSonde("~/fileLocation/data.csv")
#' dr_readSonde("~/fileLocation/data.csv", defineVar= TRUE)
#'}
#'
#' @export
"dr_readSonde" <- function(fileExtention, defineVar=FALSE) {
  if (base::is.null(defineVar)) {
    allContent = base::readLines(fileExtention)
    df = utils::read.csv(base::textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)
    df <- dplyr::as_tibble(df)
    return(df)
  }
  else if (defineVar == "TRUE"){
    allContent = base::readLines(fileExtention)
    skipSecond = allContent[-2]
    df = utils::read.csv(base::textConnection(skipSecond), header = TRUE, stringsAsFactors = FALSE)
    df <- dplyr::as_tibble(df)
    return(df)
  }
  else {
    allContent = base::readLines(fileExtention)
    df = utils::read.csv(base::textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)
    df <- dplyr::as_tibble(df)
    return(df)
  }
}
