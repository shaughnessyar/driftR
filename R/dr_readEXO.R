#' Import raw data from a YSI EXO2 Sonde
#'
#' @description This function imports the raw data from a YSI EXO2 Sonde and formats the data set as a tibble.
#'     If \code{defineVar} is set to \code{TRUE} (the default option), units of measurement will not be
#'     included in the first observation.
#'
#' @usage dr_readEXO(file, defineVar = TRUE)
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
#' dr_readEXO("data.xlsx")
#' dr_readEXO("data.xlsx", defineVar = TRUE)
#'}
#'
#' @export
dr_readEXO <- function (file, defineVar = TRUE) {
  if (!file.exists(file)) {
    stop("File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
  }
  if (!(typeof(defineVar) %in% c("logical"))) {
    stop(glue::glue("defineVar value {defineVar} not acceptable - value should TRUE or FALSE"))
  }
  if (defineVar == FALSE) {
    df <- readxl::read_excel(file)
    df <- tibble::as_tibble(df)
    return(df)
  }
  else if (defineVar == TRUE) {
    df <- readxl::read_excel(file)
    a <- which(df[,1] == "1")[1]
    df <- readxl::read_excel(file, skip = (a+1))
    df$`Date (MM/DD/YYYY)` <- base::as.character(df$`Date (MM/DD/YYYY)`)
    df$`Time (HH:MM:SS)` <- base::as.character(df$`Time (HH:MM:SS)`)
    df$`Time (HH:MM:SS)` <-  base::gsub("^.*? ", "", df$`Time (HH:MM:SS)`)
    df <- tibble::as_tibble(df)
    return(df)
  }
}
