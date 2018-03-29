#' Import raw data from an Onset Computer Corporation U24 Conductivity Logger
#'
#' @description This function imports the raw data from an Onset U24 Conductivity Logger and formats the data set as a tibble.
#'     If \code{defineVar} is set to \code{TRUE} (the default option), units of measurement will not be
#'     included in the first observation.
#'
#' @usage dr_readHOBO(file, defineVar = TRUE, fileFormat = c("TXT", "CSV"))
#'
#' @param file The name of the file which the data are to be read from. Each row of the table appears
#'     as one line of the file. If it does not contain an absolute path, the file name is relative to
#'     the current working directory.
#' @param defineVar Logical scalar that determines if the units of measurement are included in the first
#'     observation. If they are included, all vectors will be read in as character.
#' @param fileFormat The format of the input file. HOBO data can be exported as both .csv and .txt files from the instrument.
#'
#' @return A tibble with the formatted data and the variable types defined if \code{defineVar = TRUE}
#'
#' @examples
#' \dontrun{
#' dr_readHOBO("data.txt", defineVar = TRUE, fileFormat = "TXT")
#' dr_readHOBO("data.csv", defineVar = TRUE, fileFormat = "CSV")
#'}
#'
#' @importFrom readr read_csv
#' @importFrom readr read_tsv
#' @export
dr_readHOBO <- function (file, defineVar = TRUE, fileFormat = c("TXT", "CSV")) {
  if (!file.exists(file)) {
    stop("File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
  }
  if (!(typeof(defineVar) %in% c("logical"))) {
    stop(glue::glue("defineVar value {defineVar} not acceptable - value should TRUE or FALSE"))
  }
  if (fileFormat %nin% c("TXT", "CSV")) {
    stop(glue::glue("The file format {fileFormat} is invalid - format should be TXT or CSV"))
  }
  if (defineVar == FALSE) {
    if (fileFormat == "TXT"){
      df <- readr::read_tsv(file, skip = 1)
    }
    if (fileFormat == "CSV"){
      df <- readr::read_csv(file, skip = 1)
    }
    df <- tibble::as_tibble(df)
    return(df)
  }
  else if (defineVar == TRUE) {
    if (fileFormat == "TXT"){
      df <- readr::read_tsv(file, skip = 1)
    }
    if (fileFormat == "CSV"){
      df <- readr::read_csv(file, skip = 1)
    }
    df <- df %>% stats::setNames(base::gsub(",.*?$","", base::names(.)))
    df$date <- base::gsub(" .*$", "", df$`Date Time`)
    df$time <- base::substr(df$`Date Time`, 10, base::nchar(df$`Date Time`))
    df$time <- base::format(base::strptime(df$time, "%I:%M:%S %p"), "%H:%M:%S")
    df$`Date Time` <- base::paste(df$date, df$time)
    df <- tibble::as_tibble(df)
    return(df)
  }
}
