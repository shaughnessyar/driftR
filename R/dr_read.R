#' Import raw data from a YSI Multivariable V2 Sonde
#'
#' @description This function imports the raw data from a YSI Sonde 6600 and EXO2
#'     as well as an Onset U24 Conductivity Logger and formats the data set as a tibble.
#'     If \code{defineVar} is set to \code{TRUE} (the default option), units of measurement will not be
#'     included in the first observation.
#'
#' @usage dr_readSonde(file, instrument, defineVar = TRUE)
#'
#' @param instrument Which instruments the data was colected with.
#'     Options currently include "Sonde", "EXO", and "HOBO".
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
#' dr_read("data.csv", instrument = Sonde, defineVar = TRUE)
#' dr_read("data.csv", instrument = EXO, defineVar = TRUE)
#' dr_read("data.csv", instrument = HOBO, defineVar = TRUE)
#'}
#'
#' @export

dr_read <- function(file, instrument, defineVar = TRUE){
  # check file
  if(!file.exists(file)){
    stop('File cannot be found. Check file name spelling and ensure it is saved in the working directory.')
  }

  # check defineVar
  if (!(typeof(defineVar) %in% c('logical'))) {
    stop(glue::glue('defineVar value {defineVar} not acceptable - value should TRUE or FALSE'))
  }

  instrument <- rlang::quo_name(rlang::enquo(instrument))

  if (instrument == "Sonde"){
    data <- readSonde(file = file, defineVar = defineVar)
    return(data)
  }
  if (instrument == "EXO"){
    data <- readEXO(file = file, defineVar = defineVar)
    return(data)
  }
  if (instrument == "HOBO"){
    data <- readHOBO(file = file, defineVar = defineVar)
    return(data)
  }

  #defining subfunctions
  #YSI Sonde 6600
  readSonde <- function(file, defineVar = TRUE) {

    if(!file.exists(file)){
      stop('File cannot be found. Check file name spelling and ensure it is saved in the working directory.')
    }

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

  #YSI EXO2 Sonde
  readEXO <- function (file, defineVar = TRUE) {
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

  #Onset U24 Conductivity Logger
  readHOBO <- function (file, defineVar = TRUE) {

    # To prevent NOTE from R CMD check 'no visible binding for global variable'
    . = NULL

    fileFormat <- substr(file, nchar(file)-2, nchar(file))

    if (!file.exists(file)) {
      stop("File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
    }
    if (!(typeof(defineVar) %in% c("logical"))) {
      stop(glue::glue("defineVar value {defineVar} not acceptable - value should TRUE or FALSE"))
    }
    if (fileFormat %nin% c("TXT","txt","CSV","csv")) {
      stop(glue::glue("The file format is invalid - format should be .txt or .csv"))
    }
    if (defineVar == FALSE) {
      if (fileFormat == "TXT" | fileFormat == "txt"){
        df <- readr::read_tsv(file, skip = 1)
      }
      if (fileFormat == "CSV" | fileFormat == "csv"){
        df <- readr::read_csv(file, skip = 1)
      }
      df <- tibble::as_tibble(df)
      return(df)

    } else if (defineVar == TRUE) {
      if (fileFormat == "TXT" | fileFormat == "txt"){
        df <- readr::read_tsv(file, skip = 1)
      }
      if (fileFormat == "CSV" | fileFormat == "csv"){
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
}
