#' Import raw data from water quality instrument
#'
#' @description This function imports the raw data from a YSI Sonde 6600 and EXO2
#'     as well as an Onset U24 Conductivity Logger and formats the data set as a tibble.
#'     If \code{defineVar} is set to \code{TRUE} (the default option), units of measurement will not be
#'     included in the first observation.
#'
#' @usage dr_read(file, instrument, defineVar = TRUE, cleanVar = TRUE, case)
#'
#' @param instrument Which instruments the data was colected with.
#'     Options currently include "Sonde", "EXO", and "HOBO".
#' @param file The name of the file which the data are to be read from. Each row of the table appears
#'     as one line of the file. If it does not contain an absolute path, the file name is relative to
#'     the current working directory.
#' @param defineVar Logical scalar that determines if the units of measurement are included in the first
#'     observation. If they are included, all vectors will be read in as character.
#' @param cleanVar Logical scalar. Should the variable names be cleaned to remove spaces and special
#'     characters? This is implemented using the \code{janitor} package's \code{clean_names} function.
#' @param case Case to convert variable names to, see \code{janitor::clean_names} for details
#'
#' @return A tibble with the formatted data and the variable types defined if \code{defineVar = TRUE}
#'
#' @examples
#' \dontrun{
#' dr_read("data.csv", instrument = Sonde, defineVar = TRUE, cleanVar = TRUE, case = "snake")
#' dr_read("data.csv", instrument = EXO, defineVar = TRUE, cleanVar = TRUE, case = "lower_camel")
#' dr_read("data.csv", instrument = HOBO, defineVar = TRUE, cleanVar = TRUE, case = "all_caps")
#'}
#'
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom readr read_tsv
#' @importFrom readxl read_excel
#'
#' @export

dr_read <- function(file, instrument, defineVar = TRUE, cleanVar = TRUE, case){

  # check file
  if (missing(file)){
    stop("No file path supplied. Please provide a file path for your data.")
  }

  if(!file.exists(file)){
    stop('File cannot be found. Check file name spelling and ensure it is saved in the working directory.')
  }

  # check defineVar
  if (!(typeof(defineVar) %in% c('logical'))) {
    stop(glue::glue('defineVar value {defineVar} not acceptable - value should TRUE or FALSE'))
  }

  # check instrument
  if (missing(instrument)){
    stop("No argument for instrument supplied - value should be one of Sonde, EXO, or HOBO")
  }

  # quote instrument
  instrument <- rlang::quo_name(rlang::enquo(instrument))

  # check file
  if (missing(case)){
    case <- "snake"
  }

  if (instrument == "Sonde" | instrument == "sonde"){
    data <- readSonde(file = file, defineVar = defineVar)
  } else if (instrument == "EXO" | instrument == "exo" | instrument == "Exo"){
    data <- readEXO(file = file, defineVar = defineVar)
  } else if (instrument == "HOBO" | instrument == "hobo" | instrument == "Hobo"){
    data <- readHOBO(file = file, defineVar = defineVar)
  } else {
    stop(glue::glue('Instrument value {instrument} not acceptable - value should be one of Sonde, EXO, or HOBO'))
  }

  if (cleanVar == TRUE){
    . = NULL
    data <- data %>% janitor::clean_names(., case = case) %>%
    stats::setNames(base::gsub("p_h","pH", base::names(.))) %>%
    stats::setNames(base::gsub("PH","pH", base::names(.))) %>%
    stats::setNames(base::gsub("P_H","pH", base::names(.))) %>%
    stats::setNames(base::gsub("Ph","pH", base::names(.)))
  }

  return(data)

}

#defining subfunctions
#YSI Sonde 6600
readSonde <- function(file, defineVar = TRUE) {

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
    df$`Date Time` <- NULL
    df <- tibble::as_tibble(df)

    return(df)
  }
}
