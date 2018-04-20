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
    approach <- 1
  }
  if (instrument == "EXO"){
    approach <- 2
  }
  if (instrument == "HOBO"){
    approach <- 3
  }

  if (approach == 1){
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

  if (approach == 2){
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

  if (approach == 3){
    fileFormat <- substr(file, nchar(file)-2, nchar(file))
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
      else if (fileFormat == "CSV" | fileFormat == "csv"){
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
