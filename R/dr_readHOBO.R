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
