dr_readExo <- function(fileExtention, skipRows , defineVar=FALSE) {
  if (is.null(defineVar)) {
    df <- read_csv(fileExtention, skip = skipRows)
    df <- as_tibble(df)
    return(df)
  }
  else if (defineVar == "TRUE"){
    df <- read_csv(fileExtention, skip = skipRows)
    df$'Date (MM/DD/YYYY)' <- as.Date(df$'Date (MM/DD/YYYY)', "%m/%d/%y")
    df$'Time (HH:MM:SS)' <- parse_time(df$'Time (HH:MM:SS)',format='%H:%M:%S')
    df <- as_tibble(df)
    return(df)
  }
  else {
    df <- read_csv(fileExtention, skip = skipRows)
    df <- as_tibble(df)
    return(df)
  }
}
