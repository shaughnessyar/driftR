dr_readSonde <- function(fileExtention, defineVar=FALSE) {
  if (is.null(defineVar)) {
    all_content = readLines(fileExtention)
    df = read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE)
    df <- as_tibble(df)
    return(df)
  }
  else if (defineVar == "TRUE"){
    all_content = readLines(fileExtention)
    skip_second = all_content[-2]
    df = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
    df$Date <- as.Date(df$Date, "%m/%d/%y")
    df$Time <- parse_time(df$Time,format='%H:%M:%S')
    df <- as_tibble(df)
    return(df)
  }
  else {
    all_content = readLines(fileExtention)
    df = read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE)
    df <- as_tibble(df)
    return(df)
  }
}
