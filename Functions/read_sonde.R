read.sonde <- function(extention, define=FALSE) {
  if (is.null(define)) {
    all_content = readLines(extention)
    df = read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE)
    return(df)
  }
  else if (define == "TRUE"){
    all_content = readLines(extention)
    skip_second = all_content[-2]
    df = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
    df$Date <- as.Date(df$Date, "%m/%d/%y")
    df$Time <- parse_time(df$Time,format='%H:%M:%S')
    return(df)
  }
  else {
    all_content = readLines(extention)
    df = read.csv(textConnection(all_content), header = TRUE, stringsAsFactors = FALSE)
    return(df)
  }
}
