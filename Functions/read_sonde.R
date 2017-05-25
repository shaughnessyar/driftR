read.sonde <- function(extention) {
  library(readr)
  all_content = readLines(extention)
  skip_second = all_content[-2]
  df = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date, "%m/%d/%y")
  df$Time <- parse_time(df$Time,format='%H:%M:%S')
  return(df)
}
