read.usgs <- function(extention,x,Qvar) {
  library(readr)
  df <- read_csv(extention, skip = x)
  all_content = readLines(df)
  skip_second = all_content[-2]
  df2 = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
  return(df2)
}
