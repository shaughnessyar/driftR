read.exo <- function(extention, x) {
  library(readr)
  df <- read_csv(extention, skip = x)
  df$'Date (MM/DD/YYYY)' <- as.Date(df$'Date (MM/DD/YYYY)', "%m/%d/%y")
  df$'Time (HH:MM:SS)' <- parse_time(df$'Time (HH:MM:SS)',format='%H:%M:%S')
  return(df)
}
