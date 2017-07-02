read.exo <- function(extention, x , define=NULL) {
  if (is.null(define)) {
    library(readr)
    df <- read_csv(extention, skip = x)
    df$'Date (MM/DD/YYYY)' <- as.Date(df$'Date (MM/DD/YYYY)', "%m/%d/%y")
    df$'Time (HH:MM:SS)' <- parse_time(df$'Time (HH:MM:SS)',format='%H:%M:%S')
    return(df)
  }
  else if (define == "FALSE"){
    library(readr)
    df <- read_csv(extention, skip = x)
    return(df)
  }
  else {
    library(readr)
    df <- read_csv(extention, skip = x)
    df$'Date (MM/DD/YYYY)' <- as.Date(df$'Date (MM/DD/YYYY)', "%m/%d/%y")
    df$'Time (HH:MM:SS)' <- parse_time(df$'Time (HH:MM:SS)',format='%H:%M:%S')
    return(df)
}
