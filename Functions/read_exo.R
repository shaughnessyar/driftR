dr_readExo <- function(extention, x , define=FALSE) {
  if (is.null(define)) {
    df <- read_csv(extention, skip = x)
    df <- as_tibble(df)
    return(df)
  }
  else if (define == "TRUE"){
    df <- read_csv(extention, skip = x)
    df$'Date (MM/DD/YYYY)' <- as.Date(df$'Date (MM/DD/YYYY)', "%m/%d/%y")
    df$'Time (HH:MM:SS)' <- parse_time(df$'Time (HH:MM:SS)',format='%H:%M:%S')
    df <- as_tibble(df)
    return(df)
  }
  else {
    df <- read_csv(extention, skip = x)
    df <- as_tibble(df)
    return(df)
  }
}
