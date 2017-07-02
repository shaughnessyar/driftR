corr.frac <- function(dataframe, DateVar, TimeVar, Structure){
  date <- eval(substitute(DateVar), dataframe)
  time <- eval(substitute(TimeVar), dataframe)
  datetime <- paste(date, time)
  datetime <- as.POSIXct(datetime, format = Structure)
  datetime <- as.numeric(datetime)
  tot_time <- tail(datetime,n=1) - head(datetime, n=1)
  corr.frac <- (datetime-head(datetime, n=1))/tot_time
  return(corr.frac)
}
