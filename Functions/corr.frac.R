corr.frac <- function(dataframe, DateVar, TimeVar, format){
  date <- eval(substitute(DateVar), dataframe)
  time <- eval(substitute(TimeVar), dataframe)
  datetime <- paste(date, time)
  
  # set format
  if (format == "MDY"){
    dayTimeFormat <- "%m/%d/%Y %H:%M:%S"
  }
  else if (format == "YMD"){
    dayTimeFormat <- "%Y-%m-%d %H:%M:%S"
  }
  else {
    stop("invalid date-time format - use either MDY or YMD")
  }
    
  # apply format
  datetime <- as.POSIXct(datetime, format = dayTimeFormat)
  datetime <- as.numeric(datetime)
  
  tot_time <- tail(datetime,n=1) - head(datetime, n=1)
  corr.frac <- (datetime-head(datetime, n=1))/tot_time
  return(corr.frac)
}
