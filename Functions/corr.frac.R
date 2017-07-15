dr_corrFrac <- function(dataframe, DateVar, TimeVar, type){
  if (type=="1") {
    date <- eval(substitute(DateVar), dataframe)
    time <- eval(substitute(TimeVar), dataframe)
    datetime <- paste(date, time)
    datetime <- as.POSIXct(datetime, format = "%m/%d/%Y %H:%M:%S")
    datetime <- as.numeric(datetime)
    tot_time <- tail(datetime,n=1) - head(datetime, n=1)
    corr.frac <- (datetime-head(datetime, n=1))/tot_time
    return(corr.frac)
  }
  else {
    date <- eval(substitute(DateVar), dataframe)
    time <- eval(substitute(TimeVar), dataframe)
    datetime <- paste(date, time)
    datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")
    datetime <- as.numeric(datetime)
    tot_time <- tail(datetime,n=1) - head(datetime, n=1)
    corr.frac <- (datetime-head(datetime, n=1))/tot_time
    return(corr.frac)
  }
}
