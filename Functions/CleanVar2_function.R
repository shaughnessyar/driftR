CleanVar2 <- function(dataframe,a,b,c,d,e) {
  #CleanVar2(dataframe, variable, cal. value low, cal. std. low, cal. value high, cal. std. high)
  output <- dataframe
  #changes the dataframe for editing
  output$raw <- eval(substitute(a), output)
  #creates a secondary variable for desired calibrated variable. Avoids using output$a.
  output$raw <- as.numeric(output$raw)
  #Creates a numeric variable
  output$datetime <- paste(output$Date, output$Time)
  #Combines the date and time variables
  output$datetime2 <- as.POSIXct(output$datetime, format='%m/%d/%Y %H:%M:%S')
  output$timedate <- as.numeric(output$datetime2)
  tot_time <- output$timedate[c(nrow(output))]-output$timedate[c(1)]
  output$timefac <- (output$timedate-output$timedate[c(1)])/tot_time
  #determines the time-drift componant
  output$low <- c+output$timefac*(c-b)
  output$high <- e-output$timefac*(e-d)
  #generates calibration values for the 2-point calibrations
  output$corr <- (((output$raw-output$low)/(output$high-output$low))*(e-c))+c
  #carries out 2-point calibration
  return(output$corr)
  #This exports the result

}
