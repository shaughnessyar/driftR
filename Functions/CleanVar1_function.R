CleanVar1 <- function(dataframe,a,b,c) {
  #CleanVar1(dataframe, variable, cal. value, cal. std.)
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
  output$corr <- output$raw + (output$timefac*(c-b))
  #This is the 1 point calibration equation
  return(output$corr)
  #This exports the result
  #Note that NAs will be introduces for the "unit" cell.
  #Note that you will get an error if cariable name has spaces or special characters. Change variable names befor using. Example variable "Turbidity+" needs to be "Turbidity".
}
