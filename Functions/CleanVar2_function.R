dr_clean2 <- function(dataFrame,varName,calValLow,calStdLow,calValHigh,calStdHigh,correctVar) {
  #CleanVar2(dataframe, variable, cal. value low, cal. std. low, cal. value high, cal. std. high)
  corrVal <- eval(substitute(correctVar), dataFrame)
  raw <- eval(substitute(varName), dataFrame)
  low <- calStdLow+(corrVal*(calStdLow-calValLow))
  high <- calStdHigh-(corrVal*(calStdHigh-calValHigh))
  correct <- (((raw-low)/(high-low))*(calStdHigh-calStdLow))+calStdLow
  return(correct)
}
