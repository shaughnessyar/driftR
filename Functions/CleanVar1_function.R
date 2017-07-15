dr_clean1 <- function(dataframe,varName,calVal,calStd,correctVar) {
  #CleanVar1(dataframe, variable, cal. value, cal. std.)
  corrVal <- eval(substitute(correctVar), dataframe)
  raw <- eval(substitute(varName), dataframe)
  correct <- raw + (corrVal*(calVal-calStd))
  return(correct)
}
