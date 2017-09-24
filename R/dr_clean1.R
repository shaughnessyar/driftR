dr_clean1 <- function(dataFrame,varName,calVal,calStd,correctVar) {
  #CleanVar1(dataframe, variable, cal. value, cal. std.)
  corrVal <- eval(substitute(correctVar), dataFrame)
  raw <- eval(substitute(varName), dataFrame)
  correct <- raw + (corrVal*(calVal-calStd))
  return(correct)
}
