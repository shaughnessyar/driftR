dr_cleanVar1 <- function(dataframe,a,b,c) {
  #CleanVar1(dataframe, variable, cal. value, cal. std.)
  raw <- eval(substitute(a), dataframe)
  correct <- raw + (corr.frac*(c-b))
  return(correct)
}
