CleanVar2 <- function(dataframe,a,b,c,d,e) {
  #CleanVar2(dataframe, variable, cal. value low, cal. std. low, cal. value high, cal. std. high)
  raw <- eval(substitute(a), dataframe)
  low <- c+(corr.frac*(c-b))
  high <- e-(corr.frac*(e-d))
  correct <- (((raw-low)/(high-low))*(e-c))+c
  return(correct)
}
