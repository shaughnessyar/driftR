#' Creating correction factors
#'
#' @param dataFrame The working data frame
#' @param dateVar Date variable name
#' @param timeVar Time variable name
#' @param format Either "MDY" or "YMD" for \code{dateVar}
#' @return A series of correction factors
#' @examples
#' dr_correct(df, Date, Time, "YMD")
#' dr_correct(df, X, Y, "MDY")
dr_correct <- function(dataFrame, dateVar, timeVar, format){
    date <- eval(substitute(dateVar), dataFrame)
    time <- eval(substitute(timeVar), dataFrame)
    dateTime <- paste(date, time)

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
   dateTime <- as.POSIXct(dateTime, format = dayTimeFormat)
   dateTime <- as.numeric(dateTime)

   totTime <- tail(dateTime,n=1) - head(dateTime, n=1)
   corrFrac <- (dateTime-head(dateTime, n=1))/totTime
   return(corrFrac)
}
