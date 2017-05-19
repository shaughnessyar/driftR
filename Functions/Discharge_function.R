#library(readr)
#df2 <- read_csv("~/extention/fileName.csv")
#imports the discharge data and defines it as "df2"

#df1 <- read_csv("~/extention/fileName.csv")
#imports the YSI Sonde data and defines it as "df1"

#input <- "fileName"
#This defines what you want the output file to be named later

MergeDischarge <- function(dataframe1,dataframe2,`a`) {
  wq <- dataframe1
  discharge <- dataframe2
  #This creates new dataframes from the original dataframes

  discharge$Q <- eval(substitute(`a`), discharge)
  Qdta <- discharge[,c("datetime", "Q" )]
  #This subsets the data so that the only thing left is the datetime and discharge

  Qdta <- Qdta [-c(1), ]
  #drops the "unit" observation

  wq$datetime2 <- paste(wq$Date, wq$Time)
  wq$datetime2 <- as.POSIXct(wq$datetime2, format='%m/%d/%Y %H:%M:%S')
  wq$datetime2 <- as.numeric(wq$datetime2)
  #creates a numeric variable for the observational unit in the waterquality dataframe

  Qdta$datetime2 <- as.POSIXct(Qdta$datetime, format='%m/%d/%Y %H:%M')
  Qdta$datetime2 <- as.numeric(Qdta$datetime2)
  #creates a numeric variable for the observational unit in the waterquality dataframe

  wq$datetime3 <- substr(wq$datetime2,1,nchar(wq$datetime2)-2)
  Qdta$datetime3 <- substr(Qdta$datetime2,1,nchar(Qdta$datetime2)-2)
  #this subtracts the last 2 didgets of the date codes to account for varitions in seconds.

  combine <- merge(wq, Qdta, by.x='datetime3', by.y='datetime3')
  #This merges the two dataframes together

  combine$"Q(cfs)" <- as.numeric(combine$Q)
  combine$"Q(cms)" <- combine$"Q(cfs)"*0.028316847
  combine$Q <- NULL
  #This changes the discharge data into numeric data and adds it in both cubic feet and meters per second.

  combine$datetime3 <- NULL
  combine$`datetime2.x` <-NULL
  combine$`datetime2.y` <-NULL
  #deletes extra variables

  write.csv(combine, file = paste(input, "merge.csv", sep="_"))
  #This writes a csv file with the cleaned data.
}
