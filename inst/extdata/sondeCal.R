#Script for importing calibration test data
sondeCal <- read.csv("calValues.csv", stringsAsFactors = FALSE)
save(sondeCal, file="sondeCal.RData")
