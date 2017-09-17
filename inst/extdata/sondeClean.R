#Script for importing clean test data
sondeClean <- read.csv("correctedData.csv", stringsAsFactors = FALSE)
save(sondeClean, file="sondeClean.RData")
