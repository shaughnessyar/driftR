#Script for importing clean test data
sondeClean <- read.csv("sondClean.csv", stringsAsFactors = FALSE)
save(sondeClean, file="sondeClean.RData")
