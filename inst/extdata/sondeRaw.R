#Script for importing raw test data
sondeRaw <- read.csv("rawData.csv", stringsAsFactors = FALSE)
save(sondeRaw, file="sondeRaw.RData")
