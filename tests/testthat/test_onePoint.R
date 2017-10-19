context("test data one point correction process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result$factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")
result$SpCond_Corr <- dr_clean1(result, SpCond, calVal = 1.07, calStd = 1, correctVar = factors)

test_that("one point correction", {
  expect_equal(result$SpCond_Corr, test_data1$SpCond_Corr)
})
