context("test data one point correction process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result <- dr_correct(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "MDY")
result <- dr_clean1(result, sourceVar = SpCond, cleanVar = SpCond_Corr, calVal = 1.07, calStd = 1, correctVar = factors)

test_that("one point correction", {
  expect_equal(result$SpCond_Corr, test_data1$SpCond_Corr)
})
