context("test data two point correction process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result$factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")
result$pH_corr <- dr_clean2(result, pH, calValLow = 7.01, calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, correctVar = factors)

test_that("two point correction", {
  expect_equal(result$pH_corr, test_data1$pH_Corr)
})
