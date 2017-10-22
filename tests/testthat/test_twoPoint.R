context("test data two point correction process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result <- dr_correct(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "MDY")
result <- dr_clean2(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01, calStdLow = 7,
                    calValHigh = 11.8, calStdHigh =  10, correctVar = factors)

test_that("two point correction", {
  expect_equal(result$pH_Corr, test_data1$pH_Corr)
})
