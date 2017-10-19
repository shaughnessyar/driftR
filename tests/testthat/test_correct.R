context("test data correction process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")

test_that("creating correction factors", {
  expect_equal(factors, test_data1$corrFactors)
})
