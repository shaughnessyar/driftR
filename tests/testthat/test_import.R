context("test data import process")

test_data2 <- read.csv(system.file("extdata", "sondeClean2.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

test_that("importing the data", {
  expect_equal(result, test_data2)
})


