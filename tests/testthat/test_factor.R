context("test factor creation process")

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

test_that("input errors trigged", {
  expect_error(dr_factor(result, corrFactor = factors, dateVar = foo, timeVar = Time, format = "MDY"),
               "Variable foo, given for dateVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = bar, format = "MDY"),
               "Variable bar, given for timeVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = Date, dateVar = Date, timeVar = Time, format = "MDY"),
               "A variable named Date, given for corrFactor, already exists in the given data frame")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "foo"),
               "Invalid date-time format - use either MDY or YMD")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = foo),
               "object 'foo' not found")
})


result <- dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "MDY")

test_that("creating correction factors", {
  expect_equal(result$factors, test_data1$corrFactors)
})
