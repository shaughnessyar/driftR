context("test factor creation process")

# test data ------------------------------------------------

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

# test inputs ------------------------------------------------

test_that("quoted variables do not cause errors", {
  expect_error(dr_factor(result, corrFactor = "factors", dateVar = Date, timeVar = Time, format = "MDY"), NA)
  expect_error(dr_factor(result, corrFactor = factors, dateVar = "Date", timeVar = Time, format = "MDY"), NA)
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = "Time", format = "MDY"), NA)
  expect_error(dr_factor(result, corrFactor = "factors", dateVar = "Date", timeVar = "Time", format = "MDY"), NA)
})

# test errors ------------------------------------------------

test_that("input errors trigged - missing parameters", {
  expect_error(dr_factor(result, dateVar = foo, timeVar = Time, format = "MDY"),
               "A new variable name must be specified for corrFactor")
  expect_error(dr_factor(result, corrFactor = factors, timeVar = bar, format = "MDY"),
               "An existing variable with date data must be specified for dateVar")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, format = "MDY"),
               "An existing variable with time data must be specified for timeVar")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time),
               "A format - either MDY or YMD - must be specified")
})

test_that("input errors trigged - variables invalid", {
  expect_error(dr_factor(result, corrFactor = factors, dateVar = foo, timeVar = Time, format = "MDY"),
               "Variable foo, given for dateVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = bar, format = "MDY"),
               "Variable bar, given for timeVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = Date, dateVar = Date, timeVar = Time, format = "MDY"),
               "A variable named Date, given for corrFactor, already exists in the given data frame")
})

test_that("input errors trigged - format invalid", {
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "foo"),
               "The date-time format foo is invalid - format should be MDY or YMD")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = foo),
               "It appears that the format parameter is not quoted")
})

# test results ------------------------------------------------

result <- dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "MDY")

test_that("creating correction factors", {
  expect_equal(result$factors, test_data1$corrFactors)
})
