context("test factor creation process")

# test data ------------------------------------------------

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

# test inputs ------------------------------------------------

test_that("quoted variables do not cause errors", {
  expect_error(dr_factor(result, corrFactor = "factors", dateVar = Date, timeVar = Time, keepDateTime = TRUE), NA)
  expect_error(dr_factor(result, corrFactor = factors, dateVar = "Date", timeVar = Time, keepDateTime = TRUE), NA)
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = "Time", keepDateTime = TRUE), NA)
  expect_error(dr_factor(result, corrFactor = "factors", dateVar = "Date", timeVar = "Time", keepDateTime = TRUE), NA)
})

## the tests above did not catch that the function was failing without an error - it would create
## the new vector but it would be all NAs

result4 <- dr_factor(result, corrFactor = factors, dateVar = "Date", timeVar = "Time", keepDateTime = FALSE)

test_that("quoted variables do not cause errors", {
  expect_false(is.na(result4$factors[1]))
  expect_false(is.na(result4$factors[2]))
  expect_false(is.na(result4$factors[10]))
  expect_false(is.na(result4$factors[20]))
  expect_false(is.na(result4$factors[1527]))
})

# test errors ------------------------------------------------

test_that("input errors trigged - missing parameters", {
  expect_error(dr_factor(result, dateVar = foo, timeVar = Time, keepDateTime = TRUE),
               "A new variable name must be specified for corrFactor")
  expect_error(dr_factor(result, corrFactor = factors, timeVar = bar, keepDateTime = TRUE),
               "An existing variable with date data must be specified for dateVar")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, keepDateTime = TRUE),
               "An existing variable with time data must be specified for timeVar")
})

test_that("input errors trigged - variables invalid", {
  expect_error(dr_factor(result, corrFactor = factors, dateVar = foo, timeVar = Time, keepDateTime = TRUE),
               "Variable foo, given for dateVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = bar, keepDateTime = TRUE),
               "Variable bar, given for timeVar, cannot be found in the given data frame")
  expect_error(dr_factor(result, corrFactor = Date, dateVar = Date, timeVar = Time, keepDateTime = TRUE),
               "A variable named Date, given for corrFactor, already exists in the given data frame")
})

test_that("input errors trigged - format is deprecated", {
  expect_warning(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "MDY"),
                 "Argument format is deprecated; dates and times are now automatically parsed as of v1.1.")
  expect_warning(dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, format = "YMD"),
                 "Argument format is deprecated; dates and times are now automatically parsed as of v1.1.")
})

# test results ------------------------------------------------

result <- dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, keepDateTime = TRUE)

test_that("creating correction factors", {
  expect_equal(result$factors, test_data1$corrFactors)
})

testData2 <- data.frame(
  Date = c("2015-9-18", "2015-9-18", "2015-9-18", "2015-9-18", "2015-9-18", "2015-9-18"),
  Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
  Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
  SpCond = c(0.754, 0.750, 0.750, 0.749, 0.749, 0.749),
  stringsAsFactors = FALSE
)

testData3 <- data.frame(
  Date = c("9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015", "9/18/2015"),
  Time = c("12:10:49", "12:15:50", "12:20:51", "12:25:51", "12:30:51", "12:35:51"),
  Temp = c(14.76, 14.64, 14.57, 14.51, 14.50, 14.63),
  SpCond = c(0.754, 0.750, 0.750, 0.749, 0.749, 0.749),
  stringsAsFactors = FALSE
)

result2 <- dr_factor(testData2, corrFactor = factors, dateVar = Date, timeVar = Time, keepDateTime = TRUE)
result3 <- dr_factor(testData3, corrFactor = factors, dateVar = Date, timeVar = Time, keepDateTime = TRUE)

test_that("creating correction factors", {
  expect_equal(result2$factors, result3$factors)
})
