context("test data import process")

# test errors ------------------------------------------------

test_that("input errors trigged - file does not exist", {
  expect_error(dr_readSonde("foo.csv"),
               "File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
  expect_error(dr_readSonde(foo.csv),
               "object 'foo.csv' not found")
})

test_that("input errors trigged - defineVar parameter invalid", {
  expect_error(dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = "foo"),
               "defineVar value foo not acceptable - value should TRUE or FALSE")
  expect_error(dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = -5),
               "defineVar value -5 not acceptable - value should TRUE or FALSE")
  expect_error(dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = foo),
               "object 'foo' not found")
})

# test data ------------------------------------------------

## check default import

test_data2 <- read.csv(system.file("extdata", "sondeClean2.csv", package = "driftR"), stringsAsFactors = FALSE)
result1 <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

test_that("importing the data", {
  expect_equal(result1, test_data2)
})

## check to ensure extra row is present if defineVar = FALSE

result2 <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = FALSE)
result2_n <- nrow(result2)
result2_exp <- 1528

test_that("importing the data", {
  expect_equal(result2_n, result2_exp)
})
