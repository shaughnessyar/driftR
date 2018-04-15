context("test one-point correction process")

# test data ------------------------------------------------

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result <- dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, keepDateTime = TRUE)

# test inputs ------------------------------------------------

## quoting both the source var and the factor var caused errors in an earlier iteration of this function

test_that("quoted variables do not cause errors", {
  expect_error(dr_correctOne(result, sourceVar = "SpCond", cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1, factorVar = factors), NA)
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = "SpCond_Corr",
                             calVal = 1.07, calStd = 1, factorVar = factors), NA)
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1, factorVar = "factors"), NA)
})

# test errors ------------------------------------------------

test_that("input errors trigged - missing parameters", {
  expect_error(dr_correctOne(result, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1, factorVar = factors),
               "A existing variable name with data to be corrected must be specified for sourceVar")
  expect_error(dr_correctOne(result, sourceVar = SpCond,
                             calVal = 1.07, calStd = 1, factorVar = factors),
               "A new variable name must be specified for clearnVar")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calStd = 1, factorVar = factors),
               "A numeric value must be specified for calVal")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, factorVar = factors),
               "A numeric value must be specified for calStd")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1),
               "An existing variable name with the correction factor data must be specified for factorVar")
})

test_that("input errors trigged - variables invalid", {
  expect_error(dr_correctOne(result, sourceVar = foo, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1, factorVar = factors),
               "Variable foo, given for sourceVar, cannot be found in the given data frame")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = 1, factorVar = bar),
               "Variable bar, given for factorVar, cannot be found in the given data frame")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond,
                             calVal = 1.07, calStd = 1, factorVar = factors),
               "A variable named SpCond, given for cleanVar, already exists in the given data frame")
})

test_that("input errors trigged - input values invalid", {
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = "foo", calStd = 1, factorVar = factors),
               "calVal value foo not acceptable - value should numeric")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = TRUE, calStd = 1, factorVar = factors),
               "calVal value TRUE not acceptable - value should numeric")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = foo, calStd = 1, factorVar = factors),
               "object 'foo' not found")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = "foo", factorVar = factors),
               "calStd value foo not acceptable - value should numeric")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = FALSE, factorVar = factors),
               "calStd value FALSE not acceptable - value should numeric")
  expect_error(dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                             calVal = 1.07, calStd = foo, factorVar = factors),
               "object 'foo' not found")
})

# test results ------------------------------------------------

result <- dr_correctOne(result, sourceVar = SpCond, cleanVar = SpCond_Corr,
                        calVal = 1.07, calStd = 1, factorVar = factors)

test_that("one point correction", {
  expect_equal(result$SpCond_Corr, test_data1$SpCond_Corr)
})
