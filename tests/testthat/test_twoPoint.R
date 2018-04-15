context("test two-point correction process")

# test data ------------------------------------------------

test_data1 <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
result <- dr_factor(result, corrFactor = factors, dateVar = Date, timeVar = Time, keepDateTime = TRUE)

# test inputs ------------------------------------------------

## quoting both the source var and the factor var caused errors in an earlier iteration of this function

test_that("quoted variables do not cause errors", {
  expect_error(dr_correctTwo(result, sourceVar = "pH", cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors), NA)
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = "pH_Corr", calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors), NA)
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = "factors"), NA)
})

# test errors ------------------------------------------------

test_that("input errors trigged - missing parameters", {
  expect_error(dr_correctTwo(result, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "A existing variable name with data to be corrected must be specified for sourceVar")
  expect_error(dr_correctTwo(result, sourceVar = ph, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "A new variable name must be specified for clearnVar")
  expect_error(dr_correctTwo(result, sourceVar = ph, cleanVar = pH_Corr,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "A numeric value must be specified for calValLow")
  expect_error(dr_correctTwo(result, sourceVar = ph, cleanVar = pH_Corr, calValLow = 7.01,
                             calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "A numeric value must be specified for calStdLow")
  expect_error(dr_correctTwo(result, sourceVar = ph, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calStdHigh =  10, factorVar = factors),
               "A numeric value must be specified for calValHigh")
  expect_error(dr_correctTwo(result, sourceVar = ph, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, factorVar = factors),
               "A numeric value must be specified for calStdHigh")
  expect_error(dr_correctTwo(result, sourceVar = ph, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10),
               "An existing variable name with the correction factor data must be specified for factorVar")
})

test_that("input errors trigged - variables invalid", {
  expect_error(dr_correctTwo(result, sourceVar = foo, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "Variable foo, given for sourceVar, cannot be found in the given data frame")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = bar),
               "Variable bar, given for factorVar, cannot be found in the given data frame")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "A variable named pH, given for cleanVar, already exists in the given data frame")
})

test_that("input errors trigged - input values inalid", {
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = "foo",
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "calValLow value foo not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = TRUE,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "calValLow value TRUE not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = foo,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "object 'foo' not found")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = "foo", calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "calStdLow value foo not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = FALSE, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "calStdLow value FALSE not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = foo, calValHigh = 11.8, calStdHigh =  10, factorVar = factors),
               "object 'foo' not found")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = "foo", calStdHigh =  10, factorVar = factors),
               "calValHigh value foo not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = TRUE, calStdHigh =  10, factorVar = factors),
               "calValHigh value TRUE not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = foo, calStdHigh =  10, factorVar = factors),
               "object 'foo' not found")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  "foo", factorVar = factors),
               "calStdHigh value foo not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  FALSE, factorVar = factors),
               "calStdHigh value FALSE not acceptable - value should numeric")
  expect_error(dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                             calStdLow = 7, calValHigh = 11.8, calStdHigh =  foo, factorVar = factors),
               "object 'foo' not found")
})

# test results ------------------------------------------------

result <- dr_correctTwo(result, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01,
                        calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factorVar = factors)

test_that("two point correction", {
  expect_equal(result$pH_Corr, test_data1$pH_Corr)
})
