context("test dr_replace function")

# load test data ------------------------------------------------

test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

# test errors ------------------------------------------------

# test results - approach branching ------------------------------------------------

test_that("approach messages", {
  expect_message(dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                            from = "9/19/2015", to = "9/22/15"),
                 "Replacement approach - completed using the date/time arguments.")
  expect_message(dr_replace(test_data, sourceVar = Temp, exp = Temp >= 14.7),
                 "Replacement approach - completed using the expression.")
})

# test results - approach 1 ------------------------------------------------

result1 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                      to = "09/19/2015")
result1_na <- sum(is.na(result1$Temp_na))
result1_exp <- 142

test_that("replacing observations", {
  expect_equal(result1_exp, result1_na)
})

result2 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                      to = "09/19/2015 12:00")
result2_na <- sum(is.na(result2$Temp_na))
result2_exp <- 286

test_that("replacing observations", {
  expect_equal(result2_exp, result2_na)
})

result3 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                      to = "09/19/2015 12:00:52")
result3_na <- sum(is.na(result3$Temp_na))
result3_exp <- 286

test_that("replacing observations", {
  expect_equal(result3_exp, result3_na)
})

result4 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                      from = "09/23/2015")
result4_na <- sum(is.na(result4$Temp_na))
result4_exp <- 233

test_that("replacing observations", {
  expect_equal(result4_exp, result4_na)
})

result5 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                       from = "09/23/2015 6:00")
result5_na <- sum(is.na(result5$Temp_na))
result5_exp <- 161

test_that("replacing observations", {
  expect_equal(result5_exp, result5_na)
})

result6 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                       from = "09/23/2015 5:55:53")
result6_na <- sum(is.na(result6$Temp_na))
result6_exp <- 162

test_that("replacing observations", {
  expect_equal(result6_exp, result6_na)
})

result7 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                    from = "09/19/2015", to = "09/23/2015")
result7_na <- sum(is.na(result7$Temp_na))
result7_exp <- 1152

test_that("replacing observations", {
  expect_equal(result7_exp, result7_na)
})

result8 <- dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                    from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result8_na <- sum(is.na(result8$Temp_na))
result8_exp <- 1080

test_that("replacing observations", {
  expect_equal(result8_exp, result8_na)
})

# test results - approach 3 ------------------------------------------------

result9 <- dr_replace(test_data, sourceVar = Temp, exp = Temp >= 14.75)
result9_na <- sum(is.na(result9$Temp_na))
result9_exp <- 3

test_that("replacing observations", {
  expect_equal(result9_exp, result9_na)
})

result10 <- dr_replace(test_data, sourceVar = pH, exp = pH < 7.11)
result10_na <- sum(is.na(result10$pH_na))
result10_exp <- 388

test_that("replacing observations", {
  expect_equal(result10_exp, result10_na)
})
