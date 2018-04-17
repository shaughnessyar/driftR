context("test dr_replace function")

# load test data ------------------------------------------------

test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

# test errors ------------------------------------------------

test_that("input errors triggered - ambiguous arguments", {
  expect_error(dr_replace(test_data, sourceVar = Temp, dateVar = Date, exp = Temp >= 14.7),
               "The combination of arguments supplied for dr_replace is ambiguous.")
  expect_error(dr_replace(test_data, sourceVar = Temp, timeVar = Time, exp = Temp >= 14.7),
               "The combination of arguments supplied for dr_replace is ambiguous.")
  expect_error(dr_replace(test_data, sourceVar = Temp, dateVar = Date, timeVar = Time,
                          from = "09/23/2015 6:00", exp = Temp >= 14.7),
               "The combination of arguments supplied for dr_replace is ambiguous.")
})

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

result12 <- dr_replace(test_data, sourceVar = Temp, cleanVar = newTemp, dateVar = Date, timeVar = Time,
                      from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result12_na <- sum(is.na(result12$newTemp))
result12_exp <- 1080

test_that("replacing observations", {
  expect_equal(result12_exp, result12_na)
})

result13 <- dr_replace(test_data, sourceVar = Temp, overwrite = TRUE, dateVar = Date, timeVar = Time,
                       from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result13_na <- sum(is.na(result13$Temp))
result13_exp <- 1080

test_that("replacing observations", {
  expect_equal(result13_exp, result13_na)
})

# test results - approach 2 ------------------------------------------------

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

result15 <- dr_replace(test_data, sourceVar = pH, cleanVar = ph_new, exp = pH < 7.11)
result15_na <- sum(is.na(result15$ph_new))
result15_exp <- 388

test_that("replacing observations", {
  expect_equal(result15_exp, result15_na)
})

result16 <- dr_replace(test_data, sourceVar = pH, overwrite = TRUE, exp = pH < 7.11)
result16_na <- sum(is.na(result16$pH))
result16_exp <- 388

test_that("replacing observations", {
  expect_equal(result16_exp, result16_na)
})

# test results - quasiquotation ------------------------------------------------

result11 <- dr_replace(test_data, sourceVar = "Temp", cleanVar = "newTemp", dateVar = "Date",
                       timeVar = "Time", from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result11_na <- sum(is.na(result11$newTemp))
result11_exp <- 1080

test_that("replacing observations", {
  expect_equal(result11_exp, result11_na)
})

result14 <- dr_replace(test_data, sourceVar = "Temp", overwrite = TRUE, dateVar = "Date",
                       timeVar = "Time", from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result14_na <- sum(is.na(result14$Temp))
result14_exp <- 1080

test_that("replacing observations", {
  expect_equal(result14_exp, result14_na)
})
