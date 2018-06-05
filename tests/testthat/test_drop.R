context("test dr_drop function")

# load test data ------------------------------------------------

test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

# test errors ------------------------------------------------

test_that("input errors trigged - no parameters", {
  expect_error(dr_drop(test_data), "At least 1 observation must be removed from the data frame")
})

test_that("input errors trigged - head", {
  expect_error(dr_drop(test_data, head = -5),
               "Head value -5 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = 2.3),
               "Head value 2.3 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = 0),
               "Head value 0 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = "foo"),
               "Head value foo not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = TRUE),
               "Head value TRUE not acceptable - value should be NULL or integer >= 1")
})

test_that("input errors trigged - unquoted string", {
  expect_error(dr_drop(test_data, head = foo), "object 'foo' not found")
  expect_error(dr_drop(test_data, tail = foo), "object 'foo' not found")
})

test_that("input errors trigged - tail", {
  expect_error(dr_drop(test_data, tail = -8),
               "Tail value -8 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = 7.5),
               "Tail value 7.5 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = 0), "Tail value 0 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = "foo"),
               "Tail value foo not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = FALSE),
               "Tail value FALSE not acceptable - value should be NULL or integer >= 1")
})

test_that("input errors triggered - date format", {
  expect_warning(dr_drop(test_data, dateVar = Date, timeVar = Time, from = "2015919"),
                 "All formats failed to parse. No formats found.")
  expect_warning(dr_drop(test_data, dateVar = Date, timeVar = Time, to = "2015919"),
                 "All formats failed to parse. No formats found.")
  expect_warning(dr_drop(test_data, dateVar = Date, timeVar = Time, from = "2015919", to = "2015923"),
                 "All formats failed to parse. No formats found.")
})

test_that("input errors triggered - ambiguous arguments", {
  expect_error(dr_drop(test_data, head = 5, tail = 5, from = "9/19/2015"),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, head = 5, tail = 5, to = "9/19/2015"),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, head = 5, tail = 5, from = "9/19/2015", to = "9/22/15"),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, head = 5, from = "9/19/2015"),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, head = 5, dateVar = Date, timeVar = Time, from = "9/19/2015",
                       to = "9/22/15"),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, dateVar = Date, timeVar = Time, from = "9/19/2015",
                       to = "9/22/15", exp = Temp >= 14.7),
               "The combination of arguments supplied for dr_drop is ambiguous.")
  expect_error(dr_drop(test_data, head = 5, exp = Temp >= 14.7),
               "The combination of arguments supplied for dr_drop is ambiguous.")
})

# test results - approach branching ------------------------------------------------

test_that("approach messages", {
  expect_message(dr_drop(test_data, head = 5, tail = 5),
                 "Drop approach - completed using the head and/or tail arguments.")
  expect_message(dr_drop(test_data, dateVar = Date, timeVar = Time, from = "9/19/2015", to = "9/22/15"),
                 "Drop approach - completed using the date/time arguments.")
  expect_message(dr_drop(test_data, exp = Temp >= 14.7),
                 "Drop approach - completed using the expression.")
})

# test results - approach 1 ------------------------------------------------

## remove 5 observations from head, 5 from tail
result1 <- dr_drop(test_data, head = 5, tail = 5)
result1_n <- nrow(result1)
result1_exp <- rows-10
result1_headExp <- "12:35:51"
result1_headTime <- result1$Time[1]
result1_tailExp <- "19:25:50"
result1_tailTime <- result1$Time[nrow(result1)]

test_that("dropping observations", {
  expect_equal(result1_exp, result1_n)
})

test_that("correct observations dropped", {
  expect_equal(result1_headExp, result1_headTime)
  expect_equal(result1_tailExp, result1_tailTime)
})

## remove 8 observations from the tail

result2 <- dr_drop(test_data, tail = 8)
result2_n <- nrow(result2)
result2_exp <- rows-8
result2_headExp <- "12:10:49"
result2_headTime <- result2$Time[1]
result2_tailExp <- "18:55:52"
result2_tailTime <- result2$Time[nrow(result2)]

test_that("dropping observations", {
  expect_equal(result2_exp, result2_n)
})

test_that("correct observations dropped", {
  expect_equal(result2_headExp, result2_headTime)
  expect_equal(result2_tailExp, result2_tailTime)
})

## remove 12 observations from the head

result3 <- dr_drop(test_data, head = 12)
result3_n <- nrow(result3)
result3_exp <- rows-12
result3_headExp <- "13:10:52"
result3_headTime <- result3$Time[1]
result3_tailExp <- "20:15:49"
result3_tailTime <- result3$Time[nrow(result3)]

test_that("dropping observations", {
  expect_equal(result3_exp, result3_n)
})

test_that("correct observations dropped", {
  expect_equal(result3_headExp, result3_headTime)
  expect_equal(result3_tailExp, result3_tailTime)
})

# test results - approach 2 ------------------------------------------------

result6 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "09/19/2015")
result6_n <- nrow(result6)
result6_exp <- rows-142
result6_date <- result6$Date[1]
result6_dateExp <- "9/19/2015"
result6_time <- result6$Time[1]
result6_timeExp <- "0:00:53"

test_that("dropping observations", {
  expect_equal(result6_exp, result6_n)
})

test_that("correct observations dropped", {
  expect_equal(result6_dateExp, result6_date)
  expect_equal(result6_timeExp, result6_time)
})

result7 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "09/19/2015 12:00")
result7_n <- nrow(result7)
result7_exp <- rows-286
result7_date <- result7$Date[1]
result7_dateExp <- "9/19/2015"
result7_time <- result7$Time[1]
result7_timeExp <- "12:00:52"

test_that("dropping observations", {
  expect_equal(result7_exp, result7_n)
})

test_that("correct observations dropped", {
  expect_equal(result7_dateExp, result7_date)
  expect_equal(result7_timeExp, result7_time)
})

result8 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "09/19/2015 12:00:52")
result8_n <- nrow(result8)
result8_exp <- rows-286
result8_date <- result8$Date[1]
result8_dateExp <- "9/19/2015"
result8_time <- result8$Time[1]
result8_timeExp <- "12:00:52"

test_that("dropping observations", {
  expect_equal(result8_exp, result8_n)
})

test_that("correct observations dropped", {
  expect_equal(result8_dateExp, result8_date)
  expect_equal(result8_timeExp, result8_time)
})

result9 <- dr_drop(test_data, dateVar = Date, timeVar = Time, from = "09/23/2015")
result9_n <- nrow(result9)
result9_exp <- rows-233
result9_date <- result9$Date[1294]
result9_dateExp <- "9/22/2015"
result9_time <- result9$Time[1294]
result9_timeExp <- "23:55:53"

test_that("dropping observations", {
  expect_equal(result9_exp, result9_n)
})

test_that("correct observations dropped", {
  expect_equal(result9_dateExp, result9_date)
  expect_equal(result9_timeExp, result9_time)
})

result10 <- dr_drop(test_data, dateVar = Date, timeVar = Time, from = "09/23/2015 6:00")
result10_n <- nrow(result10)
result10_exp <- rows-161
result10_date <- result10$Date[1366]
result10_dateExp <- "9/23/2015"
result10_time <- result10$Time[1366]
result10_timeExp <- "5:55:53"

test_that("dropping observations", {
  expect_equal(result10_exp, result10_n)
})

test_that("correct observations dropped", {
  expect_equal(result10_dateExp, result10_date)
  expect_equal(result10_timeExp, result10_time)
})

result11 <- dr_drop(test_data, dateVar = Date, timeVar = Time, from = "09/23/2015 5:55:53")
result11_n <- nrow(result11)
result11_exp <- rows-162
result11_date <- result11$Date[1365]
result11_dateExp <- "9/23/2015"
result11_time <- result11$Time[1365]
result11_timeExp <- "5:50:53"

test_that("dropping observations", {
  expect_equal(result11_exp, result11_n)
})

test_that("correct observations dropped", {
  expect_equal(result11_dateExp, result11_date)
  expect_equal(result11_timeExp, result11_time)
})

result12 <- dr_drop(test_data, dateVar = Date, timeVar = Time,
                    from = "09/19/2015", to = "09/23/2015")
result12_n <- nrow(result12)
result12_exp <- rows-1152
result12_dateH <- result12$Date[1]
result12_dateExpH <- "9/18/2015"
result12_timeH <- result12$Time[1]
result12_timeExpH <- "12:10:49"
result12_dateT <- result12$Date[375]
result12_dateExpT <- "9/23/2015"
result12_timeT <- result12$Time[375]
result12_timeExpT <- "20:15:49"

test_that("dropping observations", {
  expect_equal(result12_exp, result12_n)
})

test_that("correct observations dropped", {
  expect_equal(result12_dateExpH, result12_dateH)
  expect_equal(result12_timeExpH, result12_timeH)
  expect_equal(result12_dateExpT, result12_dateT)
  expect_equal(result12_timeExpT, result12_timeT)
})

result13 <- dr_drop(test_data, dateVar = Date, timeVar = Time,
                    from = "09/19/2015 12:00", to = "09/23/2015 06:00")
result13_n <- nrow(result13)
result13_exp <- rows-1080
result13_dateH <- result13$Date[1]
result13_dateExpH <- "9/18/2015"
result13_timeH <- result13$Time[1]
result13_timeExpH <- "12:10:49"
result13_dateT <- result13$Date[447]
result13_dateExpT <- "9/23/2015"
result13_timeT <- result13$Time[447]
result13_timeExpT <- "20:15:49"

test_that("dropping observations", {
  expect_equal(result13_exp, result13_n)
})

test_that("correct observations dropped", {
  expect_equal(result13_dateExpH, result13_dateH)
  expect_equal(result13_timeExpH, result13_timeH)
  expect_equal(result13_dateExpT, result13_dateT)
  expect_equal(result13_timeExpT, result13_timeT)
})

# test results - approach 3 ------------------------------------------------

result4 <- dr_drop(test_data, exp = Temp >= 14.75)
result4_n <- nrow(result4)
result4_exp <- rows-3
result4_max <- max(result4$Temp)
result4_maxExp <- 14.74

test_that("dropping observations", {
  expect_equal(result4_exp, result4_n)
})

test_that("correct observations dropped", {
  expect_equal(result4_maxExp, result4_max)
})

result5 <- dr_drop(test_data, exp = pH < 7.11)
result5_n <- nrow(result5)
result5_exp <- rows-388
result5_min <- min(result5$pH)
result5_minExp <- 7.11

test_that("dropping observations", {
  expect_equal(result5_exp, result5_n)
})

test_that("correct observations dropped", {
  expect_equal(result5_minExp, result5_min)
})

# test results - date formats ------------------------------------------------

result14 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "9/19/2015")
result14_n <- nrow(result14)
result14_date <- result14$Date[1]
result14_time <- result14$Time[1]

resultDT_exp <- rows-142
resultDT_dateExp <- "9/19/2015"
resultDT_timeExp <- "0:00:53"

test_that("dropping observations", {
  expect_equal(resultDT_exp, result14_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result14_date)
  expect_equal(resultDT_timeExp, result14_time)
})

result15 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "9/19/15")
result15_n <- nrow(result15)
result15_date <- result15$Date[1]
result15_time <- result15$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result15_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result15_date)
  expect_equal(resultDT_timeExp, result15_time)
})

result16 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "9-19-15")
result16_n <- nrow(result16)
result16_date <- result16$Date[1]
result16_time <- result16$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result16_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result16_date)
  expect_equal(resultDT_timeExp, result16_time)
})

result19 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "2015-09-19")
result19_n <- nrow(result19)
result19_date <- result19$Date[1]
result19_time <- result19$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result19_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result19_date)
  expect_equal(resultDT_timeExp, result19_time)
})

result24 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "2015/09/19")
result24_n <- nrow(result24)
result24_date <- result24$Date[1]
result24_time <- result24$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result24_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result24_date)
  expect_equal(resultDT_timeExp, result24_time)
})

result20 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "2015-9-19")
result20_n <- nrow(result20)
result20_date <- result20$Date[1]
result20_time <- result20$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result20_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result20_date)
  expect_equal(resultDT_timeExp, result20_time)
})

result21 <- dr_drop(test_data, dateVar = Date, timeVar = Time, to = "20150919")
result21_n <- nrow(result21)
result21_date <- result21$Date[1]
result21_time <- result21$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result21_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result21_date)
  expect_equal(resultDT_timeExp, result21_time)
})

# test results - quasiquotation ------------------------------------------------

result25 <- dr_drop(test_data, dateVar = "Date", timeVar = "Time", to = "9/19/15")
result25_n <- nrow(result25)
result25_date <- result25$Date[1]
result25_time <- result25$Time[1]

test_that("dropping observations", {
  expect_equal(resultDT_exp, result25_n)
})

test_that("correct observations dropped", {
  expect_equal(resultDT_dateExp, result25_date)
  expect_equal(resultDT_timeExp, result25_time)
})
