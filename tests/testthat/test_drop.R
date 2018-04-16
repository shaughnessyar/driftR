context("test dr_drop function")

# test errors ------------------------------------------------

test_that("input errors trigged - head", {
  expect_error(dr_drop(test_data, head = -5), "Head value -5 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = 2.3), "Head value 2.3 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = 0), "Head value 0 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = "foo"), "Head value foo not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, head = TRUE), "Head value TRUE not acceptable - value should be NULL or integer >= 1")
})

test_that("input errors trigged - unquoted string", {
  expect_error(dr_drop(test_data, head = foo), "object 'foo' not found")
  expect_error(dr_drop(test_data, tail = foo), "object 'foo' not found")
})

test_that("input errors trigged - tail", {
  expect_error(dr_drop(test_data, tail = -8), "Tail value -8 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = 7.5), "Tail value 7.5 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = 0), "Tail value 0 not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = "foo"), "Tail value foo not acceptable - value should be NULL or integer >= 1")
  expect_error(dr_drop(test_data, tail = FALSE), "Tail value FALSE not acceptable - value should be NULL or integer >= 1")
})

# test_that("input errors trigged - no parameters", {
#  expect_error(dr_drop(test_data), "At least 1 observation must be removed from the data frame")
#  expect_error(dr_drop(test_data, head = NULL, tail = NULL), "At least 1 observation must be removed from the data frame")
# })

# test results - approach 1 ------------------------------------------------

## load test data
test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

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
