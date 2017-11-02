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

test_that("input errors trigged - no parameters", {
  expect_error(dr_drop(test_data), "At least 1 observation must be removed from the data frame")
  expect_error(dr_drop(test_data, head = NULL, tail = NULL), "At least 1 observation must be removed from the data frame")
})

# test results ------------------------------------------------
## remove 5 observations from head, 5 from tail

test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

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
