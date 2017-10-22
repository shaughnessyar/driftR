context("test dr_drop function")

test_data <- read.csv(system.file("extdata", "sondeClean.csv", package = "driftR"), stringsAsFactors = FALSE)
rows <- 1527

result1 <- dr_drop(test_data, head = 5, tail = 5)
result1_val <- nrow(result1)
result1_exp <- rows-10

test_that("dropping observations", {
  expect_equal(result1_exp, result1_val)
})

result2 <- dr_drop(test_data, tail = 5)
result2_val <- nrow(result2)
result2_exp <- rows-5

test_that("dropping observations", {
  expect_equal(result2_exp, result2_val)
})

result3 <- dr_drop(test_data, head = 5)
result3_val <- nrow(result3)
result3_exp <- rows-5

test_that("dropping observations", {
  expect_equal(result3_exp, result3_val)
})
