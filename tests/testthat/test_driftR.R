context("series of commands to clean data")

library(readr)
suppressPackageStartupMessages(library(dplyr))

test_data <- data("sondeClean")

test_that("importing the data", {
  result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
  expect_equal(result$Date, test_data$Date)
})
