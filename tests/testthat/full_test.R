context("series of commands to clean data")

library(readr)
suppressPackageStartupMessages(library(dplyr))

test_data <- data("sondeClean")

test_that("importing the data", {
  result <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
  expect_equal(result$Date, test_data$Date)
})

test_that("creating correction factors", {
  factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")
  excect_equal(factors, test_data$corrFactors)
})

test_that("one point correction", {
  factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")
  result$SpCond_corr <- dr_clean1(result, SpCond, calVal = 1.07, calStd = 1, factors)
  expect_equal(result$SpCond_corr, test_data$SpCond_corr)
})

test_that("two point correction", {
  factors <- dr_correct(result, dateVar = Date, timeVar = Time, format = "MDY")
  result$pH_corr <- dr_clean2(result, pH, calValLow = 7.01, calStdLow = 7, calValHigh = 11.8, calStdHigh =  10, factors)
  expect_equal(result$pH_corr, test_data$pH_corr)
})
