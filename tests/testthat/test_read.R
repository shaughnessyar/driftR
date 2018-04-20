context("test data read process")

# test errors ------------------------------------------------

test_that("input errors trigged - file not defined", {
  expect_error(dr_read(instrument = "Sonde", defineVar = TRUE),
               "No file path supplied. Please provide a file path for your data.")
})

test_that("input errors trigged - file does not exist", {
  expect_error(dr_read("foo.csv", instrument = "Sonde", defineVar = TRUE),
               "File cannot be found. Check file name spelling and ensure it is saved in the working directory.")
  expect_error(dr_read(foo.csv, instrument = "Sonde", defineVar = TRUE),
               "object 'foo.csv' not found")
})

test_that("input errors trigged - instrument not defined", {
  expect_error(dr_read(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE),
               "No argument for instrument supplied - value should be one of Sonde, EXO, or HOBO")
})

test_that("input errors trigged - defineVar parameter invalid", {
  expect_error(dr_read(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = "foo"),
               "defineVar value foo not acceptable - value should TRUE or FALSE")
  expect_error(dr_read(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = -5),
               "defineVar value -5 not acceptable - value should TRUE or FALSE")
  expect_error(dr_read(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = foo),
               "object 'foo' not found")
})

# test Sonde import ------------------------------------------------

## check default import

sondeClean <- read.csv(system.file("extdata", "sondeClean2.csv", package = "driftR"), stringsAsFactors = FALSE)
sondeResult1 <- dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = "Sonde",
                        defineVar = TRUE, cleanVar = FALSE)

test_that("importing the data", {
  expect_equal(sondeResult1, sondeClean)
})

sondeResult5 <- dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = "Sonde",
                        defineVar = TRUE, cleanVar = TRUE)
sondeResult5_names <- colnames(sondeResult5)
sondeResult1_names <- colnames(sondeResult1)

test_that("importing the data", {
  expect_false(isTRUE(all.equal(sondeResult1_names, sondeResult5_names)))
})

## check to ensure extra row is present if defineVar = FALSE

sondeResult2 <- dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = "Sonde",
                        defineVar = FALSE, cleanVar = TRUE)
sondeResult2_n <- nrow(sondeResult2)
sondeResult2_exp <- 1528

test_that("importing the data", {
  expect_equal(sondeResult2_n, sondeResult2_exp)
})


## check quasiquotation for instrument

sondeResult3 <- dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = Sonde,
                        defineVar = TRUE, cleanVar = TRUE)
sondeResult3_n <- nrow(sondeResult3)
sondeResult3_exp <- 1527

test_that("importing the data", {
  expect_equal(sondeResult3_n, sondeResult3_exp)
})

## check alternate names for instrument

sondeResult4 <- dr_read(system.file("extdata", "rawData.csv", package = "driftR"), instrument = "sonde",
                        defineVar = TRUE, cleanVar = TRUE)
sondeResult4_n <- nrow(sondeResult4)
sondeResult4_exp <- 1527

test_that("importing the data", {
  expect_equal(sondeResult4_n, sondeResult4_exp)
})

# test Exo import ------------------------------------------------

## check default import

exoResult1 <- dr_read(system.file("extdata", "exoRaw.xlsx", package = "driftR"), instrument = "EXO",
                        defineVar = TRUE, cleanVar = TRUE)
exoResult1_n <- nrow(exoResult1)
exoResult1_exp <- 1999

test_that("importing the data", {
  expect_equal(exoResult1_n, exoResult1_exp)
})

## check to ensure extra rows are present if defineVar = FALSE

exoResult2 <- dr_read(system.file("extdata", "exoRaw.xlsx", package = "driftR"), instrument = "EXO",
                        defineVar = FALSE, cleanVar = TRUE)
exoResult2_n <- nrow(exoResult2)
exoResult2_exp <- 2023

test_that("importing the data", {
  expect_equal(exoResult2_n, exoResult2_exp)
})

## check quasiquotation for instrument

exoResult3 <- dr_read(system.file("extdata", "exoRaw.xlsx", package = "driftR"), instrument = EXO,
                      defineVar = TRUE, cleanVar = TRUE)
exoResult3_n <- nrow(exoResult3)
exoResult3_exp <- 1999

test_that("importing the data", {
  expect_equal(exoResult3_n, exoResult3_exp)
})

## check alternate names for instrument

exoResult4 <- dr_read(system.file("extdata", "exoRaw.xlsx", package = "driftR"), instrument = "Exo",
                      defineVar = TRUE, cleanVar = TRUE)
exoResult4_n <- nrow(exoResult4)
exoResult4_exp <- 1999

test_that("importing the data", {
  expect_equal(exoResult4_n, exoResult4_exp)
})

exoResult5 <- dr_read(system.file("extdata", "exoRaw.xlsx", package = "driftR"), instrument = "exo",
                      defineVar = TRUE, cleanVar = TRUE)
exoResult5_n <- nrow(exoResult5)
exoResult5_exp <- 1999

test_that("importing the data", {
  expect_equal(exoResult5_n, exoResult5_exp)
})

# test Exo import ------------------------------------------------

## check default import

hoboResult1 <- dr_read(system.file("extdata", "hoboRaw.txt", package = "driftR"), instrument = "HOBO",
                      defineVar = TRUE, cleanVar = TRUE)
hoboResult1_n <- nrow(hoboResult1)
hoboResult1_exp <- 6023

test_that("importing the data", {
  expect_equal(hoboResult1_n, hoboResult1_exp)
})

## check to ensure extra rows are present if defineVar = FALSE

hoboResult2 <- dr_read(system.file("extdata", "hoboRaw.txt", package = "driftR"), instrument = "HOBO",
                      defineVar = FALSE, cleanVar = TRUE)
hoboResult2_n <- nrow(hoboResult2)
hoboResult2_exp <- 6023

test_that("importing the data", {
  expect_equal(hoboResult2_n, hoboResult2_exp)
})

## check quasiquotation for instrument

hoboResult3 <- dr_read(system.file("extdata", "hoboRaw.txt", package = "driftR"), instrument = HOBO,
                      defineVar = TRUE, cleanVar = TRUE)
hoboResult3_n <- nrow(hoboResult3)
hoboResult3_exp <- 6023

test_that("importing the data", {
  expect_equal(hoboResult3_n, hoboResult3_exp)
})

## check alternate names for instrument

hoboResult4 <- dr_read(system.file("extdata", "hoboRaw.txt", package = "driftR"), instrument = "Hobo",
                      defineVar = TRUE, cleanVar = TRUE)
hoboResult4_n <- nrow(hoboResult4)
hoboResult4_exp <- 6023

test_that("importing the data", {
  expect_equal(hoboResult4_n, hoboResult4_exp)
})

hoboResult5 <- dr_read(system.file("extdata", "hoboRaw.txt", package = "driftR"), instrument = "hobo",
                      defineVar = TRUE, cleanVar = TRUE)
hoboResult5_n <- nrow(hoboResult5)
hoboResult5_exp <- 6023

test_that("importing the data", {
  expect_equal(hoboResult5_n, hoboResult5_exp)
})
