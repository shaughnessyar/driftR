## ---- eval=FALSE---------------------------------------------------------
#  library(readr)
#  library(dplyr)
#  library(ggplot2)
#  library(driftR)

## ---- eval=FALSE---------------------------------------------------------
#  df <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  corrFactors <- dr_correct(df, Date, Time, format = "MDY")

## ---- eval=FALSE---------------------------------------------------------
#  df$SpCond_Corr <- dr_clean1(df, SpCond, 1.07, 1, corrFactors)
#  df$pH_Corr <- dr_clean2(df, pH, 7.01, 7, 11.8, 10, corrFactors)

## ---- eval=FALSE---------------------------------------------------------
#  df <- dr_drop(df, 6, 6)

## ---- eval=FALSE---------------------------------------------------------
#  df <- select(df, Date, Time, SpCond, SpCond_Corr, pH, pH_Corr, pHmV, pHmV_Corr,
#               Chloride, Chloride_Corr, AmmoniumN, AmmoniumN_Corr, NitrateN, NitrateN_Corr,
#               Turbidity, Turbidity_Corr, DO, DO_Corr)

## ---- eval=FALSE---------------------------------------------------------
#  df <- select(df, -NitrateN))

## ---- eval=FALSE---------------------------------------------------------
#  df <- select(df, -c(variable1, variable2)))

## ---- eval=FALSE---------------------------------------------------------
#  write_csv(df, path = "waterData.csv", na = "NA")

