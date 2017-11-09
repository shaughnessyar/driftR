library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

df <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
df <- dr_factor(df, corrFactor = corrFac, dateVar = Date, timeVar = Time, format = "MDY")
df <- dr_correctOne(df, sourceVar = SpCond, cleanVar = SpCond_Corr, calVal = 1.07, calStd = 1,
                    factorVar = corrFac)
df <- dr_correctTwo(df, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01, calStdLow = 7,
                    calValHigh = 11.8, calStdHigh =  10, factorVar = corrFac)

df %>%
  mutate(dateTime = str_c(Date, Time, sep = " ", collapse = NULL)) %>%
  mutate(dateTime = as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) %>%
  ggplot() +
  geom_line(mapping = aes(x = dateTime, y = SpCond_Corr, color = "SpCond_Corr")) +
  scale_x_datetime(labels = date_format("%m-%d-%Y"), date_breaks = "1 day")

df %>%
  mutate(dateTime = str_c(Date, Time, sep = " ", collapse = NULL)) %>%
  mutate(dateTime = as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) %>%
  select(dateTime, SpCond, SpCond_Corr) %>%
  gather(key = "measure", value = "value", SpCond, SpCond_Corr) %>%
  ggplot() +
    geom_line(mapping = aes(x = dateTime, y = value, group = measure, color = measure)) +
    scale_x_datetime(labels = date_format("%m-%d-%Y"), date_breaks = "1 day")

df %>%
  mutate(dateTime = str_c(Date, Time, sep = " ", collapse = NULL)) %>%
  mutate(dateTime = as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) %>%
  select(dateTime, SpCond, SpCond_Corr) %>%
  gather(key = "measure", value = "value", SpCond, SpCond_Corr) %>%
  ggplot() +
    geom_smooth(mapping = aes(x = dateTime, y = value, group = measure, color = measure, linetype = measure)) +
    scale_x_datetime(labels = date_format("%m-%d-%Y"), date_breaks = "1 day")


df %>%
  mutate(dateTime = str_c(Date, Time, sep = " ", collapse = NULL)) %>%
  mutate(dateTime = as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) %>%
  select(dateTime, pH, pH_Corr) %>%
  gather(key = "measure", value = "value", pH, pH_Corr) %>%
  ggplot() +
    geom_smooth(mapping = aes(x = dateTime, y = value, group = measure, color = measure, linetype = measure)) +
    scale_x_datetime(labels = date_format("%m-%d-%Y"), date_breaks = "1 day")
