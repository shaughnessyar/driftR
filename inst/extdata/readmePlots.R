library(dplyr)
library(ggplot2)

df <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
df <- dr_factor(df, corrFactor = corrFac, dateVar = Date, timeVar = Time, format = "MDY")
df <- dr_correctTwo(df, sourceVar = pH, cleanVar = pH_Corr, calValLow = 7.01, calStdLow = 7,
                    calValHigh = 11.8, calStdHigh =  10, factorVar = corrFac)

df %>%
  dplyr::mutate(dateTime = stringr::str_c(Date, Time, sep = " ", collapse = NULL)) %>%
  dplyr::mutate(dateTime = base::as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) -> df

ggplot(data = df) +
  geom_smooth(mapping = aes(x = dateTime, y = pH, color = "pH")) +
  geom_smooth(mapping = aes(x = dateTime, y = pH_Corr, color = "pH_Corr")) +
  labs(
    title = "Comparison of Corrected and Un-Corrected pH Values",
    subtitle = "Equilibration Not Accounted For",
    x = "Date/Time",
    y = "pH Value",
    caption = "Created using the example data included in the driftR package;\nGeneralized additive model used for smoothing",
    colour = "Series"
  ) +
  scale_colour_discrete(labels=c("Raw pH", "Corrected pH"))

ggsave("man/figures/pH_noDrop.png", width = 200, height = 150, units = "mm", dpi = 300)

df <- dr_drop(df, head=35, tail=10)

ggplot(data = df) +
  geom_smooth(mapping = aes(x = dateTime, y = pH, color = "pH")) +
  geom_smooth(mapping = aes(x = dateTime, y = pH_Corr, color = "pH_Corr")) +
  labs(
    title = "Comparison of Corrected and Un-Corrected pH Values",
    subtitle = "Equilibration Accounted For Using dr_drop()",
    x = "Date/Time",
    y = "pH Value",
    caption = "Created using the example data included in the driftR package;\nGeneralized additive model used for smoothing",
    colour = "Series"
  ) +
  scale_colour_discrete(labels=c("Raw pH", "Corrected pH"))

ggsave("man/figures/pH_Drop.png", width = 200, height = 150, units = "mm", dpi = 300)

