library(dplyr)
library(ggplot2)

df <- dr_readSonde(system.file("extdata", "rawData.csv", package = "driftR"), defineVar = TRUE)
df <- dr_factor(df, corrFactor = corrFac, dateVar = Date, timeVar = Time, format = "MDY", keepDateTime = TRUE)
df <- dr_correctTwo(df, sourceVar = Chloride, cleanVar = Chloride_Corr,
                    calValLow = 11.6, calStdLow = 10, calValHigh = 1411,
                    calStdHigh =  1000, factorVar = corrFac)

df %>%
  dplyr::mutate(dateTime = base::as.POSIXct(dateTime, format = "%m/%d/%Y %H:%M:%S")) -> df

ggplot(data = df) +
  geom_smooth(mapping = aes(x = dateTime, y = Chloride, color = "Chloride")) +
  geom_smooth(mapping = aes(x = dateTime, y = Chloride_Corr, color = "Chloride_Corr")) +
  labs(
    title = "Comparison of Corrected and Uncorrected Chloride Values",
    subtitle = "Equilibration Not Accounted For",
    x = "Date/Time",
    y = "Chloride Value",
    caption = "Created using the example data included in the driftR package;\nGeneralized additive model used for smoothing",
    colour = "Series"
  ) +
  scale_colour_discrete(labels=c("Uncorrected Chloride", "Corrected Chloride"))

ggsave("man/figures/chloride_noDrop.png", width = 200, height = 150, units = "mm", dpi = 100)

df <- dr_drop(df, head=6, tail=6)

ggplot(data = df) +
  geom_smooth(mapping = aes(x = dateTime, y = Chloride, color = "Chloride")) +
  geom_smooth(mapping = aes(x = dateTime, y = Chloride_Corr, color = "Chloride_Corr")) +
  labs(
    title = "Comparison of Corrected and Uncorrected Chloride Values",
    subtitle = "Equilibration Accounted For Using dr_drop()",
    x = "Date/Time",
    y = "Chloride Value",
    caption = "Created using the example data included in the driftR package;\nGeneralized additive model used for smoothing",
    colour = "Series"
  ) +
  scale_colour_discrete(labels=c("Uncorrected Chloride", "Corrected Chloride"))

ggsave("man/figures/chloride_Drop.png", width = 200, height = 150, units = "mm", dpi = 100)



