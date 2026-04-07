#Clean the Gainesville city varible B16002 2024

#Read in the data 
b16002gnv2024 <- read.csv("~/Downloads/ACSDT5Y2024.B16002_2026-04-07T134635/ACSDT5Y2024.B16002-Data.csv", header = TRUE)

#Make first row column names
colnames(b16002gnv2024) <- as.character(b16002gnv2024[1, ])

#Remove first row
b16002gnv2024 <- b16002gnv2024[-1, ]

#Restructure column names
#library(dplyr)
colnames(b16002gnv2024) <- gsub("!!", "_", colnames(b16002gnv2024))
colnames(b16002gnv2024) <- gsub(" ", "_", colnames(b16002gnv2024))
colnames(b16002gnv2024) <- gsub(":", "", colnames(b16002gnv2024))
colnames(b16002gnv2024) <- gsub(",", "", colnames(b16002gnv2024))

#Filter out margin of error
b16002gnv2024 <- b16002gnv2024 |>
  select(-contains("Margin"))

write.csv(b16002gnv2024, "B16002_2024City.csv")
